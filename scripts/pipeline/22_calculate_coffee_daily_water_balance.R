# ======================================================================
# COFFE-STUDY | STEP 22 | CONTINUOUS DAILY SOIL-WATER BALANCE
# Run this COMPLETE file with Source in RStudio. No weather downloads.
#
# All 1,876 existing municipal reference points are processed by default.
# 2001-01-01 to 2024-12-31; meteorology + CERES, unchanged step16 inputs.
# Daily FAO56 Penman-Monteith ET0 uses the SAME functions/policies as step17.
# ETc = Kc * ET0. Daily exponential Thornthwaite-Mather bucket withdrawal.
# Rainfed SCENARIOS, not observed soils, roots, irrigation or actual field ET.
#
# CAD (mm): 50,100,150,200 x constant Kc: 0.90,0.95,1.10 (12 scenarios).
# Central scenario: CAD100_Kc095. Irrigation = 0 in every scenario.
# Rain meets demand first. P>=ETc: linear recharge; overflow exits the bucket.
# P<ETc: S_new=S_old*exp((P-ETc)/CAD); ETr=P+S_old-S_new.
# No interception/runoff partition, groundwater input or lateral inflow.
# S is available storage ABOVE the implicit wilting threshold, not total soil water.
#
# STORAGE UNCERTAINTY IS NOT IMPUTATION:
# Start with an empty-to-full feasible storage interval [0,CAD]. Propagate
# both endpoints. On a missing forcing day, fluxes stay NA and the feasible
# storage interval becomes [0,CAD]; this is NOT a physical reservoir reset.
# Subsequent known days narrow the interval. Point estimates require endpoint
# agreement within 1e-6 mm. Bounds are feasibility bounds, NOT confidence limits.
# After a missing input, do not skip a day, set ET0 to zero, or reuse last storage.
# Fluxes may be identified before storage (e.g., rain>=demand). Check separately.
#
# Main seasonal ISNA = sum(ETr)/sum(ETc). Zero-demand daily ratios stay NA.
# Counts/longest runs below 0.8/0.5 are descriptive, not validated injury limits.
# Water balance is continuous; season/year boundaries NEVER restart storage.
# Window definitions and all response/eligibility columns from step21 survive.
# Full central daily output is saved once per municipality, not per species.
# All scenarios have seasonal and annual summaries. No new regression/figure.
#
# Optional settings (not required):
# COFFEE_STUDY_DIR: override the project directory.
# COFFEE_WB_MAX_NEW: cap NEW municipal calculations per execution; default Inf.
# COFFEE_WB_TEST_ONLY=TRUE: run internal mathematical tests only.
# COFFEE_WB_SAVE_ALL_DAILY=TRUE: also save all scenario daily bounds (large).
#
# References for the components, not validation of this implementation:
# https://www.fao.org/4/x0490e/x0490e07.htm
# https://www.fao.org/4/x0490e/x0490e0b.htm
# https://www.sciencedirect.com/science/article/pii/0022169486901241
# Exact operational equations, assumptions and gap rules are exported.
# ======================================================================

coffee_water_balance_step22 <- function() {
  version <- "22_daily_TM_rainfed_bounds_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  tolerance_mm <- 1e-6
  balance_tolerance_mm <- 1e-8
  cloud_ratio_lower <- 0.30
  cloud_ratio_upper <- 1.00
  first_day <- as.Date("2001-01-01")
  last_day <- as.Date("2024-12-31")
  dates <- seq(first_day, last_day, by = "day")
  test_only <- toupper(Sys.getenv("COFFEE_WB_TEST_ONLY", "FALSE")) == "TRUE"
  save_all_daily <- toupper(Sys.getenv("COFFEE_WB_SAVE_ALL_DAILY", "FALSE")) == "TRUE"
  limit_new <- suppressWarnings(as.numeric(Sys.getenv("COFFEE_WB_MAX_NEW", "Inf")))
  if (is.na(limit_new) || limit_new < 0) stop("Invalid COFFEE_WB_MAX_NEW.", call. = FALSE)
  out_dir <- file.path(base_dir, "04_outputs", "daily_water_balance")
  derived_dir <- file.path(base_dir, "02_data_processed", "daily_water_balance")
  cache_dir <- file.path(derived_dir, "municipality_cache")
  daily_dir <- file.path(derived_dir, "daily_CAD100_Kc095")
  all_daily_dir <- file.path(derived_dir, "daily_all_scenarios")
  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    missing <- setdiff(cols, names(x))
    assert(!length(missing), paste(label, "missing:", paste(missing, collapse=", ")))
  }
  hash <- function(path) unname(tools::md5sum(path))
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required input not found:", path))
    utils::read.csv(path, colClasses="character", stringsAsFactors=FALSE,
      check.names=FALSE, na.strings=c("NA", ""), fileEncoding="UTF-8")
  }
  number <- function(x, label, allow_na=FALSE) {
    z <- suppressWarnings(as.numeric(as.character(x)))
    assert(!any(!is.na(x) & is.na(z)) && all(is.finite(z) | is.na(z)) &&
      (allow_na || !anyNA(z)), paste("Invalid numeric column:", label))
    z
  }
  logical_col <- function(x, label) {
    assert(!anyNA(x) && all(as.character(x) %in% c("TRUE", "FALSE")),
      paste("Invalid logical column:", label))
    as.character(x) == "TRUE"
  }
  checked <- function(x, key) {
    need(x,c("check","result"),"Summary")
    z <- x$result[x$check==key]
    assert(length(z)==1L && !is.na(z),paste("Missing/duplicate summary check:",key))
    z
  }
  local_path <- function(path) {
    p <- gsub("\\", "/", path, fixed=TRUE)
    if (file.exists(p)) return(p)
    marker <- "/coffe-study/"
    ix <- regexpr(marker,p,fixed=TRUE)[1L]
    assert(ix>0L,paste("Cannot resolve recorded path:",path))
    z <- file.path(base_dir,substring(p,ix+nchar(marker)))
    assert(file.exists(z),paste("Missing recorded input:",z))
    z
  }
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    z <- do.call(rbind,x); rownames(z)<-NULL; z
  }
  write_csv <- function(x,path) {
    f <- tempfile("wb22_",tmpdir=dirname(path),fileext=".csv")
    on.exit(unlink(f),add=TRUE)
    utils::write.csv(x,f,row.names=FALSE,na="NA",fileEncoding="UTF-8")
    assert(file.copy(f,path,overwrite=TRUE),paste("Cannot save:",path))
  }
  write_rds <- function(x,path) {
    f <- tempfile("wb22_",tmpdir=dirname(path),fileext=".rds")
    on.exit(unlink(f),add=TRUE)
    saveRDS(x,f,compress="gzip")
    assert(file.copy(f,path,overwrite=TRUE),paste("Cannot save:",path))
  }
  valid_bytes <- function(path) {
    file.exists(path) && file.exists(paste0(path,".md5.txt")) &&
      identical(paste(readLines(paste0(path,".md5.txt"),warn=FALSE),collapse=""),hash(path))
  }
  longest_run <- function(x) {
    if (!length(x)) return(0L)
    if (anyNA(x)) return(NA_integer_)
    z <- rle(x)
    if (any(z$values)) max(z$lengths[z$values]) else 0L
  }
  scenarios <- expand.grid(cad_mm=c(50,100,150,200),kc=c(0.90,0.95,1.10),
    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  scenarios$scenario_id <- sprintf("CAD%03d_Kc%03d",scenarios$cad_mm,round(100*scenarios$kc))
  scenarios$central <- scenarios$cad_mm==100 & abs(scenarios$kc-0.95)<1e-12
  scenarios$irrigation_mm_day <- 0
  scenarios <- scenarios[order(!scenarios$central,scenarios$kc,scenarios$cad_mm),]
  rownames(scenarios)<-NULL
  central <- which(scenarios$central)
  scenarios$parameter_basis <- "Standardized scenario; not measured municipal soil or calibrated species Kc"

  es <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))
  solar_geometry <- function(date, latitude, z) {
    J <- as.integer(format(date, "%j"))
    phi <- latitude * pi / 180
    dr <- 1 + 0.033 * cos(2 * pi * J / 365)
    delta <- 0.409 * sin(2 * pi * J / 365 - 1.39)
    ws <- acos(pmin(1, pmax(-1, -tan(phi) * tan(delta))))
    ra <- (24 * 60 / pi) * 0.0820 * dr *
      (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
    data.frame(ra_mj_m2 = pmax(ra, 0), rso_mj_m2 = (0.75 + 2e-5 * z) * pmax(ra, 0))
  }
  pm <- function(tmin, tmax, dewpoint, wind2m, pressure, rs, ra, rso) {
    t <- (tmin + tmax) / 2
    ea <- es(dewpoint)
    sat <- (es(tmin) + es(tmax)) / 2
    vpd_raw <- sat - ea
    vpd <- vpd_raw
    vpd[!is.na(vpd_raw) & vpd_raw < 0] <- NA_real_
    slope <- 4098 * es(t) / (t + 237.3)^2
    gamma <- 0.000665 * pressure
    ratio_raw <- rs / rso
    ratio_used <- pmin(cloud_ratio_upper, pmax(cloud_ratio_lower, ratio_raw))
    emissivity <- 0.34 - 0.14 * sqrt(ea)
    thermal <- 4.903e-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2
    longwave <- thermal * emissivity * (1.35 * ratio_used - 0.35)
    longwave_unfloored <- thermal * emissivity * (1.35 * pmin(1, ratio_raw) - 0.35)
    net_sw <- 0.77 * rs
    net <- net_sw - longwave
    net_unfloored <- net_sw - longwave_unfloored
    denom <- slope + gamma * (1 + 0.34 * wind2m)
    calc_et <- function(rn) (0.408 * slope * rn + gamma * 900 / (t + 273) * wind2m * vpd) / denom
    raw <- calc_et(net)
    raw_unfloored <- calc_et(net_unfloored)
    bad_domain <- (!is.na(tmin) & !is.na(tmax) & tmin > tmax) |
      (!is.na(rso) & rso <= 0) | (!is.na(emissivity) & emissivity < 0) |
      (!is.na(wind2m) & wind2m < 0) | (!is.na(pressure) & pressure <= 0) |
      (!is.na(rs) & rs < 0)
    raw[bad_domain] <- NA_real_
    raw_unfloored[bad_domain] <- NA_real_
    data.frame(tmean_fao56_c = t, actual_vapour_pressure_est_kpa = ea,
      saturation_vapour_pressure_fao56_kpa = sat, vpd_fao56_raw_kpa = vpd_raw,
      vpd_fao56_estimated_kpa = vpd, ra_mj_m2 = ra, rso_mj_m2 = rso,
      rs_rso_raw = ratio_raw, rs_rso_used = ratio_used,
      net_longwave_estimated_mj_m2 = longwave, net_radiation_estimated_mj_m2 = net,
      et0_fao56_raw_mm = raw, et0_reference_mm = pmax(raw, 0),
      et0_no_lower_cloud_bound_mm = pmax(raw_unfloored, 0),
      flag_negative_vpd = !is.na(vpd_raw) & vpd_raw < 0,
      flag_cloud_ratio_lower_bound = !is.na(ratio_raw) & ratio_raw < cloud_ratio_lower,
      flag_cloud_ratio_upper_bound = !is.na(ratio_raw) & ratio_raw > cloud_ratio_upper,
      flag_negative_et0_raw = !is.na(raw) & raw < 0,
      flag_et0_formula_domain = bad_domain)
  }


  # A single continuous run, vectorized across the specified scenarios.
  # For valid forcing, the storage map and ETr are monotone in initial storage.
  # Therefore propagating the two feasible endpoints encloses every possible
  # state under the stated input and parameter assumptions.
  balance_engine <- function(p,et0,cad,kc,tol=tolerance_mm,initial_low=0,initial_high=cad) {
    n<-length(p); k<-length(cad)
    assert(length(et0)==n && length(kc)==k && k>0L && all(cad>0) && all(kc>0),
      "Invalid balance inputs.")
    assert(all(is.na(p)|is.finite(p)) && all(is.na(et0)|is.finite(et0)) &&
      all(p>=0,na.rm=TRUE) && all(et0>=0,na.rm=TRUE),"Negative/nonfinite water forcing.")
    slo<-rep(initial_low,length.out=k); shi<-rep(initial_high,length.out=k)
    assert(all(slo>=0 & slo<=shi & shi<=cad),"Initial storage bounds outside CAD.")
    Slo<-Shi<-Elo<-Ehi<-Xlo<-Xhi<-matrix(NA_real_,nrow=n,ncol=k)
    E<-outer(et0,kc,"*")
    known<-is.finite(p)&is.finite(et0)
    gap_seen<-rep(FALSE,n)
    max_closure<-rep(0,k); physical_failure<-rep(0L,k)
    for (tt in seq_len(n)) {
      gap_seen[tt]<-(!known[tt]) || (tt>1L && gap_seen[tt-1L])
      if (!known[tt]) {
        # Unknown END state, not an inferred physical recharge or water flux.
        slo<-rep(0,k); shi<-cad
        Slo[tt,]<-slo; Shi[tt,]<-shi
        next
      }
      delta<-p[tt]-E[tt,]
      wet<-delta>=0
      lo_new<-ifelse(wet,pmin(cad,slo+delta),slo*exp(pmin(delta,0)/cad))
      hi_new<-ifelse(wet,pmin(cad,shi+delta),shi*exp(pmin(delta,0)/cad))
      elo<-ifelse(wet,E[tt,],p[tt]+slo-lo_new)
      ehi<-ifelse(wet,E[tt,],p[tt]+shi-hi_new)
      xlo<-ifelse(wet,pmax(0,slo+delta-cad),0)
      xhi<-ifelse(wet,pmax(0,shi+delta-cad),0)
      rlo<-p[tt]-elo-xlo-(lo_new-slo)
      rhi<-p[tt]-ehi-xhi-(hi_new-shi)
      max_closure<-pmax(max_closure,abs(rlo),abs(rhi))
      bad<-lo_new < -tol | hi_new > cad+tol | lo_new>hi_new+tol |
        elo < -tol | elo>ehi+tol | ehi>E[tt,]+tol | xlo < -tol | xhi<xlo-tol
      physical_failure<-physical_failure+as.integer(bad)
      Slo[tt,]<-lo_new; Shi[tt,]<-hi_new
      Elo[tt,]<-elo; Ehi[tt,]<-ehi; Xlo[tt,]<-xlo; Xhi[tt,]<-xhi
      slo<-lo_new; shi<-hi_new
    }
    identified <- function(lo,hi) {
      z<-lo
      z[!is.finite(lo)|!is.finite(hi)|hi-lo>tol]<-NA_real_
      z
    }
    etr<-identified(Elo,Ehi); storage<-identified(Slo,Shi)
    # A missing forcing day's END storage is unknown, irrespective of tolerances.
    storage[!known,]<-NA_real_
    surplus<-identified(Xlo,Xhi)
    ratio<-etr/E; ratio[!is.finite(ratio)|E<=0]<-NA_real_
    list(etc=E,etr=etr,storage=storage,surplus=surplus,isna=ratio,
      storage_low=Slo,storage_high=Shi,etr_low=Elo,etr_high=Ehi,
      surplus_low=Xlo,surplus_high=Xhi,forcing_known=known,gap_seen=gap_seen,
      maximum_mass_residual_mm=max_closure,physical_failure_days=physical_failure)
  }
  balance_engine <- compiler::cmpfun(balance_engine)

  summarize_water <- function(b,p,periods,indices) {
    out<-vector("list",nrow(periods)); k<-nrow(scenarios)
    for (ii in seq_len(nrow(periods))) {
      q<-indices[[ii]]; n<-length(q)
      complete_forcing<-all(b$forcing_known[q])
      demand<-colSums(b$etc[q,,drop=FALSE])
      actual<-colSums(b$etr[q,,drop=FALSE])
      flow_known<-colSums(is.finite(b$etr[q,,drop=FALSE]))==n
      # Using the low endpoint within tolerance is explicitly numerical,
      # never an average of materially different possible trajectories.
      ratio<-actual/demand
      ratio[!complete_forcing|!flow_known|!is.finite(ratio)|demand<=0]<-NA_real_
      lower<-colSums(b$etr_low[q,,drop=FALSE])/demand
      upper<-colSums(b$etr_high[q,,drop=FALSE])/demand
      lower[!complete_forcing|!is.finite(lower)|demand<=0]<-NA_real_
      upper[!complete_forcing|!is.finite(upper)|demand<=0]<-NA_real_
      zero_demand<-colSums(b$etc[q,,drop=FALSE]==0,na.rm=TRUE)
      ratio_ok<-is.finite(b$isna[q,,drop=FALSE]) | b$etc[q,,drop=FALSE]==0
      ratio_ok[is.na(ratio_ok)]<-FALSE
      counts_spells<-function(threshold) {
        rr<-b$isna[q,,drop=FALSE]; ok<-colSums(ratio_ok)==n
        event<-rr<threshold
        event[b$etc[q,,drop=FALSE]==0 & !is.na(b$etc[q,,drop=FALSE])]<-FALSE
        count<-colSums(event)
        run<-vapply(seq_len(k),function(j) if(ok[j]) longest_run(event[,j]) else NA_integer_,integer(1))
        count[!ok]<-NA_real_
        list(count=count,run=run)
      }
      a<-counts_spells(0.8); z<-counts_spells(0.5)
      storage_rel<-sweep(b$storage[q,,drop=FALSE],2,scenarios$cad_mm,"/")
      relmean<-colMeans(storage_rel)
      relmin<-vapply(seq_len(k),function(j) if(anyNA(storage_rel[,j])) NA_real_ else min(storage_rel[,j]),numeric(1))
      def<-demand-actual; def[!complete_forcing|!flow_known]<-NA_real_
      status<-ifelse(!complete_forcing,"MISSING_FORCING_IN_WINDOW",
        ifelse(!flow_known,"UNRESOLVED_STORAGE_HISTORY",
          ifelse(demand<=0,"ZERO_DEMAND","COMPLETE")))
      out[[ii]]<-data.frame(year=rep(periods$year[ii],k),window_id=periods$window_id[ii],
        period_start=as.character(periods$start[ii]),period_end=as.character(periods$end[ii]),
        scenario_id=scenarios$scenario_id,cad_mm=scenarios$cad_mm,kc=scenarios$kc,
        expected_days=n,precipitation_total_mm=if(all(is.finite(p[q])))sum(p[q]) else NA_real_,
        etc_total_mm=demand,etr_total_mm=actual,isna_ratio=ratio,
        isna_lower_bound=lower,isna_upper_bound=upper,
        deficit_total_mm=def,excess_total_mm=colSums(b$surplus[q,,drop=FALSE]),
        days_isna_lt080=a$count,longest_isna_lt080_days=a$run,
        days_isna_lt050=z$count,longest_isna_lt050_days=z$run,
        mean_relative_storage=relmean,minimum_relative_storage=relmin,
        mean_relative_storage_lower_bound=colMeans(sweep(b$storage_low[q,,drop=FALSE],2,scenarios$cad_mm,"/")),
        mean_relative_storage_upper_bound=colMeans(sweep(b$storage_high[q,,drop=FALSE],2,scenarios$cad_mm,"/")),
        storage_start_lower_mm=if(q[1L]==1L)rep(0,k) else b$storage_low[q[1L]-1L,],
        storage_start_upper_mm=if(q[1L]==1L)scenarios$cad_mm else b$storage_high[q[1L]-1L,],
        storage_end_lower_mm=b$storage_low[q[n],],storage_end_upper_mm=b$storage_high[q[n],],
        maximum_storage_interval_mm=apply(b$storage_high[q,,drop=FALSE]-b$storage_low[q,,drop=FALSE],2,max),
        missing_forcing_days=sum(!b$forcing_known[q]),
        unresolved_etr_days=colSums(!is.finite(b$etr[q,,drop=FALSE]) & b$forcing_known[q]),
        unresolved_storage_days=colSums(!is.finite(b$storage[q,,drop=FALSE])),
        zero_demand_days=zero_demand,status=status,stringsAsFactors=FALSE)
    }
    bind(out)
  }

  tests<-list()
  test<-function(label,ok) tests[[length(tests)+1L]]<<-data.frame(test=label,pass=isTRUE(ok))
  dry<-balance_engine(0,10,100,1,initial_low=100,initial_high=100)
  test("Exponential withdrawal exact solution",abs(dry$storage[1,1]-100*exp(-0.1))<1e-10)
  test("Dry mass conservation",dry$maximum_mass_residual_mm<1e-10)
  wet<-balance_engine(30,5,100,1,initial_low=90,initial_high=90)
  test("Recharge and overflow",wet$storage[1,1]==100 && wet$etr[1,1]==5 && wet$surplus[1,1]==15)
  empty<-balance_engine(0,5,100,1,initial_low=0,initial_high=0)
  test("Empty storage supplies no soil-water extraction",empty$storage[1,1]==0 && empty$etr[1,1]==0)
  partial<-balance_engine(8,3,100,1,initial_low=10,initial_high=10)
  test("Partial recharge",partial$storage[1,1]==15 && partial$etr[1,1]==3)
  zero<-balance_engine(8,0,100,1,initial_low=10,initial_high=10)
  test("Zero demand ratio is undefined",is.na(zero$isna[1,1]) && zero$etr[1,1]==0 && zero$storage[1,1]==18)
  gap<-balance_engine(c(150,0,150,0),c(5,NA,5,10),100,1)
  test("Missing forcing is not zero flux",is.na(gap$etr[2,1]) && is.na(gap$storage[2,1]))
  test("Gap restores a feasible interval only",gap$storage_low[2,1]==0 && gap$storage_high[2,1]==100)
  test("Recharge identifies state after gap",gap$storage[3,1]==100 && is.finite(gap$isna[4,1]))
  test("Overflow can remain uncertain on recovery day",is.na(gap$surplus[3,1]) && gap$etr[3,1]==5)
  dates_test<-as.Date(c("2020-12-31","2021-01-01"))
  across<-balance_engine(c(0,0),c(5,5),100,1,initial_low=100,initial_high=100)
  test("No reset across year boundary",diff(dates_test)==1 && abs(across$storage[2,1]-100*exp(-0.1))<1e-10)
  test("Ratios aggregated by demand",abs(sum(c(1,9))/sum(c(2,9))-mean(c(1/2,9/9)))>0.01)
  test("Sequences cross internal boundaries",longest_run(c(TRUE,TRUE,FALSE,TRUE))==2L)
  pp<-c(0,6,0,250,0,NA,0,300,2,0); ee<-c(5,4,6,3,4,5,2,3,5,8)
  many<-balance_engine(pp,ee,scenarios$cad_mm,scenarios$kc)
  same<-vapply(seq_len(nrow(scenarios)),function(j) {
    one<-balance_engine(pp,ee,scenarios$cad_mm[j],scenarios$kc[j])
    isTRUE(all.equal(as.numeric(one$storage_low),many$storage_low[,j],tolerance=1e-12)) &&
      isTRUE(all.equal(as.numeric(one$etr_high),many$etr_high[,j],tolerance=1e-12))
  },logical(1))
  test("Vector scenarios equal separate runs",all(same))
  test("All scenarios conserve water on known days",max(many$maximum_mass_residual_mm)<1e-10 && sum(many$physical_failure_days)==0)
  enclosing<-balance_engine(c(0,2,3,200,0),c(5,5,5,5,5),100,1)
  enclosed<-vapply(seq(0,100,by=5),function(init) {
    b<-balance_engine(c(0,2,3,200,0),c(5,5,5,5,5),100,1,initial_low=init,initial_high=init)
    all(b$storage>=enclosing$storage_low-1e-10 & b$storage<=enclosing$storage_high+1e-10) &&
      all(b$etr>=enclosing$etr_low-1e-10 & b$etr<=enclosing$etr_high+1e-10)
  },logical(1))
  test("Intermediate initial states enclosed",all(enclosed))
  ea<-(es(12.3)*0.84+es(21.5)*0.63)/2
  lg<-log(ea/0.6108); td<-237.3*lg/(17.27-lg)
  sg<-solar_geometry(as.Date("2001-07-06"),50.8,100)
  f<-pm(12.3,21.5,td,2.078,100.1,22.07,sg$ra_mj_m2,sg$rso_mj_m2)
  test("FAO56 example ET0",abs(f$et0_reference_mm-3.88)<0.04)
  test("Negative VPD remains missing",is.na(pm(10,20,25,2,100,20,35,26)$et0_reference_mm))
  test("Twelve scenarios and unique central",nrow(scenarios)==12L && length(central)==1L && scenarios$scenario_id[central]=="CAD100_Kc095")
  fixture_period<-data.frame(year=2012L,window_id="TEST",start=as.Date("2012-01-01"),end=as.Date("2012-01-03"))
  fixture_b<-balance_engine(c(1000,0,0),c(2,4,0),scenarios$cad_mm,scenarios$kc)
  fixture_s<-summarize_water(fixture_b,c(1000,0,0),fixture_period,list(1:3))
  test("Seasonal ratio equals ratio of sums",all(abs(fixture_s$isna_ratio-colSums(fixture_b$etr)/colSums(fixture_b$etc))<1e-12))
  test("Zero-demand day counted separately",all(fixture_s$zero_demand_days==1L) && all(fixture_s$status=="COMPLETE"))
  fixture_gap<-balance_engine(c(1000,0,1000),c(2,NA,4),scenarios$cad_mm,scenarios$kc)
  fixture_g<-summarize_water(fixture_gap,c(1000,0,1000),fixture_period,list(1:3))
  test("Incomplete window has no full-period ratio",all(is.na(fixture_g$isna_ratio)) && all(is.na(fixture_g$isna_lower_bound)) && all(fixture_g$status=="MISSING_FORCING_IN_WINDOW"))
  rm(fixture_period,fixture_b,fixture_s,fixture_gap,fixture_g)
  test_table<-bind(tests)
  assert(all(test_table$pass),paste("Numerical test failed:",paste(test_table$test[!test_table$pass],collapse=", ")))
  if(test_only){ print(test_table,row.names=FALSE); return(invisible(test_table)) }

  for(p in c(out_dir,derived_dir,cache_dir,daily_dir,if(save_all_daily)all_daily_dir)) {
    dir.create(p,recursive=TRUE,showWarnings=FALSE); assert(dir.exists(p),paste("Cannot create:",p))
  }
  old_options<-options(scipen=999)
  on.exit(options(old_options),add=TRUE)
  lock<-file.path(out_dir,".step22_running")
  assert(dir.create(lock,showWarnings=FALSE),paste("Another step22 run may be active. Confirm it has stopped before removing:",lock))
  writeLines(paste("PID",Sys.getpid(),format(Sys.time(),usetz=TRUE)),file.path(lock,"owner.txt"))
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  prior<-list.files(out_dir,pattern="\\.(csv|txt)$",full.names=TRUE)
  if(length(prior)) {
    ar<-file.path(out_dir,"previous_runs");dir.create(ar,showWarnings=FALSE)
    dest<-tempfile(paste0("before_",format(Sys.time(),"%Y%m%dT%H%M%S"),"_"),tmpdir=ar)
    assert(dir.create(dest) && all(file.copy(prior,dest)),"Cannot archive previous step22 reports.")
  }
  write_csv(test_table,file.path(out_dir,"coffee_water_balance_numerical_tests.csv"))
  write_csv(scenarios,file.path(out_dir,"coffee_water_balance_scenarios.csv"))
  report<-list(execution_status="INCOMPLETE",script_version=version,
    simulation_start=as.character(first_day),simulation_end=as.character(last_day),
    central_scenario="CAD100_Kc095",scenario_count=12L,irrigation="ZERO_RAINFED_SCENARIOS",
    municipal_targets=0L,municipalities_processed=0L,municipalities_reused=0L,new_municipalities=0L,
    daily_steps_per_municipality=length(dates),source_files_verified=0L,
    maximum_mass_residual_mm=0,maximum_et0_benchmark_difference_mm=0,
    et0_benchmarks_verified=0L,missing_forcing_municipal_days=0L,
    seasonal_rows=0L,seasonal_rows_with_unavailable_isna=0L,central_panel_rows=0L,
    central_eligible_rows_all_water_windows_complete=0L,numerical_tests_passed=TRUE,
    weather_imputed=FALSE,weather_downloads=0L,reset_at_calendar_boundaries=FALSE,
    storage_gap_handling="FEASIBLE_ENDPOINT_PROPAGATION_NOT_FLUX_IMPUTATION",
    calibrated_soil_parameters=FALSE,observed_irrigation=FALSE,
    estimated_regressions=FALSE,causal_attribution=FALSE,upstream_inputs_modified=FALSE,
    full_R_national_execution_tested_by_author=FALSE,last_stage="Reading inputs",error_if_incomplete="None")
  progress<-list()
  save_progress<-function() {
    write_csv(data.frame(check=names(report),result=vapply(report,as.character,character(1))),
      file.path(out_dir,"coffee_water_balance_summary.csv"))
    if(length(progress))write_csv(bind(progress),file.path(out_dir,"coffee_water_balance_cache_index.csv"))
  }
  on.exit({try(save_progress(),silent=TRUE)
    try(writeLines(capture.output(utils::sessionInfo()),file.path(out_dir,"sessionInfo_step22.txt")),silent=TRUE)
  },add=TRUE)

  worker<-function() {
    paths<-c(panel=file.path(base_dir,"02_data_processed","seasonal_climate_yield_panel","coffee_yield_seasonal_climate_panel_2012_2024.rds"),
      summary21=file.path(base_dir,"04_outputs","seasonal_climate_yield_panel","coffee_seasonal_climate_summary.csv"),
      windows=file.path(base_dir,"04_outputs","seasonal_climate_yield_panel","coffee_seasonal_window_dates.csv"),
      provenance21=file.path(base_dir,"04_outputs","seasonal_climate_yield_panel","coffee_seasonal_input_provenance.csv"),
      manifest=file.path(base_dir,"04_outputs","climate_power_brazil_download","coffee_power_brazil_download_manifest.csv"),
      crosswalk=file.path(base_dir,"04_outputs","climate_power_brazil_download","coffee_power_brazil_municipality_series_index.csv"),
      targets=file.path(base_dir,"02_data_processed","climate","coffee_climate_target_registry.csv"),
      annual17=file.path(base_dir,"02_data_processed","climate","NASA_POWER","brazil_indices","coffee_power_brazil_municipality_annual_indices.rds"))
    assert(all(file.exists(paths)),paste("Required input(s) missing:\n",paste(paths[!file.exists(paths)],collapse="\n")))
    provenance<-data.frame(role=names(paths),file=unname(paths),md5=vapply(paths,hash,character(1)),stringsAsFactors=FALSE)
    write_csv(provenance,file.path(out_dir,"coffee_water_balance_input_provenance.csv"))
    s21<-read_csv(paths[["summary21"]]); origins<-read_csv(paths[["provenance21"]])
    assert(checked(s21,"execution_status")=="COMPLETED_CANDIDATE_SEASONAL_PANEL" &&
      checked(s21,"numerical_tests_passed")=="TRUE" && checked(s21,"annual_benchmarks_passed")=="TRUE",
      "Step21 did not complete its checks.")
    need(origins,c("role","md5"),"Step21 provenance")
    assert(sum(origins$role=="manifest16")==1L &&
      hash(paths[["manifest"]])==origins$md5[origins$role=="manifest16"],
      "Source manifest differs from the one used in step21. Revalidate affected stages.")
    panel<-as.data.frame(readRDS(paths[["panel"]]),stringsAsFactors=FALSE)
    need(panel,c("municipality_code","municipality","state","species","year","MET_cell_id",
      "included_step20","production_t","area_harvested_ha","time_standard"),"Step21 panel")
    panel$municipality_code<-as.character(panel$municipality_code)
    panel$year<-as.integer(number(panel$year,"PAM year"))
    included<-logical_col(panel$included_step20,"included_step20")
    key<-paste(panel$municipality_code,panel$species,panel$year,sep="|")
    assert(!anyDuplicated(key) && nrow(panel)==as.integer(checked(s21,"panel_rows")) &&
      sum(included)==as.integer(checked(s21,"inherited_eligible_rows")),"Step21 panel keys/counts disagree.")
    assert(setequal(panel$species,c("Arabica","Canephora")) && setequal(panel$year,2012:2024) &&
      all(panel$time_standard=="UTC"),"Unexpected study scope or time standard.")
    codes<-sort(unique(panel$municipality_code))
    assert(all(grepl("^[0-9]{7}$",codes)),"Invalid municipal code format.")
    win<-read_csv(paths[["windows"]])
    need(win,c("year","window_id","start","end","expected_days"),"Seasonal windows")
    win$year<-as.integer(number(win$year,"window year"));win$start<-as.Date(win$start);win$end<-as.Date(win$end)
    assert(!anyNA(win$start) && !anyNA(win$end) && !anyDuplicated(paste(win$year,win$window_id)) &&
      nrow(win)==52L && setequal(win$window_id,paste0("W",1:4)) && setequal(win$year,2012:2024),"Invalid window registry.")
    check_dates<-data.frame(year=win$year,window_id=win$window_id,
      start=as.Date(ifelse(win$window_id=="W1",paste0(win$year-2,"-09-01"),
        ifelse(win$window_id=="W2",paste0(win$year-1,"-04-01"),
          ifelse(win$window_id=="W3",paste0(win$year-1,"-09-01"),paste0(win$year,"-01-01"))))),
      end=as.Date(ifelse(win$window_id=="W1",paste0(win$year-1,"-03-31"),
        ifelse(win$window_id=="W2",paste0(win$year-1,"-08-31"),
          ifelse(win$window_id=="W3",paste0(win$year-1,"-12-31"),paste0(win$year,"-03-31"))))))
    assert(all(win$start==check_dates$start & win$end==check_dates$end) &&
      all(as.integer(win$end-win$start)+1L==number(win$expected_days,"window days")),
      "Step21 windows differ from the accepted calendar design.")
    windex<-lapply(seq_len(nrow(win)),function(j)which(dates>=win$start[j]&dates<=win$end[j]))
    assert(all(lengths(windex)==number(win$expected_days,"window days")),"A window falls outside the simulation.")
    for(w in paste0("W",1:4)) {
      z<-win[win$window_id==w,];pos<-match(panel$year,z$year)
      assert(all(as.Date(panel[[paste0("season_",w,"_start")]])==z$start[pos]) &&
        all(as.Date(panel[[paste0("season_",w,"_end")]])==z$end[pos]),"Panel/registry window-date mismatch.")
    }
    annual_periods<-data.frame(year=2001:2024,window_id="ANNUAL",
      start=as.Date(paste0(2001:2024,"-01-01")),end=as.Date(paste0(2001:2024,"-12-31")))
    aindex<-lapply(2001:2024,function(y)which(as.integer(format(dates,"%Y"))==y))
    man<-read_csv(paths[["manifest"]]);idx<-read_csv(paths[["crosswalk"]]);targets<-read_csv(paths[["targets"]])
    need(man,c("cell_id","family","daily_file","daily_md5","response_elevation_m","status"),"Manifest")
    need(idx,c("municipality_code","family","cell_id","requested_municipal_latitude","requested_municipal_longitude"),"Crosswalk")
    need(targets,c("municipality_code","municipality","state","latitude","longitude"),"Targets")
    assert(!anyDuplicated(man$cell_id) && !anyDuplicated(paste(idx$municipality_code,idx$family)) &&
      !anyDuplicated(targets$municipality_code) && setequal(codes,targets$municipality_code),"Ambiguous source crosswalk.")
    sites<-targets[match(codes,targets$municipality_code),c("municipality_code","municipality","state","latitude","longitude")]
    sites$latitude<-number(sites$latitude,"latitude");sites$longitude<-number(sites$longitude,"longitude")
    for(fam in c("MET","CERES")) {
      z<-idx[idx$family==fam,];pos<-match(codes,z$municipality_code)
      assert(!anyNA(pos),paste("Missing",fam,"municipal mapping"))
      sites[[paste0(fam,"_cell_id")]]<-z$cell_id[pos]
      assert(max(abs(number(z$requested_municipal_latitude[pos],"latitude")-sites$latitude))<1e-7 &&
        max(abs(number(z$requested_municipal_longitude[pos],"longitude")-sites$longitude))<1e-7,"Coordinates differ from reference registry.")
    }
    assert(all(panel$MET_cell_id==sites$MET_cell_id[match(panel$municipality_code,sites$municipality_code)]),"Panel MET IDs differ.")
    used<-unique(c(sites$MET_cell_id,sites$CERES_cell_id))
    pos<-match(used,man$cell_id);assert(!anyNA(pos),"Source cell absent from manifest.")
    man<-man[pos,];man$local_file<-vapply(man$daily_file,local_path,character(1))
    assert(all(man$status %in% c("VALID_COMPLETE","VALID_WITH_QA_FLAGS")),"Source download incomplete.")
    assert(all(man$family[match(sites$MET_cell_id,man$cell_id)]=="MET") &&
      all(man$family[match(sites$CERES_cell_id,man$cell_id)]=="CERES"),"Source-family labels disagree with IDs.")
    sites$response_elevation_m<-number(man$response_elevation_m[match(sites$MET_cell_id,man$cell_id)],"MET elevation")
    assert(all(is.finite(sites$response_elevation_m)) && all(sites$response_elevation_m>=-500 & sites$response_elevation_m<=6000),"Invalid MET elevation for Rso.")
    # Byte checks are mandatory even on resumed runs; do not reuse changed data.
    message("Verifying ",nrow(man)," existing MET/CERES files (no downloads)...")
    assert(all(vapply(man$local_file,hash,character(1))==man$daily_md5),"A daily source file changed since step16.")
    report$source_files_verified<<-nrow(man)
    write_csv(man[c("cell_id","family","local_file","daily_md5")],file.path(out_dir,"coffee_water_balance_source_provenance.csv"))
    reference<-as.data.frame(readRDS(paths[["annual17"]]),stringsAsFactors=FALSE)
    need(reference,c("municipality_code","year","et0_total_mm","precipitation_total_mm"),"Step17 annual references")
    reference$municipality_code<-as.character(reference$municipality_code)
    reference<-reference[reference$year %in% 2001:2024 & reference$municipality_code %in% codes,]
    assert(nrow(reference)==length(codes)*24L && !anyDuplicated(paste(reference$municipality_code,reference$year)),"Incomplete step17 benchmark keys.")
    ref_groups<-split(seq_len(nrow(reference)),reference$municipality_code)
    write_csv(sites,file.path(out_dir,"coffee_water_balance_municipality_index.csv"))
    report$municipal_targets<<-length(codes)
    notes<-c("COFFE-STUDY | STEP22 | DAILY WATER BALANCE",paste("Version",version),
      paste("Executed",format(Sys.time(),usetz=TRUE)),
      "Simulation is continuous from 2001-01-01 to 2024-12-31, with no calendar resets.",
      "Meteorology MERRA-2 and CERES radiation from existing step16 source cells; UTC days.",
      "FAO56 ET0 functions and raw-input screening are copied from step17 and benchmarked annually.",
      "Municipal reference latitude and MET-cell response elevation are used as in step17; no farm altitude claim.",
      "All CAD (50,100,150,200 mm) by Kc (0.90,0.95,1.10) combinations are scenarios, not mapped soils or calibrated physiology.",
      "Central scenario CAD100_Kc095. Irrigation=0. Same scenarios for both species.",
      "P>=ETc: Snew=min(CAD,S+P-ETc), ETr=ETc, excess=max(0,S+P-ETc-CAD).",
      "P<ETc: Snew=S*exp((P-ETc)/CAD), ETr=P+S-Snew, excess=0.",
      "All rainfall enters the simplified balance; no separate interception, runoff, rooting dynamics, capillary/lateral input.",
      "Excess is combined reservoir overflow, not separately estimated deep drainage or surface runoff.",
      "Initial available-storage bounds are [0,CAD]. Feasible bounds propagate with the monotone equations.",
      "A missing P/ET0 day has unknown fluxes and end storage interval [0,CAD]; this is not a physical reset/recharge.",
      "Known future days contract the interval. No weather value is imputed and no unknown day is skipped.",
      "Strict scalar flux/storage uses the lower endpoint only when endpoints agree within 1e-6 mm; endpoints are always retained.",
      "Flux identification and storage identification are separate. A wet day can have known ETr and unknown storage/overflow.",
      "Each valid-forcing endpoint closes P-ETr-excess-(Snew-Sold)=0. Unknown days have no claimed closure.",
      "Bounds are physical feasibility intervals under these scenarios, not statistical intervals or total uncertainty.",
      "Seasonal ISNA=sum(ETr)/sum(ETc), only with complete forcing and identifiable ETr each day; ETc=0 daily ratios stay NA.",
      "Whole-window ISNA bounds are exported only when forcing is complete; bounds are not point-estimate replacements.",
      "Days and sequences below 0.8/0.5 are descriptors, not validated damage thresholds or yield-loss percentages.",
      "Zero-demand days break low-ISNA sequences and are excluded from ratio counts without implying moisture stress.",
      "The 2001-2009 run-up precedes all windows; initialization adequacy is checked, not assumed.",
      "Daily central scenario output is saved once per municipality; all-scenario daily output is opt-in.",
      "All scenarios have annual and W1-W4 summaries. Windows do not restart the continuous balance.",
      "All original step21 rows, values and eligibility remain in the central augmented panel. Missing water indicators have explicit flags.",
      "Canephora windows keep calendar labels; Arabica candidate phases are not verified farm phenology.",
      "No new regression, national climate attribution or additional main figure is produced.",
      "https://www.fao.org/4/x0490e/x0490e07.htm",
      "https://www.fao.org/4/x0490e/x0490e0b.htm",
      "https://www.sciencedirect.com/science/article/pii/0022169486901241")
    writeLines(notes,file.path(out_dir,"coffee_water_balance_method_notes.txt"))
    dict<-data.frame(metric=c("isna_ratio","isna_lower_bound/isna_upper_bound","etr_total_mm","etc_total_mm","deficit_total_mm",
      "excess_total_mm","days_isna_lt080/lt050","longest_isna_lt080/lt050_days","mean_relative_storage","maximum_storage_interval_mm"),
      definition=c("sum(ETr)/sum(ETc); requires complete forcing and identified ETr; not mean daily ratios",
        "Bounds on ratio from feasible storage trajectories when all period forcing is known; not confidence limits",
        "Sum of identified daily actual ET in the standardized rainfed bucket","Sum of Kc*FAO56 reference ET0",
        "sum(ETc-ETr); not yield loss","Sum of identified bucket overflow; no separate runoff/drainage partition",
        "Count positive-demand days below threshold, only with complete ratio support",
        "Maximum consecutive positive-demand days below threshold within the window",
        "Mean S/CAD; requires identifiable end-of-day storage each day","Largest feasible end-storage interval width within the window"),
      stringsAsFactors=FALSE)
    write_csv(dict,file.path(out_dir,"coffee_water_balance_dictionary.csv"))
    raw_vars<-c("tmean_c","tmin_c","tmax_c","dewpoint_mean_c","rhmean_pct","precipitation_mm","wind2m_mean_m_s","surface_pressure_kpa","shortwave_mj_m2")
    low<-c(-60,-60,-60,-90,0,0,0,40,0);high<-c(60,60,60,60,100,1500,100,110,60)
    source_cache<-new.env(parent=emptyenv());fifo<-character()
    get_cell<-function(id) {
      if(exists(id,envir=source_cache,inherits=FALSE))return(get(id,envir=source_cache))
      if(length(fifo)>=8L){rm(list=fifo[1L],envir=source_cache);fifo<<-fifo[-1L]}
      j<-match(id,man$cell_id);assert(!is.na(j),"Unknown source ID")
      d<-as.data.frame(readRDS(man$local_file[j]),stringsAsFactors=FALSE)
      vars<-if(man$family[j]=="MET")raw_vars[1:8] else raw_vars[9]
      need(d,c("date",vars,"cell_id","family","source","source_product","time_standard"),paste("Daily source",id))
      d$date<-as.Date(d$date)
      assert(!anyNA(d$date)&&!anyDuplicated(d$date)&&all(d$cell_id==id)&&all(d$source=="NASA POWER")&&all(d$time_standard=="UTC"),"Invalid source keys or metadata.")
      jj<-match(dates,d$date);assert(!anyNA(jj),paste("Missing daily dates",id))
      assert(all(d$family==man$family[j]),"Daily source-family identity mismatch.")
      expected_product<-if(man$family[j]=="MET") "MERRA" else "CERES|SYN1DEG"
      assert(all(grepl(expected_product,d$source_product,ignore.case=TRUE)),"Unknown daily source product.")
      d<-d[jj,c("date",vars),drop=FALSE]
      for(v in vars)d[[v]]<-number(d[[v]],v,TRUE)
      assign(id,d,envir=source_cache);fifo<<-c(fifo,id);d
    }
    cache_files<-setNames(file.path(cache_dir,paste0("M_",codes,"_water.rds")),codes)
    order_sites<-order(sites$MET_cell_id,sites$CERES_cell_id,sites$municipality_code)
    for(ii in seq_along(order_sites)) {
      site<-sites[order_sites[ii],,drop=FALSE];code<-site$municipality_code
      signature<-list(version=version,municipality_code=code,latitude=site$latitude,
        elevation=site$response_elevation_m,ids=c(site$MET_cell_id,site$CERES_cell_id),
        md5=man$daily_md5[match(c(site$MET_cell_id,site$CERES_cell_id),man$cell_id)],
        window_md5=provenance$md5[provenance$role=="windows"],
        reference_md5=provenance$md5[provenance$role=="annual17"],
        scenarios=scenarios,simulation=c(as.character(first_day),as.character(last_day)),
        tolerance_mm=tolerance_mm,all_daily=save_all_daily)
      cf<-cache_files[[code]]; df<-file.path(daily_dir,paste0("M_",code,"_CAD100_Kc095_2001_2024.rds"))
      af<-file.path(all_daily_dir,paste0("M_",code,"_all_scenarios_2001_2024.rds"))
      old<-if(valid_bytes(cf))tryCatch(readRDS(cf),error=function(e)NULL) else NULL
      reuse<-is.list(old)&&identical(old$signature,signature)&&is.data.frame(old$seasonal)&&
        nrow(old$seasonal)==52L*12L && valid_bytes(df) && identical(old$daily_md5,hash(df)) &&
        (!save_all_daily || (valid_bytes(af)&&identical(old$all_daily_md5,hash(af))))
      if(reuse) {
        result<-old;report$municipalities_reused<<-report$municipalities_reused+1L
      } else {
        if(report$new_municipalities>=limit_new) {
          report$execution_status<<-"PAUSED_LOCAL_LIMIT"
          report$last_stage<<-"Completed municipal caches retained; rerun this script. No partial full panel exported."
          save_progress();message(report$last_stage);return(invisible(NULL))
        }
        message(sprintf("[%d/%d] %s | continuous daily balance, 12 scenarios",ii,length(codes),site$municipality))
        met<-get_cell(site$MET_cell_id);sol<-get_cell(site$CERES_cell_id)
        d<-met;d$shortwave_mj_m2<-sol$shortwave_mj_m2[match(d$date,sol$date)]
        clean<-list();outside<-rep(FALSE,nrow(d))
        for(j in seq_along(raw_vars)) {
          v<-raw_vars[j];x<-d[[v]];bad<-!is.na(x)&(x<low[j]|x>high[j]);outside<-outside|bad
          x[bad]<-NA_real_;clean[[v]]<-x
        }
        bad_order<-(!is.na(clean$tmin_c)&!is.na(clean$tmean_c)&clean$tmin_c>clean$tmean_c+0.05)|
          (!is.na(clean$tmean_c)&!is.na(clean$tmax_c)&clean$tmean_c>clean$tmax_c+0.05)|
          (!is.na(clean$tmin_c)&!is.na(clean$tmax_c)&clean$tmin_c>clean$tmax_c)
        for(v in c("tmean_c","tmin_c","tmax_c"))clean[[v]][bad_order]<-NA_real_
        geo<-solar_geometry(dates,site$latitude,site$response_elevation_m)
        calc<-pm(clean$tmin_c,clean$tmax_c,clean$dewpoint_mean_c,clean$wind2m_mean_m_s,
          clean$surface_pressure_kpa,clean$shortwave_mj_m2,geo$ra_mj_m2,geo$rso_mj_m2)
        rr<-reference[ref_groups[[code]],];rr<-rr[match(2001:2024,rr$year),]
        totals<-vapply(aindex,function(q)sum(calc$et0_reference_mm[q]),numeric(1))
        rainfall<-vapply(aindex,function(q)sum(clean$precipitation_mm[q]),numeric(1))
        expected<-as.numeric(rr$et0_total_mm);difference<-totals-expected
        match_et<-is.na(totals)&is.na(expected) | (is.finite(difference)&abs(difference)<1e-6)
        pdiff<-rainfall-as.numeric(rr$precipitation_total_mm)
        assert(all(match_et)&&all((is.na(rainfall)&is.na(rr$precipitation_total_mm))|
          (is.finite(pdiff)&abs(pdiff)<1e-6)),paste("Step17 ET0/precipitation reproduction failed for",code))
        benchmark<-data.frame(municipality_code=code,year=2001:2024,et0_recomputed_mm=totals,
          et0_step17_mm=expected,absolute_difference_mm=abs(difference),verified=match_et)
        b<-balance_engine(clean$precipitation_mm,calc$et0_reference_mm,scenarios$cad_mm,scenarios$kc)
        assert(max(b$maximum_mass_residual_mm)<balance_tolerance_mm && sum(b$physical_failure_days)==0L,
          paste("Daily water-conservation/physical constraint test failed for",code))
        seasonal<-summarize_water(b,clean$precipitation_mm,win,windex)
        annual<-summarize_water(b,clean$precipitation_mm,annual_periods,aindex)
        for(v in c("municipality_code","municipality","state","MET_cell_id","CERES_cell_id")) {
          seasonal[[v]]<-site[[v]];annual[[v]]<-site[[v]]
        }
        for(z in c("seasonal","annual")) {
          tab<-get(z);tab$source<-"NASA POWER";tab$time_standard<-"UTC";tab$irrigation_mm_day<-0
          assign(z,tab)
        }
        g<-which(!b$forcing_known)
        # Only assess pure initialization before the first forcing gap.
        pre_gap<-if(length(g))seq_len(g[1L]-1L) else seq_along(dates)
        first_init<-vapply(seq_len(nrow(scenarios)),function(j) {
          jj<-pre_gap[(b$storage_high[pre_gap,j]-b$storage_low[pre_gap,j])<=tolerance_mm]
          if(length(jj))as.character(dates[jj[1L]]) else NA_character_
        },character(1))
        sw<-b$storage_high-b$storage_low
        qa<-data.frame(municipality_code=code,municipality=site$municipality,
          scenario_id=scenarios$scenario_id,cad_mm=scenarios$cad_mm,kc=scenarios$kc,
          daily_rows=length(dates),missing_forcing_days=length(g),
          negative_vpd_days=sum(calc$flag_negative_vpd),
          first_forcing_gap=if(length(g))as.character(dates[g[1L]]) else NA_character_,
          initial_bounds_converged_before_first_gap=first_init,
          initialization_converged_before_analysis=as.logical(!is.na(first_init)&as.Date(first_init)<min(win$start)),
          storage_bounds_known_at_analysis_start=sw[match(min(win$start),dates),]<=tolerance_mm,
          unresolved_storage_days=colSums(!is.finite(b$storage)),
          unresolved_etr_known_forcing_days=colSums(!is.finite(b$etr)&b$forcing_known),
          maximum_mass_residual_mm=b$maximum_mass_residual_mm,
          physical_failure_days=b$physical_failure_days,
          incomplete_seasonal_windows=vapply(scenarios$scenario_id,function(sc)sum(!is.finite(seasonal$isna_ratio[seasonal$scenario_id==sc])),integer(1)),
          minimum_isna=vapply(seq_len(nrow(scenarios)),function(j)if(all(is.na(b$isna[,j])))NA_real_ else min(b$isna[,j],na.rm=TRUE),numeric(1)),
          maximum_isna=vapply(seq_len(nrow(scenarios)),function(j)if(all(is.na(b$isna[,j])))NA_real_ else max(b$isna[,j],na.rm=TRUE),numeric(1)),
          stringsAsFactors=FALSE)
        # Central daily table: all days, both storage and flux bounds.
        j<-central
        daily<-data.frame(date=dates,precipitation_mm=clean$precipitation_mm,
          et0_reference_mm=calc$et0_reference_mm,irrigation_mm=0,etc_mm=b$etc[,j],
          etr_mm=b$etr[,j],isna=b$isna[,j],storage_mm=b$storage[,j],
          deficit_mm=b$etc[,j]-b$etr[,j],excess_mm=b$surplus[,j],
          storage_lower_mm=b$storage_low[,j],storage_upper_mm=b$storage_high[,j],
          etr_lower_mm=b$etr_low[,j],etr_upper_mm=b$etr_high[,j],
          excess_lower_mm=b$surplus_low[,j],excess_upper_mm=b$surplus_high[,j],
          forcing_missing=!b$forcing_known,prior_or_current_forcing_gap=b$gap_seen,
          flag_negative_vpd=calc$flag_negative_vpd,vpd_raw_kpa=calc$vpd_fao56_raw_kpa,
          et0_raw_mm=calc$et0_fao56_raw_mm,cloud_ratio_lower_flag=calc$flag_cloud_ratio_lower_bound,
          cloud_ratio_upper_flag=calc$flag_cloud_ratio_upper_bound,
          municipality_code=code,scenario_id=scenarios$scenario_id[j],cad_mm=scenarios$cad_mm[j],
          kc=scenarios$kc[j],MET_cell_id=site$MET_cell_id,CERES_cell_id=site$CERES_cell_id,
          time_standard="UTC",source="NASA POWER",stringsAsFactors=FALSE)
        write_rds(daily,df);writeLines(hash(df),paste0(df,".md5.txt"))
        if(save_all_daily) {
          write_rds(list(dates=dates,scenarios=scenarios,municipality=site,precipitation_mm=clean$precipitation_mm,
            et0_reference_mm=calc$et0_reference_mm,balance=b,signature=signature),af)
          writeLines(hash(af),paste0(af,".md5.txt"))
        }
        result<-list(signature=signature,seasonal=seasonal,annual=annual,qa=qa,
          benchmark=benchmark,daily_file=df,daily_md5=hash(df),
          all_daily_file=if(save_all_daily)af else NA_character_,all_daily_md5=if(save_all_daily)hash(af) else NA_character_)
        write_rds(result,cf);writeLines(hash(cf),paste0(cf,".md5.txt"))
        report$new_municipalities<<-report$new_municipalities+1L
        rm(b,daily,d,calc,geo,clean,sw,seasonal,annual)
      }
      report$municipalities_processed<<-report$municipalities_processed+1L
      report$maximum_mass_residual_mm<<-max(report$maximum_mass_residual_mm,result$qa$maximum_mass_residual_mm)
      diff<-result$benchmark$absolute_difference_mm
      if(any(is.finite(diff)))report$maximum_et0_benchmark_difference_mm<<-max(report$maximum_et0_benchmark_difference_mm,diff,na.rm=TRUE)
      report$et0_benchmarks_verified<<-report$et0_benchmarks_verified+sum(result$benchmark$verified)
      report$missing_forcing_municipal_days<<-report$missing_forcing_municipal_days+result$qa$missing_forcing_days[1L]
      progress[[length(progress)+1L]]<<-data.frame(municipality_code=code,cache_file=cf,cache_md5=hash(cf),
        daily_central_file=df,daily_central_md5=result$daily_md5,reused=reuse,stringsAsFactors=FALSE)
      if(ii%%10L==0L || ii==length(codes))save_progress()
      if(ii%%50L==0L)invisible(gc(FALSE))
      rm(result,old)
    }
    report$last_stage<<-"Combining cached annual/seasonal summaries and joining central indicators"
    save_progress()
    # Summaries only are combined; no multi-municipality daily matrix is built.
    readpart<-function(nm)bind(lapply(unname(cache_files),function(f)readRDS(f)[[nm]]))
    quality<-readpart("qa");benchmarks<-readpart("benchmark")
    write_csv(quality,file.path(out_dir,"coffee_water_balance_quality.csv"))
    write_csv(benchmarks,file.path(out_dir,"coffee_water_balance_et0_benchmark.csv"))
    annual<-readpart("annual")
    write_rds(annual,file.path(derived_dir,"coffee_water_balance_annual.rds"))
    write_csv(annual,file.path(derived_dir,"coffee_water_balance_annual.csv"));rm(annual);invisible(gc(FALSE))
    seasonal<-readpart("seasonal")
    assert(nrow(seasonal)==length(codes)*52L*12L &&
      !anyDuplicated(paste(seasonal$municipality_code,seasonal$year,seasonal$window_id,seasonal$scenario_id)),
      "Duplicate/incomplete water-season keys.")
    write_rds(seasonal,file.path(derived_dir,"coffee_water_balance_seasonal.rds"))
    write_csv(seasonal,file.path(derived_dir,"coffee_water_balance_seasonal.csv"))
    report$seasonal_rows<<-nrow(seasonal)
    report$seasonal_rows_with_unavailable_isna<<-sum(!is.finite(seasonal$isna_ratio))
    central_tab<-seasonal[seasonal$scenario_id==scenarios$scenario_id[central],]
    keep<-c("isna_ratio","deficit_total_mm","etr_total_mm","etc_total_mm","excess_total_mm",
      "days_isna_lt080","longest_isna_lt080_days","days_isna_lt050","longest_isna_lt050_days",
      "mean_relative_storage","isna_lower_bound","isna_upper_bound","missing_forcing_days","unresolved_etr_days","status")
    augmented<-panel
    for(w in paste0("W",1:4)) {
      z<-central_tab[central_tab$window_id==w,];kk<-match(paste(panel$municipality_code,panel$year),paste(z$municipality_code,z$year))
      assert(!anyNA(kk),paste("Missing central balance join:",w))
      for(v in keep)augmented[[paste0("wb_",w,"_",v)]]<-z[[v]][kk]
    }
    all_complete<-Reduce(`&`,lapply(paste0("W",1:4),function(w)is.finite(augmented[[paste0("wb_",w,"_isna_ratio")]])))
    augmented$wb_all_four_windows_complete<-all_complete
    augmented$included_step20_and_water_complete<-included & all_complete
    augmented$wb_central_scenario<-"CAD100_Kc095"
    augmented$wb_parameter_values_observed<-FALSE
    augmented$wb_rainfed_scenario<-TRUE
    assert(identical(augmented[names(panel)],panel),"Upstream panel values/order were changed.")
    write_rds(augmented,file.path(derived_dir,"coffee_yield_seasonal_water_balance_panel_2012_2024.rds"))
    write_csv(augmented,file.path(derived_dir,"coffee_yield_seasonal_water_balance_panel_2012_2024.csv"))
    report$central_panel_rows<<-nrow(augmented)
    report$central_eligible_rows_all_water_windows_complete<<-sum(included&all_complete)
    # Coverage relative to inherited eligibility, not a silently reduced sample.
    groups<-split(seq_len(nrow(seasonal)),paste(seasonal$scenario_id,seasonal$year,seasonal$window_id,sep="|"))
    coverage<-list()
    for(g in groups) {
      z<-seasonal[g,];yr<-z$year[1L]
      for(sp in c("Arabica","Canephora")) {
        rr<-which(panel$species==sp & panel$year==yr & included)
        jj<-match(panel$municipality_code[rr],z$municipality_code)
        assert(!anyNA(jj),"Missing scenario coverage join.")
        ok<-is.finite(z$isna_ratio[jj]); pp<-as.numeric(panel$production_t[rr]);aa<-as.numeric(panel$area_harvested_ha[rr])
        coverage[[length(coverage)+1L]]<-data.frame(species=sp,year=yr,window_id=z$window_id[1L],
          scenario_id=z$scenario_id[1L],eligible_step20_rows=length(rr),complete_isna_rows=sum(ok),
          missing_isna_rows=sum(!ok),production_coverage_pct=if(sum(pp)>0)100*sum(pp[ok])/sum(pp) else NA_real_,
          area_coverage_pct=if(sum(aa)>0)100*sum(aa[ok])/sum(aa) else NA_real_,
          eligible_MET_cells=length(unique(panel$MET_cell_id[rr])),
          complete_MET_cells=length(unique(panel$MET_cell_id[rr][ok])),
          source="NASA POWER",irrigation="Rainfed scenario",phenology_verified=FALSE,stringsAsFactors=FALSE)
      }
    }
    write_csv(bind(coverage),file.path(out_dir,"coffee_water_balance_coverage.csv"))
    assert(all(vapply(paths,hash,character(1))==provenance$md5),"An upstream input changed during this run.")
    report$execution_status<<-if(report$seasonal_rows_with_unavailable_isna>0L)"COMPLETED_WATER_BALANCE_WITH_GAPS" else "COMPLETED_WATER_BALANCE"
    report$last_stage<<-"All scenarios summarized; central daily files and unchanged-sample augmented panel saved. No regression fitted."
    save_progress()
    message("\n",report$execution_status,"\nReports: ",out_dir,
      "\nCentral daily balances: ",daily_dir,"\nSeasonal/annual tables and augmented panel: ",derived_dir)
  }
  tryCatch(worker(),error=function(e) {
    report$error_if_incomplete<<-substr(conditionMessage(e),1,1400)
    report$execution_status<<-"INCOMPLETE"
    try(save_progress(),silent=TRUE)
    stop(conditionMessage(e),"\nCompleted local caches retained.\nSummary: ",
      file.path(out_dir,"coffee_water_balance_summary.csv"),call.=FALSE)
  },interrupt=function(e) {
    report$execution_status<<-"INTERRUPTED_RESUMABLE"
    report$error_if_incomplete<<-"User interruption; completed caches retained."
    try(save_progress(),silent=TRUE)
    message("\nInterrupted. Rerun the same script to resume completed caches.")
    invisible(NULL)
  })
}

coffee_water_balance_step22()

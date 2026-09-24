# ======================================================================
# COFFE-STUDY | STEP 23 | SEASONAL WATER SATISFACTION AND SCENARIO SENSITIVITY
# Run this COMPLETE file with Source in RStudio.
#
# Reuses step22 municipality caches. No weather download, no new daily
# balance and no regression. Source files and previous outputs are unchanged.
# Primary descriptive support: the SAME municipalities with finite seasonal
# ISNA in ALL 13 reporting years, ALL four windows and ALL 12 scenarios.
# Selection is based on water-data completeness, not yield or significance.
# Weights: fixed mean harvested area in 2012-2015, separately by species.
# Report the retained baseline area: this subset is not the full target set.
# Full-target strict estimates and changing-available-support diagnostics are
# exported separately. No missing ISNA or weather value is replaced.
#
# Local ISNA = sum(ETr)/sum(ETc). Main spatial indicator = area-weighted mean
# of these local ratios. A ratio of spatially weighted ETr and ETc is ALSO
# exported, separately: it additionally weights locations by their demand.
# Scenario ranges are sensitivity ranges, NOT confidence intervals.
# ======================================================================

coffee_water_step23 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  out_dir <- file.path(base_dir, "04_outputs", "water_stress_synthesis")
  fig_dir <- file.path(base_dir, "05_figures", "Water_stress_diagnostics")
  upstream <- file.path(base_dir, "04_outputs", "daily_water_balance")
  years <- 2012:2024
  species <- c("Arabica", "Canephora")
  windows <- paste0("W", 1:4)
  central <- "CAD100_Kc095"
  tol <- 1e-7
  make_figures <- toupper(Sys.getenv("COFFEE_WATER_SYNTHESIS_PLOTS", "TRUE")) == "TRUE"
  for (x in c(out_dir, fig_dir)) {
    dir.create(x, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(x)) stop("Cannot create: ", x, call. = FALSE)
  }
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  lock <- file.path(out_dir, ".step23_running")
  if (!dir.create(lock, showWarnings = FALSE)) stop(
    "A step23 run may be active. Confirm it has stopped before removing:\n", lock, call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(d, cols, label) assert(all(cols %in% names(d)),
    paste(label, "missing:", paste(setdiff(cols, names(d)), collapse = ", ")))
  number <- function(x, label, missing_ok = FALSE) {
    a <- as.character(x)
    y <- suppressWarnings(as.numeric(a))
    assert(!any(!is.na(a) & is.na(y)) && all(is.na(y) | is.finite(y)) &&
      (missing_ok || !anyNA(y)), paste("Invalid numeric field:", label))
    y
  }
  logical_col <- function(x, label) {
    y <- toupper(as.character(x))
    assert(!anyNA(y) && all(y %in% c("TRUE", "FALSE")), paste("Invalid logical field:", label))
    y == "TRUE"
  }
  hash <- function(f) unname(tools::md5sum(f))
  read_csv <- function(f) {
    assert(file.exists(f), paste("Required input missing:", f))
    utils::read.csv(f, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(d, f) {
    tmp <- tempfile("water23_", tmpdir = dirname(f), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(d, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, f, overwrite = TRUE), paste("Cannot write:", f))
  }
  bind <- function(x) {
    z <- do.call(rbind, x)
    if (!is.null(z)) rownames(z) <- NULL
    z
  }
  checked <- function(d, key) {
    z <- d$result[d$check == key]
    assert(length(z) == 1L && !is.na(z), paste("Missing/ambiguous summary key:", key))
    z
  }
  # Only relocate the known project root; no filename or municipality matching.
  local_path <- function(x) {
    x <- gsub("\\", "/", x, fixed = TRUE)
    if (file.exists(x)) return(x)
    marker <- "/coffe-study/"
    pos <- regexpr(marker, x, fixed = TRUE)[1L]
    assert(pos > 0L, paste("Cannot resolve project path:", x))
    file.path(base_dir, substring(x, pos + nchar(marker)))
  }
  same_numbers <- function(x, y, threshold = tol) {
    (is.na(x) & is.na(y)) | (is.finite(x) & is.finite(y) & abs(x-y) <= threshold)
  }
  weighted_parts <- function(x, weights) {
    ok <- is.finite(x) & weights > 0
    target <- sum(weights)
    observed <- sum(weights[ok])
    available <- if (observed > 0) sum(x[ok] * weights[ok])/observed else NA_real_
    strict <- if (target > 0 && all(is.finite(x[weights > 0]))) available else NA_real_
    c(strict = strict, available = available,
      coverage = if (target > 0) observed/target else NA_real_)
  }
  tests <- data.frame(test = c("Weighted ratio differs from ratio of weighted totals",
    "Missing positive weight withholds full-support estimate", "Zero-weight NA is harmless",
    "Four-year difference uses equal annual weights", "Common support excludes any scenario gap"),
    pass = c(abs(weighted.mean(c(0.5,1),c(1,1)) - sum(c(5,100))/sum(c(10,100))) > 0.1,
      is.na(weighted_parts(c(NA,1),c(1,1))["strict"]) &&
        weighted_parts(c(NA,1),c(1,1))["coverage"] == 0.5,
      weighted_parts(c(NA,0.5),c(0,1))["strict"] == 0.5,
      mean(c(0.6,0.8,0.6,0.8))-mean(c(0.4,0.6,0.4,0.6)) > 0.199999,
      !all(is.finite(c(0.8,NA,0.9)))), stringsAsFactors = FALSE)
  write_csv(tests, file.path(out_dir, "coffee_water_stress_numerical_tests.csv"))
  assert(all(tests$pass), "A synthesis unit test failed.")

  report <- list(execution_status = "INCOMPLETE", script_version = "23_water_synthesis_v1",
    municipal_targets = 0L, common_support_municipalities = 0L,
    cache_files_checked = 0L, seasonal_rows_checked = 0L,
    seasonal_ratio_identity_max_error = 0, central_complete_eligible_rows = 0L,
    unchanged_step20_eligible_rows = 0L, common_support_selection =
      "Finite ISNA in all 13 years, 4 windows and 12 scenarios; not yield selection",
    weighting = "Fixed 2012-2015 mean harvested area, separately by species",
    full_target_diagnostic_substituted = FALSE, weather_downloads = 0L,
    daily_balance_recomputed = FALSE, regressions_fitted = FALSE,
    parameters_are_observations = FALSE, original_inputs_modified = FALSE,
    figure_status = "NOT_ATTEMPTED", error_if_incomplete = "None")
  save_report <- function() write_csv(data.frame(check = names(report), result =
    vapply(report, as.character, character(1))), file.path(out_dir, "coffee_water_stress_summary.csv"))
  on.exit({try(save_report(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()),
      file.path(out_dir, "sessionInfo_step23.txt")), silent = TRUE)}, add = TRUE)

  worker <- function() {
    paths <- c(summary = file.path(upstream, "coffee_water_balance_summary.csv"),
      scenarios = file.path(upstream, "coffee_water_balance_scenarios.csv"),
      quality = file.path(upstream, "coffee_water_balance_quality.csv"),
      cache_index = file.path(upstream, "coffee_water_balance_cache_index.csv"),
      benchmark = file.path(upstream, "coffee_water_balance_et0_benchmark.csv"),
      provenance22 = file.path(upstream, "coffee_water_balance_input_provenance.csv"))
    assert(all(file.exists(paths)), paste("Missing step22 files:", paste(paths[!file.exists(paths)],collapse="\n")))
    origins <- read_csv(paths[["provenance22"]])
    need(origins, c("role", "file", "md5"), "Step22 provenance")
    for (role in c("panel", "windows")) {
      rr <- which(origins$role == role)
      assert(length(rr) == 1L, paste("Missing upstream role:", role))
      f <- local_path(origins$file[rr])
      assert(file.exists(f) && identical(hash(f), origins$md5[rr]),
        paste("Upstream",role,"changed since step22; do not combine versions."))
      paths[role] <- f
    }
    provenance <- data.frame(role = names(paths), file = unname(paths),
      md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_water_stress_input_provenance.csv"))
    s22 <- read_csv(paths[["summary"]])
    assert(checked(s22,"execution_status") %in% c("COMPLETED_WATER_BALANCE", "COMPLETED_WATER_BALANCE_WITH_GAPS"),
      "Step22 has not finished. Complete it before running this synthesis.")
    assert(checked(s22,"numerical_tests_passed") == "TRUE", "Step22 numerical tests did not pass.")
    sc <- read_csv(paths[["scenarios"]]); need(sc,c("scenario_id","cad_mm","kc","central"),"Scenarios")
    sc$cad_mm <- number(sc$cad_mm,"CAD"); sc$kc <- number(sc$kc,"Kc")
    assert(nrow(sc)==12L && !anyDuplicated(sc$scenario_id) && central %in% sc$scenario_id &&
      setequal(sc$cad_mm,c(50,100,150,200)) && setequal(sc$kc,c(0.90,0.95,1.10)),"Unexpected scenarios.")
    sc <- sc[order(sc$kc,sc$cad_mm),]
    win <- read_csv(paths[["windows"]]); need(win,c("year","window_id","start","end","expected_days"),"Window dates")
    win$year <- as.integer(number(win$year,"window year"))
    win$expected_days <- number(win$expected_days,"window days")
    assert(nrow(win)==52L && !anyDuplicated(paste(win$year,win$window_id)),"Invalid window registry.")
    qa <- read_csv(paths[["quality"]]); need(qa,c("municipality_code","scenario_id", "incomplete_seasonal_windows",
      "physical_failure_days","maximum_mass_residual_mm","initialization_converged_before_analysis"),"Water QA")
    for(v in c("incomplete_seasonal_windows","physical_failure_days","maximum_mass_residual_mm")) qa[[v]]<-number(qa[[v]],v)
    assert(all(qa$physical_failure_days==0) && max(qa$maximum_mass_residual_mm)<1e-6,
      "Water balance physical checks failed; no synthesis produced.")
    assert(all(logical_col(qa$initialization_converged_before_analysis,"initialization")),
      "Initial-state uncertainty has not converged for every scenario; review step22.")
    bench <- read_csv(paths[["benchmark"]])
    assert(all(logical_col(bench$verified,"ET0 benchmark")),"ET0 benchmark failure.")
    ci <- read_csv(paths[["cache_index"]]); need(ci,c("municipality_code","cache_file","cache_md5"),"Cache index")
    assert(!anyNA(ci$municipality_code)&&!anyDuplicated(ci$municipality_code),"Duplicate/invalid cache codes.")
    ci <- ci[order(ci$municipality_code),]
    codes <- ci$municipality_code
    assert(nrow(ci)==as.integer(checked(s22,"municipalities_processed")) &&
      nrow(qa)==nrow(ci)*12L && !anyDuplicated(paste(qa$municipality_code,qa$scenario_id)) &&
      setequal(qa$municipality_code,codes),"Incomplete municipality/scenario QA.")
    common <- vapply(codes,function(code) {
      z<-qa[qa$municipality_code==code,]
      assert(setequal(z$scenario_id,sc$scenario_id),paste("Missing scenarios:",code))
      all(z$incomplete_seasonal_windows==0)
    },logical(1))

    panel <- as.data.frame(readRDS(paths[["panel"]]), stringsAsFactors=FALSE)
    need(panel,c("municipality_code","municipality","state","year","species","production_t", "area_harvested_ha","included_step20"),"Seasonal panel")
    panel$municipality_code<-as.character(panel$municipality_code)
    panel$year<-as.integer(number(panel$year,"PAM year"))
    included<-logical_col(panel$included_step20,"included_step20")
    panel<-panel[c("municipality_code","municipality","state","year","species","production_t","area_harvested_ha")]
    panel$included_step20<-included
    for(v in c("production_t","area_harvested_ha")) panel[[v]]<-number(panel[[v]],v)
    assert(setequal(panel$municipality_code,codes) && setequal(panel$species,species) && setequal(panel$year,years) &&
      nrow(panel)==length(codes)*2L*13L && !anyDuplicated(paste(panel$municipality_code,panel$species,panel$year)),
      "The original complete code/year/species panel has changed.")
    assert(all(panel$area_harvested_ha>=0) && all(panel$production_t>=0),"Negative production/area.")
    base <- stats::aggregate(panel$area_harvested_ha[panel$year %in% 2012:2015],
      list(municipality_code=panel$municipality_code[panel$year %in% 2012:2015],
           species=panel$species[panel$year %in% 2012:2015]),mean)
    names(base)[3L]<-"baseline_area_ha"
    area <- vapply(species,function(sp) {
      z<-base[base$species==sp,];z$baseline_area_ha[match(codes,z$municipality_code)]
    },numeric(length(codes)))
    assert(!anyNA(area)&&all(colSums(area)>0)&&all(colSums(area[common,,drop=FALSE])>0),"Empty area-weight support.")
    membership<-data.frame(municipality_code=codes,
      municipality=panel$municipality[match(codes,panel$municipality_code)],
      state=panel$state[match(codes,panel$municipality_code)],
      common_all_years_windows_scenarios=unname(common),area,stringsAsFactors=FALSE)
    names(membership)[5:6]<-paste0(species,"_baseline_area_ha")
    write_csv(membership,file.path(out_dir,"coffee_water_stress_support_membership.csv"))
    support <- data.frame(species=species,
      original_positive_weight_municipalities=colSums(area>0),
      retained_positive_weight_municipalities=colSums(area[common,,drop=FALSE]>0),
      original_baseline_area_ha=colSums(area), retained_baseline_area_ha=colSums(area[common,,drop=FALSE]),
      retained_baseline_area_pct=100*colSums(area[common,,drop=FALSE])/colSums(area),
      definition="Same complete municipalities across all years/windows/scenarios",stringsAsFactors=FALSE)
    write_csv(support,file.path(out_dir,"coffee_water_stress_support_coverage.csv"))
    report$municipal_targets<<-length(codes);report$common_support_municipalities<<-sum(common)
    report$unchanged_step20_eligible_rows<<-sum(included)

    jobs <- expand.grid(year=years,window_id=windows,scenario_id=sc$scenario_id,stringsAsFactors=FALSE)
    job_key<-paste(jobs$year,jobs$window_id,jobs$scenario_id,sep="|")
    jw<-match(paste(jobs$year,jobs$window_id),paste(win$year,win$window_id))
    js<-match(jobs$scenario_id,sc$scenario_id)
    jobs$cad_mm<-sc$cad_mm[js]; jobs$kc<-sc$kc[js]
    jobs$period_start<-win$start[jw];jobs$period_end<-win$end[jw];jobs$expected_days<-win$expected_days[jw]
    metrics<-c("isna_ratio","etr_total_mm","etc_total_mm","deficit_total_mm",
      "days_isna_lt080","days_isna_lt050","longest_isna_lt080_days","longest_isna_lt050_days")
    units<-c("ratio","mm","mm","mm","days","days","days","days")
    scopes<-c("Full_target_support","Common_complete_support")
    nj<-nrow(jobs);nm<-length(metrics)
    dims<-c(nj,nm,2L,2L)
    sumx<-observed_area<-counts<-array(0,dim=dims)
    sum_etr_joint<-sum_etc_joint<-joint_area<-array(0,dim=c(nj,2L,2L))
    common_by_year_central<-common_by_year_all<-matrix(FALSE,nrow=length(codes),ncol=13L)
    cache_audit<-vector("list",length(codes))
    for(i in seq_along(codes)) {
      code<-codes[i]
      if(i==1L||i%%50L==0L||i==length(codes)) message(sprintf("[%d/%d] Reading existing summaries",i,length(codes)))
      f<-local_path(ci$cache_file[i])
      assert(file.exists(f)&&identical(hash(f),ci$cache_md5[i]),paste("Cache absent/changed:",code))
      obj<-readRDS(f);z<-as.data.frame(obj$seasonal,stringsAsFactors=FALSE)
      need(z,c("municipality_code","year","window_id","scenario_id","cad_mm","kc","period_start", "period_end",
        "expected_days","status","isna_lower_bound","isna_upper_bound",metrics),paste("Cache",code))
      assert(nrow(z)==nj&&all(as.character(z$municipality_code)==code),paste("Wrong cache scope:",code))
      zk<-paste(z$year,z$window_id,z$scenario_id,sep="|")
      assert(!anyDuplicated(zk)&&setequal(zk,job_key),paste("Missing/duplicate seasonal keys:",code))
      z<-z[match(job_key,zk),]
      assert(all(as.character(z$period_start)==jobs$period_start)&all(as.character(z$period_end)==jobs$period_end)&
        all(z$expected_days==jobs$expected_days)&all(z$cad_mm==jobs$cad_mm)&all(z$kc==jobs$kc),paste("Scenario/window mismatch:",code))
      ok<-is.finite(z$isna_ratio)
      assert(all(!ok| (z$isna_ratio>=-tol & z$isna_ratio<=1+tol))&&
        all(!ok|is.finite(z$etr_total_mm)&is.finite(z$etc_total_mm)&z$etc_total_mm>0),paste("Invalid ISNA:",code))
      ratio_error<-abs(z$isna_ratio[ok]-z$etr_total_mm[ok]/z$etc_total_mm[ok])
      assert(all(ratio_error<tol)&&all(same_numbers(z$deficit_total_mm[ok],z$etc_total_mm[ok]-z$etr_total_mm[ok])),
        paste("Ratio/deficit identity failure:",code))
      assert(all(z$status[ok]=="COMPLETE"),paste("ISNA not marked complete:",code))
      boundsok<-ok&is.finite(z$isna_lower_bound)&is.finite(z$isna_upper_bound)
      assert(all(z$isna_ratio[boundsok]>=z$isna_lower_bound[boundsok]-tol)&
        all(z$isna_ratio[boundsok]<=z$isna_upper_bound[boundsok]+tol),paste("ISNA outside feasible bounds:",code))
      qcode<-qa[qa$municipality_code==code,]
      for(scid in sc$scenario_id) assert(sum(!ok[z$scenario_id==scid])==
        qcode$incomplete_seasonal_windows[qcode$scenario_id==scid],paste("QA count mismatch:",code,scid))
      assert(all(ok)==common[i],paste("Common-support mismatch:",code))
      for(y in seq_along(years)) {
        sel<-z$year==years[y]
        common_by_year_central[i,y]<-all(ok[sel&z$scenario_id==central])
        common_by_year_all[i,y]<-all(ok[sel])
      }
      mat<-as.matrix(z[metrics]);storage.mode(mat)<-"double"
      obs<-is.finite(mat);mat[!obs]<-0
      for(s in 1:2) if(area[i,s]>0) {
        for(h in 1:2) if(h==1L||common[i]) {
          a<-area[i,s]
          sumx[,,s,h]<-sumx[,,s,h]+mat*a
          observed_area[,,s,h]<-observed_area[,,s,h]+obs*a
          counts[,,s,h]<-counts[,,s,h]+obs
          e<-z$etr_total_mm; d<-z$etc_total_mm;e[!ok]<-0;d[!ok]<-0
          sum_etr_joint[,s,h]<-sum_etr_joint[,s,h]+e*a
          sum_etc_joint[,s,h]<-sum_etc_joint[,s,h]+d*a
          joint_area[,s,h]<-joint_area[,s,h]+ok*a
        }
      }
      report$cache_files_checked<<-i;report$seasonal_rows_checked<<-report$seasonal_rows_checked+nrow(z)
      if(length(ratio_error)) report$seasonal_ratio_identity_max_error<<-max(report$seasonal_ratio_identity_max_error,ratio_error)
      cache_audit[[i]]<-data.frame(municipality_code=code,file=f,md5=ci$cache_md5[i],seasonal_rows=nrow(z),
        unavailable_isna=sum(!ok),ratio_identity_max_error=if(length(ratio_error))max(ratio_error) else NA_real_)
      if(i%%100L==0L)save_report()
    }
    write_csv(bind(cache_audit),file.path(out_dir,"coffee_water_stress_cache_verification.csv"))
    rows<-list();flux_rows<-list()
    for(s in 1:2)for(h in 1:2) {
      target_area<-if(h==1L)sum(area[,s]) else sum(area[common,s])
      n_target<-if(h==1L)sum(area[,s]>0) else sum(area[common,s]>0)
      for(m in seq_along(metrics)) {
        valid<-observed_area[,m,s,h]>0
        estimate<-rep(NA_real_,nj);estimate[valid]<-sumx[valid,m,s,h]/observed_area[valid,m,s,h]
        strict<-estimate;strict[counts[,m,s,h]!=n_target]<-NA_real_
        rows[[length(rows)+1L]]<-data.frame(jobs,species=species[s],support=scopes[h],metric=metrics[m],unit=units[m],
          estimate_strict=strict,available_weight_diagnostic=estimate,
          observed_support_area_pct=100*observed_area[,m,s,h]/target_area,
          observed_original_area_pct=100*observed_area[,m,s,h]/sum(area[,s]),
          positive_weight_municipalities=n_target,contributing_municipalities=counts[,m,s,h],
          weighting="Fixed 2012-2015 mean harvested area",stringsAsFactors=FALSE)
      }
      ratio<-sum_etr_joint[,s,h]/sum_etc_joint[,s,h]
      ratio[!is.finite(ratio)]<-NA_real_
      strict<-ratio;strict[counts[,1L,s,h]!=n_target]<-NA_real_
      flux_rows[[length(flux_rows)+1L]]<-data.frame(jobs,species=species[s],support=scopes[h],
        demand_weighted_ratio_strict=strict,demand_weighted_ratio_available_diagnostic=ratio,
        observed_original_area_pct=100*joint_area[,s,h]/sum(area[,s]),
        note="Ratio of weighted ETr and ETc totals on the SAME ISNA-valid municipalities; not main area-mean ISNA")
    }
    exposure<-bind(rows)
    write_csv(exposure,file.path(out_dir,"coffee_water_stress_exposure.csv"))
    write_csv(bind(flux_rows),file.path(out_dir,"coffee_water_stress_demand_weighted_ratios.csv"))
    primary<-exposure[exposure$support=="Common_complete_support" & exposure$metric=="isna_ratio",]
    assert(all(is.finite(primary$estimate_strict)),"Common-support ISNA unexpectedly incomplete.")

    comparisons<-list()
    for(sp in species)for(scid in sc$scenario_id)for(w in windows) {
      z<-primary[primary$species==sp&primary$scenario_id==scid&primary$window_id==w,]
      assert(nrow(z)==13L&&setequal(z$year,years),"Incomplete primary temporal series.")
      early<-mean(z$estimate_strict[z$year %in% 2012:2015])
      late<-mean(z$estimate_strict[z$year %in% 2021:2024])
      comparisons[[length(comparisons)+1L]]<-data.frame(species=sp,window_id=w,scenario_id=scid,
        cad_mm=z$cad_mm[1L],kc=z$kc[1L],baseline_period="2012 to 2015",recent_period="2021 to 2024",
        baseline_isna=early,recent_isna=late,change_ratio=late-early,change_pp=100*(late-early),
        retained_baseline_area_pct=support$retained_baseline_area_pct[match(sp,support$species)],
        support="Same complete municipalities in all 12 scenarios and 13 years",
        meaning="Difference in mean seasonal water satisfaction under rainfed scenarios, not yield loss",stringsAsFactors=FALSE)
    }
    comparison<-bind(comparisons)
    write_csv(comparison,file.path(out_dir,"coffee_water_stress_period_comparison.csv"))
    sensitivity<-bind(lapply(split(comparison,paste(comparison$species,comparison$window_id)),function(z) {
      zz<-z[z$scenario_id==central,]
      data.frame(species=zz$species,window_id=zz$window_id,central_baseline_isna=zz$baseline_isna,
        central_recent_isna=zz$recent_isna,central_change_pp=zz$change_pp,
        scenario_minimum_change_pp=min(z$change_pp),scenario_maximum_change_pp=max(z$change_pp),
        scenarios_with_decrease=sum(z$change_pp< -tol),scenarios_with_increase=sum(z$change_pp>tol),
        scenario_count=nrow(z),retained_baseline_area_pct=zz$retained_baseline_area_pct,
        uncertainty_type="Parameter sensitivity envelope, NOT a statistical confidence interval")
    }))
    write_csv(sensitivity,file.path(out_dir,"coffee_water_stress_scenario_sensitivity.csv"))
    pi<-match(panel$municipality_code,codes);py<-match(panel$year,years)
    panel$central_all_windows_complete<-common_by_year_central[cbind(pi,py)]
    panel$all_scenarios_all_windows_complete<-common_by_year_all[cbind(pi,py)]
    report$central_complete_eligible_rows<<-sum(panel$included_step20&panel$central_all_windows_complete)
    assert(report$central_complete_eligible_rows==as.integer(checked(s22,"central_eligible_rows_all_water_windows_complete")),
      "Central regression-readiness count differs from step22.")
    readiness<-bind(lapply(split(panel,paste(panel$species,panel$year)),function(z) {
      z<-z[z$included_step20,]
      a<-z$central_all_windows_complete;b<-z$all_scenarios_all_windows_complete
      data.frame(species=z$species[1L],year=z$year[1L],step20_eligible_rows=nrow(z),
        central_complete_rows=sum(a),all_scenarios_complete_rows=sum(b),
        central_production_coverage_pct=100*sum(z$production_t[a])/sum(z$production_t),
        all_scenarios_production_coverage_pct=100*sum(z$production_t[b])/sum(z$production_t),
        central_area_coverage_pct=100*sum(z$area_harvested_ha[a])/sum(z$area_harvested_ha),
        all_scenarios_area_coverage_pct=100*sum(z$area_harvested_ha[b])/sum(z$area_harvested_ha))
    }))
    write_csv(readiness,file.path(out_dir,"coffee_water_stress_regression_readiness.csv"))
    write_csv(panel[c("municipality_code","species","year","included_step20","central_all_windows_complete",
      "all_scenarios_all_windows_complete")],file.path(out_dir,"coffee_water_stress_record_eligibility.csv"))

    notes<-c("COFFE-STUDY | STEP23 | WATER SATISFACTION SYNTHESIS",paste("Executed",format(Sys.time(),usetz=TRUE)),
      "Uses verified step22 caches; daily forcing, FAO56 and water balance are not rerun.",
      "Local seasonal ISNA remains sum(ETr)/sum(ETc), not the mean daily ratio.",
      "Main spatial ISNA is the fixed-harvested-area-weighted mean of local seasonal ratios.",
      "A separate ratio of area-weighted ETr/ETc totals has demand-weighted spatial interpretation.",
      "Weights use 2012-2015 mean areas from the unchanged step21 panel, independently by species.",
      "Primary descriptive support is common across all four windows, all 13 years and all 12 scenarios.",
      "Common support is chosen solely by finite ISNA; no yield association or model fitting determines it.",
      "Weights are explicitly renormalized ONCE on this fixed subset; report retained baseline-area fractions.",
      "The subset does not restore full target geography or remove selection from missing climate.",
      "Full-target strict estimates and varying-available-support diagnostics are exported but not substituted.",
      "Only primary ISNA is used in period-change and sensitivity plots; other metrics retain own coverage.",
      "Period summaries are means of four yearly window ratios, not ratios pooled across multiple years.",
      "Scenario envelopes are ranges of 12 parameter configurations, not confidence limits or adaptation effects.",
      "CAD/Kc are assumed; no municipal soils or observed irrigation are inferred. Same scenarios for both species.",
      "Regressions are NOT fitted. Record-level readiness keeps step20 eligibility and labels new missingness.",
      "Descriptive common-geography support and regression record-level support are different estimands.",
      "W1-W4 remain candidate calendar windows; verified farm phenology and harvest dates remain unavailable.",
      "No result is a measured plant stress, irrigation requirement, yield-loss percentage or climate-attributable loss.",
      "Figures are water-balance diagnostics, not a new numbered main figure.")
    writeLines(notes,file.path(out_dir,"coffee_water_stress_method_notes.txt"))
    writeLines(c("Seasonal simulated water satisfaction under standardized rainfed scenarios.",
      "Top: area-weighted mean local seasonal ETr/ETc in the central CAD100 mm, Kc0.95 scenario, with fixed 2012-2015 harvested-area weights.",
      "Bottom: differences in mean water satisfaction between 2021-2024 and 2012-2015, in percentage points. Points are central; segments span all 12 CAD/Kc scenarios, NOT confidence intervals.",
      "The same complete municipality subset is used in all years/windows/scenarios. Its original area fraction is reported above each species panel.",
      "W1 Sep(t-2)-Mar(t-1); W2 Apr-Aug(t-1); W3 Sep-Dec(t-1); W4 Jan-Mar(t). Calendar windows are not verified local phenology.",
      "Water satisfaction is not a percentage of realized crop productivity."),file.path(out_dir,"coffee_water_stress_caption_draft.txt"))
    if(make_figures) {
      if(requireNamespace("ggplot2",quietly=TRUE) && requireNamespace("patchwork",quietly=TRUE) &&
         utils::packageVersion("ggplot2") >= "3.5.0") {
        font<-"serif"
        if(requireNamespace("systemfonts",quietly=TRUE) &&
           "Times New Roman" %in% systemfonts::system_fonts()$family) font<-"Times New Roman"
        report$font_used<<-font
        theme_rogerio<-function() ggplot2::theme_minimal(base_size=14,base_family=font)+
          ggplot2::theme(panel.grid=ggplot2::element_blank(),
            axis.line=ggplot2::element_line(colour="black",linewidth=0.4),
            axis.ticks=ggplot2::element_line(colour="black",linewidth=0.4),
            axis.text=ggplot2::element_text(colour="black"),
            legend.position="top",legend.title=ggplot2::element_blank(),
            strip.text=ggplot2::element_text(face="bold"),panel.spacing=grid::unit(0.8,"lines"),
            plot.title=ggplot2::element_text(face="bold"),plot.subtitle=ggplot2::element_text(size=11))
        labels<-setNames(sprintf("%s | %.1f%% of baseline area",species,support$retained_baseline_area_pct),species)
        top<-primary[primary$scenario_id==central,]; top$species<-factor(top$species,levels=species)
        top$window_id<-factor(top$window_id,levels=windows)
        bottom<-sensitivity;bottom$species<-factor(bottom$species,levels=species)
        bottom$window_id<-factor(bottom$window_id,levels=windows)
        radius<-max(1,ceiling(max(abs(c(bottom$scenario_minimum_change_pp,bottom$scenario_maximum_change_pp)))*1.15))
        p1<-ggplot2::ggplot(top,ggplot2::aes(year,estimate_strict,linetype=window_id))+
          ggplot2::geom_line(linewidth=0.7)+
          ggplot2::facet_wrap(~species,nrow=1,axes="all",axis.labels="all",labeller=ggplot2::as_labeller(labels))+
          ggplot2::scale_linetype_manual(values=c("solid","dashed","dotdash","longdash"),drop=FALSE)+
          ggplot2::scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+
          ggplot2::scale_x_continuous(breaks=c(2012,2015,2018,2021,2024))+
          ggplot2::labs(x=NULL,y="Seasonal ETr / ETc",title="Simulated water satisfaction",
            subtitle="Central rainfed scenario; fixed area weights on common complete support")+theme_rogerio()
        p2<-ggplot2::ggplot(bottom,ggplot2::aes(window_id,central_change_pp))+
          ggplot2::geom_hline(yintercept=0,linetype="dotted",linewidth=0.4)+
          ggplot2::geom_linerange(ggplot2::aes(ymin=scenario_minimum_change_pp,ymax=scenario_maximum_change_pp),linewidth=0.7)+
          ggplot2::geom_point(size=2.5)+
          ggplot2::facet_wrap(~species,nrow=1,axes="all",axis.labels="all")+
          ggplot2::scale_y_continuous(limits=c(-radius,radius))+
          ggplot2::labs(x="Calendar window",y="Change in water satisfaction (pp)",
            title="2021 to 2024 minus 2012 to 2015",
            subtitle="Point: central scenario; segment: range across all 12 CAD/Kc scenarios")+theme_rogerio()
        fig<-patchwork::wrap_plots(p1,p2,ncol=1,heights=c(1.15,1))
        png<-file.path(fig_dir,"Coffee_water_satisfaction_scenarios.png")
        dev<-if(requireNamespace("ragg",quietly=TRUE)) ragg::agg_png else "png"
        ggplot2::ggsave(png,fig,width=12,height=9.5,dpi=320,device=dev,bg="white",limitsize=FALSE)
        report$figure_status<<-"PNG_SAVED"
      } else { report$figure_status<<-"SKIPPED_PLOTTING_PACKAGES_UNAVAILABLE"
        message("Tables complete. Figure needs ggplot2 >=3.5 and patchwork already installed.") }
    } else report$figure_status<<-"SKIPPED_BY_CONFIGURATION"
    assert(all(vapply(paths,hash,character(1))==provenance$md5),"An upstream input changed during execution.")
    report$execution_status<<-"COMPLETED_WATER_STRESS_SYNTHESIS"
    save_report()
    message("\n",report$execution_status,"\nResults: ",out_dir,"\nFigure: ",fig_dir)
    invisible(list(support=support,comparison=comparison,sensitivity=sensitivity,readiness=readiness))
  }
  tryCatch(worker(), error=function(e) {
    report$error_if_incomplete<<-conditionMessage(e)
    try(save_report(),silent=TRUE)
    stop(conditionMessage(e),"\nOriginal water caches were not modified.\nReport: ",
      file.path(out_dir,"coffee_water_stress_summary.csv"),call.=FALSE)
  })
}

coffee_water_step23()

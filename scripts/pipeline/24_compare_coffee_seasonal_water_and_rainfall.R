# ======================================================================
# COFFE-STUDY | STEP 24 | SEASONAL RAINFALL VERSUS WATER-SATISFACTION INDICES
# Run this COMPLETE file with Source in RStudio. No earlier script is run.
#
# Inputs: unchanged step21 seasonal panel and step22 summary caches, verified
# by the completed step23 v2 reconciliation. NO weather/water recalculation.
#
# Response: observed log productivity. Equal weight per municipality-year.
# All comparisons share one sample, separately by species:
#   step20 eligibility + finite ISNA for ALL 4 windows and ALL 12 scenarios;
#   reapply >=10 retained years, >=4 in each calendar parity to ensure the
#   previously required time support still exists AFTER water-data exclusions.
# This is NOT the 1673-municipality descriptive common-geography subset.
#
# Fixed, prespecified model families, each with BOTH nuisance specifications:
# H: heat in W1-W4 (reference)
# P: heat + precipitation in W1-W4
# W: heat + water limitation in W1-W4, for each of the 12 CAD/Kc scenarios.
# Water limitation = (1 - sum(ETr)/sum(ETc))/0.10 at municipality/window level.
# Thus one regression unit means a 0.10 LOWER seasonal satisfaction ratio.
# Heat unit: +10 Tmax>=32 C days. Precipitation unit: +100 mm per window.
# Both control sets fit municipal intercepts, municipal linear trends, year
# effects jointly with regressors. The second adds municipal odd/even effects.
# Do not interpret calendar parity as observed biennial bearing phase.
#
# Primary intervals: MET cell + CERES cell + year multiway clustered covariance.
# MET cell + year is an explicitly labelled sensitivity for comparison with
# step20. Covariance is NOT forced positive semidefinite; invalid intervals
# are withheld. Only 13 year clusters: inferential results are exploratory.
#
# H/P/W models are compared within the same sample and nuisance specification.
# Partial R2 is IN SAMPLE, not predictive skill or a fraction of total crop loss.
# W1-W4 are candidate calendars, not observed regional phenology. W4 can
# overlap harvest for some systems. No causal loss, adaptation benefit, or
# operational forecast is estimated. Rainfed water indices are scenarios.
# All model families and all CAD/Kc results are retained, with no selection
# based on coefficient signs, interval exclusion or goodness of fit.
#
# Optional environment settings:
#   COFFEE_STUDY_DIR                 project root override
#   COFFEE_STEP24_TEST_ONLY=TRUE     numerical tests only
#   COFFEE_STEP24_SKIP_PLOTS=TRUE    tables only
#   COFFEE_STEP24_SKIP_OMISSION=TRUE skip reporting-year sensitivity
# Output: 04_outputs/seasonal_water_associations and
#         05_figures/Figure04_seasonal_water_diagnostic
#
# API documentation checked 2026-09-21:
# https://lrberge.github.io/fixest/reference/feols.html
# https://lrberge.github.io/fixest/reference/vcov_cluster.html
# https://lrberge.github.io/fixest/reference/ssc.html
# ======================================================================

coffee_step24 <- function() {
  version <- "24_seasonal_water_vs_rain_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  test_only <- toupper(Sys.getenv("COFFEE_STEP24_TEST_ONLY", "FALSE")) == "TRUE"
  make_plot <- toupper(Sys.getenv("COFFEE_STEP24_SKIP_PLOTS", "FALSE")) != "TRUE"
  do_omission <- toupper(Sys.getenv("COFFEE_STEP24_SKIP_OMISSION", "FALSE")) != "TRUE"
  years <- 2012:2024
  windows <- paste0("W", 1:4)
  species_set <- c("Arabica", "Canephora")
  central <- "CAD100_Kc095"
  min_years <- 10L
  min_parity <- 4L
  tol <- 1e-7
  out_dir <- file.path(base_dir,"04_outputs","seasonal_water_associations")
  fig_dir <- file.path(base_dir,"05_figures","Figure04_seasonal_water_diagnostic")
  for (p in c(out_dir,fig_dir)) {
    dir.create(p,recursive=TRUE,showWarnings=FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ",p,call.=FALSE)
  }
  lock <- file.path(out_dir,".step24_running")
  if (!dir.create(lock,showWarnings=FALSE)) stop(
    "A step24 session may be active. Confirm that it has stopped before removing:\n",lock,call.=FALSE)
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  old <- options(scipen=999)
  on.exit(options(old),add=TRUE)
  writeLines(paste("PID",Sys.getpid(),format(Sys.time(),usetz=TRUE)),file.path(lock,"owner.txt"))

  assert <- function(ok,msg) if (!isTRUE(ok)) stop(msg,call.=FALSE)
  need <- function(d,cols,label) assert(all(cols %in% names(d)),
    paste(label,"missing:",paste(setdiff(cols,names(d)),collapse=", ")))
  bind <- function(x) {if (!length(x)) return(data.frame());z<-do.call(rbind,x);rownames(z)<-NULL;z}
  hash <- function(p) unname(tools::md5sum(p))
  read_csv <- function(p) {
    assert(file.exists(p),paste("Required input not found:",p))
    utils::read.csv(p,colClasses="character",stringsAsFactors=FALSE,check.names=FALSE,
      na.strings=c("NA",""),fileEncoding="UTF-8")
  }
  num <- function(x,label,allow_na=TRUE) {
    z<-suppressWarnings(as.numeric(as.character(x)))
    assert(!any(!is.na(x)&is.na(z))&&all(is.na(z)|is.finite(z))&&(allow_na||!anyNA(z)),
      paste("Invalid numeric field:",label));z
  }
  bool <- function(x,label) {
    z<-toupper(as.character(x));assert(!anyNA(z)&&all(z %in% c("TRUE","FALSE")),
      paste("Invalid logical field:",label));z=="TRUE"
  }
  write_csv <- function(x,p) {
    t<-tempfile("s24_",tmpdir=dirname(p),fileext=".csv");on.exit(unlink(t),add=TRUE)
    utils::write.csv(x,t,row.names=FALSE,na="NA",fileEncoding="UTF-8")
    assert(file.copy(t,p,overwrite=TRUE),paste("Cannot write:",p))
  }
  write_rds <- function(x,p) {
    t<-tempfile("s24_",tmpdir=dirname(p),fileext=".rds");on.exit(unlink(t),add=TRUE)
    saveRDS(x,t,compress="gzip");assert(file.copy(t,p,overwrite=TRUE),paste("Cannot write:",p))
  }
  local_path <- function(x) {
    assert(length(x)==1L&&!is.na(x),"Missing/ambiguous project path.")
    x<-gsub("\\","/",x,fixed=TRUE)
    if (file.exists(x)) return(x)
    token<-"/coffe-study/";i<-regexpr(token,x,fixed=TRUE)[1L]
    assert(i>0L,paste("Cannot relocate known project root:",x))
    file.path(base_dir,substring(x,i+nchar(token)))
  }
  check <- function(d,nm) {
    need(d,c("check","result"),"Summary");z<-d$result[d$check==nm]
    assert(length(z)==1L&&!is.na(z),paste("Missing/ambiguous check:",nm));z
  }
  pkey <- function(d) paste(d$municipality_code,d$species,d$year,sep="|")
  wkey <- function(d) paste(d$municipality_code,d$year,d$window_id,d$scenario_id,sep="|")
  same <- function(a,b,t=tol) (is.na(a)&is.na(b))|(is.finite(a)&is.finite(b)&abs(a-b)<=t)
  water_status <- function(z) {
    label<-rep("COMPLETE",nrow(z))
    label[is.finite(z$etc_total_mm)&z$etc_total_mm<=0]<-"ZERO_DEMAND"
    label[z$unresolved_etr_days>0]<-"UNRESOLVED_STORAGE_HISTORY"
    label[z$missing_forcing_days>0]<-"MISSING_FORCING_IN_WINDOW"
    label
  }
  report<-list(execution_status="INCOMPLETE",script_version=version,
    panel_rows=0L,step20_eligible_rows=0L,all_scenario_complete_candidate_rows=0L,
    retained_common_rows=0L,retained_municipality_species_series=0L,
    minimum_retained_years=min_years,minimum_retained_years_per_parity=min_parity,
    water_caches_verified=0L,reconciled_status_rows_verified=0L,
    planned_fits=56L,completed_fits=0L,primary_covariances_withheld=0L,
    omission_refits_completed=0L,omission_refits_failed=0L,
    numerical_tests_passed=FALSE,same_sample_across_model_families=TRUE,
    sample_is_descriptive_common_geography=FALSE,
    primary_covariance="MET_CERES_year",water_increment="0.10 lower ETr/ETc",
    observed_irrigation=FALSE,observed_local_phenology=FALSE,
    missing_values_imputed=FALSE,weather_downloads=0L,daily_balance_recomputed=FALSE,
    out_of_sample_prediction_tested=FALSE,causal_attribution=FALSE,
    source_inputs_modified=FALSE,plot_status="NOT_ATTEMPTED",last_stage="Startup",error="None")
  coefficients<-diagnostics<-omission<-warnlog<-comparisons<-list()
  warn <- function(expr,label) withCallingHandlers(expr,warning=function(w) {
    warnlog[[length(warnlog)+1L]]<<-data.frame(context=label,message=conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  progress <- function() {
    write_csv(data.frame(check=names(report),result=vapply(report,as.character,character(1))),
      file.path(out_dir,"coffee_seasonal_water_association_summary.csv"))
    items<-list(coefficients="coefficients",diagnostics="model_diagnostics",omission="year_omission",
      warnlog="warnings",comparisons="model_comparison")
    for(n in names(items)) {
      z<-get(n,inherits=TRUE)
      if(length(z))write_csv(bind(z),file.path(out_dir,paste0("coffee_seasonal_water_",items[[n]],".csv")))
    }
  }
  on.exit({try(progress(),silent=TRUE);try(writeLines(capture.output(utils::sessionInfo()),
    file.path(out_dir,"sessionInfo_step24.txt")),silent=TRUE)},add=TRUE)

  # Each rerun gets a preserved copy of earlier outputs, not earlier data.
  if(!test_only) {
    prior<-list.files(out_dir,pattern="^coffee_seasonal_water_.*\\.(csv|txt|rds)$",full.names=TRUE)
    if(length(prior)) {
      root<-file.path(out_dir,"previous_runs");dir.create(root,showWarnings=FALSE)
      backup<-tempfile(paste0("before_",format(Sys.time(),"%Y%m%dT%H%M%S"),"_"),tmpdir=root)
      assert(dir.create(backup),"Cannot preserve prior step24 output.")
      assert(all(file.copy(prior,backup,overwrite=FALSE)),"Could not copy previous step24 output.")
      # Remove only copies of this step's old results after a successful backup.
      unlink(prior)
    }
  }

  formula_for <- function(terms,parity=FALSE) stats::as.formula(paste0(
    "log_yield ~ ",paste(terms,collapse=" + "),
    " | municipality_code[year_centered",if(parity)", odd_year" else "","] + year"))
  fit_one <- function(z,terms,parity,label) {
    f<-warn(fixest::feols(formula_for(terms,parity),data=z,nthreads=1,notes=FALSE,
      demeaned=TRUE,data.save=TRUE,fixef.rm="none",fixef.tol=1e-8,fixef.iter=20000,
      collin.tol=1e-10,vcov="iid"),label)
    assert(stats::nobs(f)==nrow(z),paste(label,"automatically removed rows."))
    assert(setequal(names(stats::coef(f)),terms),paste(label,"dropped a regressor; review collinearity."))
    b<-stats::coef(f);fb<-stats::lm.fit(f$X_demeaned,f$y_demeaned)$coefficients
    assert(all(is.finite(b))&&max(abs(unname(b)-unname(fb)))<1e-6,paste(label,"joint adjustment check failed."))
    assert(isTRUE(f$convStatus)||is.null(f$convStatus),paste(label,"did not converge."))
    f
  }

  worker <- function() {
    if(!requireNamespace("fixest",quietly=TRUE))
      utils::install.packages("fixest",repos="https://cloud.r-project.org")
    assert(requireNamespace("fixest",quietly=TRUE)&&utils::packageVersion("fixest")>="0.13.0",
      "fixest >=0.13.0 is required. Update it, restart R, and rerun.")
    assert("vcov_fix" %in% names(formals(fixest::vcov_cluster)),"Update fixest; vcov_fix argument not available.")
    report$fixest_version<<-as.character(utils::packageVersion("fixest"))
    # Deterministic, artificial numerical fixtures; never exported as study data.
    tests<-list()
    test<-function(label,ok)tests[[length(tests)+1L]]<<-data.frame(test=label,pass=isTRUE(ok))
    d<-expand.grid(municipality_code=as.character(1:18),year=years,stringsAsFactors=FALSE)
    id<-as.numeric(d$municipality_code);d$year_centered<-d$year-2018;d$odd_year<-d$year%%2
    d$x<-sin(0.37*id+0.43*d$year);d$q<-cos(0.13*id-0.17*d$year)
    d$log_yield<-1+0.05*id+0.002*id*d$year_centered+0.01*id*d$odd_year+
      0.2*d$x-0.15*d$q+sin(d$year)*0.1+0.02*cos(id*d$year)
    for(parity in c(FALSE,TRUE)) {
      f<-fit_one(d,c("x","q"),parity,"Numerical FE test")
      explicit<-stats::as.formula(paste0("log_yield ~ x+q+factor(municipality_code)+factor(year)",
        "+factor(municipality_code):year_centered",if(parity)"+factor(municipality_code):odd_year" else ""))
      lmfit<-stats::lm(explicit,d)
      test(paste("Joint nuisance coefficients match dummy lm; parity",parity),
        max(abs(stats::coef(f)-stats::coef(lmfit)[names(stats::coef(f))]))<1e-6)
    }
    basic<-fixest::feols(log_yield~x+q,d,notes=FALSE)
    X<-stats::model.matrix(~x+q,d);score<-X*as.numeric(stats::residuals(basic));bread<-solve(crossprod(X))
    a<-as.character(ceiling(id/2));b<-as.character((id%%5));c<-as.character(d$year)
    vm<-function(g)bread%*%crossprod(rowsum(score,g))%*%bread
    direct<-vm(a)+vm(b)+vm(c)-vm(paste(a,b))-vm(paste(a,c))-vm(paste(b,c))+vm(paste(a,b,c))
    lib<-warn(fixest::vcov_cluster(basic,cluster=list(a,b,c),
      ssc=fixest::ssc(K.adj=FALSE,G.adj=FALSE),vcov_fix=FALSE),"Numerical multiway test")
    test("Three-way covariance equals inclusion-exclusion sandwich",max(abs(direct-lib))<1e-8)
    test("Water predictor means 0.10 lower satisfaction",abs(((1-0.6)/0.1-(1-0.7)/0.1)-1)<1e-12)
    test("Multiplicative contrast",abs(100*expm1(log(1.1))-10)<1e-10)
    fx<-data.frame(missing_forcing_days=c(0,0,0,1),unresolved_etr_days=c(1,0,0,0),
      etc_total_mm=c(100,100,0,NA_real_))
    test("Status classified per scenario",identical(water_status(fx),
      c("UNRESOLVED_STORAGE_HISTORY","COMPLETE","ZERO_DEMAND","MISSING_FORCING_IN_WINDOW")))
    test("Missing ratios are not imputed",is.na((1-NA_real_)/0.1))
    test("Composite keys distinguish species and missing years",!anyDuplicated(pkey(data.frame(
      municipality_code=c("1100015","1100015","1100015"),species=c("Arabica","Canephora","Arabica"),year=c(2012,2012,2014)))))
    write_csv(bind(tests),file.path(out_dir,"coffee_seasonal_water_numerical_tests.csv"))
    assert(all(bind(tests)$pass),"A numerical test failed before study estimation.")
    report$numerical_tests_passed<<-TRUE
    if(test_only) {report$execution_status<<-"NUMERICAL_TESTS_ONLY";message("All step24 numerical tests passed.");return(invisible(NULL))}

    # 1. Exact source contracts and unchanged provenance.
    syn<-file.path(base_dir,"04_outputs","water_stress_synthesis")
    bal<-file.path(base_dir,"04_outputs","daily_water_balance")
    paths<-c(summary23=file.path(syn,"coffee_water_stress_summary.csv"),
      provenance23=file.path(syn,"coffee_water_stress_input_provenance.csv"),
      eligibility=file.path(syn,"coffee_water_stress_record_eligibility.csv"),
      reconciliation=file.path(syn,"coffee_water_stress_status_reconciliation.csv"),
      cache_verification=file.path(syn,"coffee_water_stress_cache_verification.csv"),
      cache_index=file.path(bal,"coffee_water_balance_cache_index.csv"),
      scenarios=file.path(bal,"coffee_water_balance_scenarios.csv"),
      municipalities=file.path(bal,"coffee_water_balance_municipality_index.csv"))
    assert(all(file.exists(paths)),paste("Required step22/23 input missing:",paste(paths[!file.exists(paths)],collapse="\n")))
    s23<-read_csv(paths[["summary23"]]);assert(check(s23,"execution_status")=="COMPLETED_WATER_STRESS_SYNTHESIS","Run the completed step23 fixed script first.")
    assert(check(s23,"numerical_values_replaced")=="FALSE"&&check(s23,"numerical_tests_passed")=="TRUE", "Step23 source/test contract failed.")
    prov23<-read_csv(paths[["provenance23"]]);need(prov23,c("role","file","md5"),"Step23 provenance")
    assert(!anyDuplicated(prov23$role),"Ambiguous step23 source roles.")
    for(i in seq_len(nrow(prov23))) {
      p<-local_path(prov23$file[i]);assert(file.exists(p)&&hash(p)==prov23$md5[i],paste("Upstream input changed:",prov23$role[i]))
    }
    for(role in c("panel","windows")) {
      pos<-which(prov23$role==role);assert(length(pos)==1L,paste("Missing source role:",role))
      paths[[role]]<-local_path(prov23$file[pos])
    }
    provenance<-data.frame(role=names(paths),file=unname(paths),md5=vapply(paths,hash,character(1)))
    write_csv(provenance,file.path(out_dir,"coffee_seasonal_water_input_provenance.csv"))
    sc<-read_csv(paths[["scenarios"]]);need(sc,c("scenario_id","cad_mm","kc","central"),"Scenarios")
    assert(nrow(sc)==12L&&!anyDuplicated(sc$scenario_id)&&sum(bool(sc$central,"central"))==1L&&
      sc$scenario_id[bool(sc$central,"central")]==central,"Expected the original 12 scenarios and central configuration.")
    sc$cad_mm<-num(sc$cad_mm,"CAD",FALSE);sc$kc<-num(sc$kc,"Kc",FALSE)
    win<-read_csv(paths[["windows"]]);need(win,c("year","window_id","start","end","expected_days"),"Window dates")
    assert(nrow(win)==52L&&!anyDuplicated(paste(win$year,win$window_id)),"Unexpected window dates.")
    d<-as.data.frame(readRDS(paths[["panel"]]),stringsAsFactors=FALSE)
    need(d,c("municipality_code","municipality","state","species","year","MET_cell_id","log_yield",
      "analysis_yield_kg_ha","production_t","area_harvested_ha","included_step20"),"Step21 panel")
    d$municipality_code<-as.character(d$municipality_code);d$species<-as.character(d$species)
    d$year<-as.integer(num(d$year,"year",FALSE));d$included_step20<-bool(d$included_step20,"included_step20")
    assert(!anyDuplicated(pkey(d))&&setequal(d$species,species_set)&&setequal(d$year,years),"Invalid panel keys/species/years.")
    elig<-read_csv(paths[["eligibility"]]);need(elig,c("municipality_code","species","year","included_step20",
      "central_all_windows_complete","all_scenarios_all_windows_complete"),"Step23 record eligibility")
    assert(!anyDuplicated(pkey(elig))&&setequal(pkey(elig),pkey(d)),"Eligibility keys differ from panel.")
    elig<-elig[match(pkey(d),pkey(elig)),]
    assert(identical(d$included_step20,bool(elig$included_step20,"inherited eligibility")),"Stage20 inclusion changed.")
    met<-read_csv(paths[["municipalities"]]);need(met,c("municipality_code","MET_cell_id","CERES_cell_id"),"Source-cell index")
    assert(!anyDuplicated(met$municipality_code),"Repeated municipality index keys.")
    im<-match(d$municipality_code,met$municipality_code)
    assert(!anyNA(im)&&all(d$MET_cell_id==met$MET_cell_id[im]),"Municipality MET source mismatch.")
    d$CERES_cell_id<-met$CERES_cell_id[im]
    for(w in windows) {
      cols<-paste0("season_",w,c("_hot_days_ge32","_precipitation_total_mm","_start","_end","_expected_days"))
      need(d,cols,paste("Seasonal panel",w))
      j<-match(paste(d$year,w),paste(win$year,win$window_id))
      assert(!anyNA(j)&&all(as.character(d[[cols[3]]])==win$start[j])&&all(as.character(d[[cols[4]]])==win$end[j]),"Seasonal dates changed.")
      d[[paste0("heat_",w)]]<-num(d[[cols[1]]],cols[1])/10
      d[[paste0("rain_",w)]]<-num(d[[cols[2]]],cols[2])/100
    }
    ccols<-c(paste0("heat_",windows),paste0("rain_",windows))
    assert(all(stats::complete.cases(d[d$included_step20,ccols,drop=FALSE])),"New seasonal weather gaps; no sample silently removed.")
    report$panel_rows<<-nrow(d);report$step20_eligible_rows<<-sum(d$included_step20)

    # 2. Read SUMMARY caches only, and verify per-scenario status audit.
    ci<-read_csv(paths[["cache_index"]]);cv<-read_csv(paths[["cache_verification"]])
    need(ci,c("municipality_code","cache_file","cache_md5"),"Cache index")
    need(cv,c("municipality_code","md5","numerical_values_modified","source_cache_modified"),"Step23 cache audit")
    assert(!anyDuplicated(ci$municipality_code)&&!anyDuplicated(cv$municipality_code)&&
      setequal(ci$municipality_code,unique(d$municipality_code))&&setequal(ci$municipality_code,cv$municipality_code),"Source cache coverage mismatch.")
    cv<-cv[match(ci$municipality_code,cv$municipality_code),]
    assert(all(ci$cache_md5==cv$md5)&&!any(bool(cv$numerical_values_modified,"cache values modified"))&&
      !any(bool(cv$source_cache_modified,"cache modified")),"Source caches differ from step23 audited versions.")
    rec<-read_csv(paths[["reconciliation"]]);need(rec,c("municipality_code","year","window_id","scenario_id", "original_status","verified_status","source_cache_md5"),"Status audit")
    assert(!anyDuplicated(wkey(rec)),"Repeated reconciliation keys.")
    nrec<-0L;allcache<-vector("list",nrow(ci))
    wide_columns<-unlist(lapply(sc$scenario_id,function(s)paste(s,windows,sep="__")),use.names=FALSE)
    for(i in seq_len(nrow(ci))) {
      code<-ci$municipality_code[i]
      if(i==1L||i%%100L==0L||i==nrow(ci))message(sprintf("[%d/%d] Reading water summaries",i,nrow(ci)))
      p<-local_path(ci$cache_file[i]);assert(file.exists(p)&&hash(p)==ci$cache_md5[i],paste("Water cache changed:",code))
      obj<-readRDS(p);z<-as.data.frame(obj$seasonal,stringsAsFactors=FALSE)
      need(z,c("municipality_code","year","window_id","scenario_id","cad_mm","kc","status","period_start","period_end",
        "isna_ratio","etc_total_mm","etr_total_mm","deficit_total_mm","missing_forcing_days","unresolved_etr_days"),paste("Cache",code))
      z$municipality_code<-as.character(z$municipality_code)
      assert(nrow(z)==624L&&all(z$municipality_code==code)&&!anyDuplicated(wkey(z)),paste("Unexpected cache keys:",code))
      j<-match(paste(z$year,z$window_id),paste(win$year,win$window_id));s<-match(z$scenario_id,sc$scenario_id)
      assert(!anyNA(j)&&!anyNA(s)&&all(as.character(z$period_start)==win$start[j])&&all(as.character(z$period_end)==win$end[j])&&
        all(abs(z$cad_mm-sc$cad_mm[s])<tol)&&all(abs(z$kc-sc$kc[s])<tol),paste("Cache window/scenario mismatch:",code))
      forcing<-z$missing_forcing_days==0L;flow<-forcing&z$unresolved_etr_days==0L
      complete<-flow&is.finite(z$etc_total_mm)&z$etc_total_mm>0
      assert(!anyNA(complete)&&all(is.finite(z$isna_ratio)==complete),paste("Ratio support disagrees with counters:",code))
      assert(all(same(z$isna_ratio[complete],z$etr_total_mm[complete]/z$etc_total_mm[complete]))&&
        all(same(z$deficit_total_mm[complete],z$etc_total_mm[complete]-z$etr_total_mm[complete]))&&
        all(z$isna_ratio[complete]>=-tol&z$isna_ratio[complete]<=1+tol),paste("Water identity failed:",code))
      status_verified<-water_status(z);rc<-rec[rec$municipality_code==code,,drop=FALSE]
      changed<-which(as.character(z$status)!=status_verified)
      assert(setequal(wkey(z)[changed],wkey(rc)),paste("Status differences no longer match completed step23 audit:",code))
      if(nrow(rc)) {
        k<-match(wkey(rc),wkey(z));assert(!anyNA(k)&&all(rc$source_cache_md5==ci$cache_md5[i])&&
          all(rc$original_status==z$status[k])&&all(rc$verified_status==status_verified[k]),paste("Status provenance failed:",code))
      }
      nrec<-nrec+nrow(rc)
      m<-matrix(NA_real_,nrow=13L,ncol=48L,dimnames=list(as.character(years),wide_columns))
      ir<-match(as.character(z$year),rownames(m));ic<-match(paste(z$scenario_id,z$window_id,sep="__"),colnames(m))
      assert(!anyNA(ir)&&!anyNA(ic)&&!anyDuplicated(paste(ir,ic)),paste("Incomplete scenario/year matrix:",code))
      m[cbind(ir,ic)]<-z$isna_ratio
      one<-data.frame(municipality_code=code,year=years,stringsAsFactors=FALSE)
      one<-cbind(one,as.data.frame(m,check.names=FALSE))
      allcache[[i]]<-one;report$water_caches_verified<<-i
    }
    assert(nrec==nrow(rec),"Reconciliation audit contains unverified rows.")
    report$reconciled_status_rows_verified<<-nrec
    water<-bind(allcache);rm(allcache);invisible(gc(FALSE))
    k<-match(paste(d$municipality_code,d$year),paste(water$municipality_code,water$year))
    assert(!anyNA(k),"Missing municipality/year water summary.")
    ratios<-water[k,wide_columns,drop=FALSE]
    complete_all<-stats::complete.cases(ratios)
    complete_central<-stats::complete.cases(ratios[paste(central,windows,sep="__")])
    assert(identical(unname(complete_all),unname(bool(elig$all_scenarios_all_windows_complete,"all scenario support")))&&
      identical(unname(complete_central),unname(bool(elig$central_all_windows_complete,"central support"))),"Recomputed water support differs from step23.")
    candidate<-d$included_step20&complete_all
    report$all_scenario_complete_candidate_rows<<-sum(candidate)
    # Reapply the EXISTING time-support thresholds to water-complete records.
    series_key<-paste(d$municipality_code,d$species,sep="|")
    sg<-split(which(candidate),series_key[candidate]);series_report<-list()
    for(n in names(sg)) {
      q<-sg[[n]];ys<-d$year[q];odd<-sum(ys%%2L==1L);even<-sum(ys%%2L==0L)
      assert(!anyDuplicated(ys),"Repeated years within municipal species.")
      X<-cbind(1,ys-2018,ys%%2L)
      retained<-length(q)>=min_years&&odd>=min_parity&&even>=min_parity&&qr(X)$rank==3L
      series_report[[n]]<-data.frame(series_key=n,municipality_code=d$municipality_code[q[1]],species=d$species[q[1]],
        water_complete_years=length(q),odd_years=odd,even_years=even,retained=retained,
        rule="Reapply >=10 years and >=4 per parity AFTER water completeness, without outcome/sign screening")
    }
    sr<-bind(series_report);d$included_step24<-candidate&series_key %in% sr$series_key[sr$retained]
    membership<-d[c("municipality_code","municipality","species","year","included_step20")]
    membership$water_complete_all_scenarios<-complete_all
    membership$adequate_retained_time_support<-series_key %in% sr$series_key[sr$retained]
    membership$included_step24<-d$included_step24
    write_csv(sr,file.path(out_dir,"coffee_seasonal_water_series_support.csv"))
    write_csv(membership,file.path(out_dir,"coffee_seasonal_water_sample_membership.csv"))
    coverage<-list()
    for(sp in species_set)for(yr in years) {
      orig<-d$species==sp&d$year==yr&d$included_step20;sel<-d$species==sp&d$year==yr&d$included_step24
      p<-num(d$production_t,"production");a<-num(d$area_harvested_ha,"area")
      coverage[[length(coverage)+1L]]<-data.frame(species=sp,year=yr,step20_rows=sum(orig),
        water_complete_candidate_rows=sum(orig&candidate),retained_rows=sum(sel),
        retained_production_pct_of_step20=100*sum(p[sel])/sum(p[orig]),
        retained_area_pct_of_step20=100*sum(a[sel])/sum(a[orig]))
    }
    write_csv(bind(coverage),file.path(out_dir,"coffee_seasonal_water_sample_coverage.csv"))
    keep<-which(d$included_step24)
    zall<-d[keep,c("municipality_code","municipality","state","species","year","MET_cell_id","CERES_cell_id",
      "log_yield","analysis_yield_kg_ha",ccols),drop=FALSE]
    zall<-cbind(zall,ratios[keep,,drop=FALSE]);rownames(zall)<-NULL
    zall$log_yield<-num(zall$log_yield,"log productivity",FALSE)
    assert(all(is.finite(zall$analysis_yield_kg_ha)&zall$analysis_yield_kg_ha>0)&&
      max(abs(zall$log_yield-log(zall$analysis_yield_kg_ha)))<1e-10,"Observed response changed.")
    zall$year_centered<-zall$year-2018;zall$odd_year<-zall$year%%2L
    report$retained_common_rows<<-nrow(zall);report$retained_municipality_species_series<<-nrow(sr[sr$retained,,drop=FALSE])
    write_rds(zall,file.path(out_dir,"coffee_seasonal_water_analysis_input.rds"))
    rm(d,ratios,water);invisible(gc(FALSE))

    # 3. Prespecified fits on the same records, never on aggregate exposure.
    specs<-list()
    for(parity in c(FALSE,TRUE)) {
      control<-if(parity)"Trend_parity_year" else "Trend_year"
      for(family in c("H","P","W"))for(sid in if(family=="W")sc$scenario_id else "NOT_APPLICABLE") {
        terms<-paste0("heat_",windows)
        if(family=="P")terms<-c(terms,paste0("rain_",windows))
        if(family=="W")terms<-c(terms,paste0("water_",windows))
        specs[[length(specs)+1L]]<-data.frame(model_id=paste(control,family,sid,sep="__"),
          control=control,parity=parity,family=family,scenario_id=sid,
          formula=paste(deparse(formula_for(terms,parity)),collapse=" "),
          terms=paste(terms,collapse="|"),includes_W4_current_PAM_year=TRUE)
      }
    }
    specs<-bind(specs);report$planned_fits<<-nrow(specs)*length(species_set)
    write_csv(specs,file.path(out_dir,"coffee_seasonal_water_specifications.csv"))
    ssc<-fixest::ssc(K.adj=TRUE,K.fixef="nonnested",G.adj=TRUE,G.df="min",t.df="min")
    report$last_stage<<-"Fitting prespecified seasonal comparisons"
    progress()
    main_store<-list();fit_dir<-file.path(out_dir,"compact_fits");dir.create(fit_dir,showWarnings=FALSE)
    term_table <- function(f,V,z,sp,covariance) {
      b<-stats::coef(f);V<-V[names(b),names(b),drop=FALSE]
      validmatrix<-all(is.finite(V));emin<-NA_real_;psd<-FALSE
      if(validmatrix) {
        ee<-eigen((V+t(V))/2,symmetric=TRUE,only.values=TRUE)$values
        emin<-min(ee);psd<-emin>=-1e-10*max(max(abs(ee)),.Machine$double.eps)
      }
      G<-c(length(unique(z$MET_cell_id)),length(unique(z$year)))
      if(covariance=="MET_CERES_year")G<-c(G,length(unique(z$CERES_cell_id)))
      df<-min(G)-1L;tcrit<-stats::qt(0.975,df)
      out<-lapply(names(b),function(term) {
        vari<-V[term,term];ok<-psd&&is.finite(vari)&&vari>0&&df>1
        se<-if(ok)sqrt(vari) else NA_real_;kind<-sub("_W[1-4]$","",term)
        inc<-switch(kind,heat="10 additional Tmax>=32 C days in this window",rain="100 mm additional precipitation in this window",
          water="0.10 lower seasonal ETr/ETc (10 percentage points)")
        data.frame(species=z$species[1L],model_id=sp$model_id,control=sp$control,family=sp$family,
          scenario_id=sp$scenario_id,term=term,window_id=sub(".*_(W[1-4])$","\\1",term),predictor=kind,
          increment=inc,beta_log=unname(b[term]),se_log=se,association_pct=100*expm1(unname(b[term])),
          lower95_pct=if(ok)100*expm1(b[term]-tcrit*se) else NA_real_,
          upper95_pct=if(ok)100*expm1(b[term]+tcrit*se) else NA_real_,covariance=covariance,
          covariance_psd=psd,minimum_covariance_eigenvalue=emin,df_t=df,
          ci_status=if(ok)"EXPLORATORY" else "WITHHELD_INVALID_COVARIANCE_OR_VARIANCE",
          n_obs=nrow(z),municipalities=length(unique(z$municipality_code)),MET_cells=length(unique(z$MET_cell_id)),
          CERES_cells=length(unique(z$CERES_cell_id)),year_clusters=length(unique(z$year)),
          windows_are_verified_phenology=FALSE,causal_effect=FALSE,predictive_validation=FALSE,stringsAsFactors=FALSE)
      });bind(out)
    }
    for(species in species_set) {
      z<-zall[zall$species==species,,drop=FALSE]
      assert(length(unique(z$year))==13L,"Reporting-year coverage reduced; review support.")
      for(j in seq_len(nrow(specs))) {
        sp<-specs[j,,drop=FALSE];label<-paste(species,sp$model_id)
        if(sp$family=="W")for(w in windows)z[[paste0("water_",w)]]<-(1-z[[paste(sp$scenario_id,w,sep="__")]])/0.1
        terms<-strsplit(sp$terms,"|",fixed=TRUE)[[1L]]
        message("Fitting ",report$completed_fits+1L,"/",report$planned_fits,": ",label)
        f<-fit_one(z,terms,sp$parity,label)
        V3<-warn(fixest::vcov_cluster(f,cluster=list(MET=z$MET_cell_id,CERES=z$CERES_cell_id,year=z$year),
          ssc=ssc,vcov_fix=FALSE),paste(label,"MET/CERES/year covariance"))
        V2<-warn(fixest::vcov_cluster(f,cluster=list(MET=z$MET_cell_id,year=z$year),
          ssc=ssc,vcov_fix=FALSE),paste(label,"MET/year sensitivity covariance"))
        c3<-term_table(f,V3,z,sp,"MET_CERES_year");c2<-term_table(f,V2,z,sp,"MET_year_sensitivity")
        coefficients[[length(coefficients)+1L]]<<-rbind(c3,c2)
        if(!all(c3$covariance_psd))report$primary_covariances_withheld<<-report$primary_covariances_withheld+1L
        ssr<-sum(stats::residuals(f)^2);denom<-sum(f$y_demeaned^2);corx<-stats::cor(f$X_demeaned)
        ev<-eigen(corx,symmetric=TRUE,only.values=TRUE)$values
        diagnostics[[length(diagnostics)+1L]]<<-data.frame(species=species,model_id=sp$model_id,control=sp$control,
          family=sp$family,scenario_id=sp$scenario_id,n_obs=nrow(z),n_climate_parameters=length(terms),
          residual_sum_squares_log=ssr,nuisance_residual_sum_squares_log=denom,
          partial_R2_beyond_nuisance=1-ssr/denom,residual_RMSE_log=sqrt(ssr/nrow(z)),
          residualized_condition_index=if(min(ev)>0)sqrt(max(ev)/min(ev)) else Inf,
          maximum_absolute_regressor_correlation=max(abs(corx[upper.tri(corx)])),
          primary_covariance_psd=all(c3$covariance_psd),out_of_sample=FALSE)
        write_rds(list(coefficient=stats::coef(f),covariance_MET_CERES_year=V3,covariance_MET_year=V2,
          formula=sp$formula,specification=sp,sample_keys=pkey(z),input_provenance=provenance),
          file.path(fit_dir,paste0(species,"_",sp$model_id,".rds")))
        if(sp$family=="P"||(sp$family=="W"&&sp$scenario_id==central))
          main_store[[paste(species,sp$model_id,sep="|")]]<-list(species=species,spec=sp,beta=stats::coef(f))
        report$completed_fits<<-report$completed_fits+1L;progress()
      }
    }
    dg<-bind(diagnostics)
    for(i in seq_len(nrow(dg))) {
      r<-dg[i,,drop=FALSE];h<-dg[dg$species==r$species&dg$control==r$control&dg$family=="H",,drop=FALSE]
      p<-dg[dg$species==r$species&dg$control==r$control&dg$family=="P",,drop=FALSE]
      assert(nrow(h)==1L&&nrow(p)==1L&&h$n_obs==r$n_obs&&p$n_obs==r$n_obs,"Model-comparison sample mismatch.")
      assert(abs(r$nuisance_residual_sum_squares_log-h$nuisance_residual_sum_squares_log)<1e-6,
        "Nuisance response differs across model families.")
      comparisons[[i]]<<-data.frame(species=r$species,model_id=r$model_id,control=r$control,family=r$family,
        scenario_id=r$scenario_id,n_obs=r$n_obs,n_climate_parameters=r$n_climate_parameters,
        partial_R2_beyond_nuisance=r$partial_R2_beyond_nuisance,
        partial_R2_beyond_heat_reference=1-r$residual_sum_squares_log/h$residual_sum_squares_log,
        difference_RMSE_log_minus_rain=r$residual_RMSE_log-p$residual_RMSE_log,
        difference_partial_R2_minus_rain=r$partial_R2_beyond_nuisance-p$partial_R2_beyond_nuisance,
        interpretation="In-sample fit on identical records; not validation of climate importance or predictive skill")
    }
    progress()

    # 4. Omit a whole reporting year, refitting every nuisance coefficient.
    if(do_omission)for(entry in main_store) {
      sp<-entry$spec;z<-zall[zall$species==entry$species,,drop=FALSE]
      if(sp$family=="W")for(w in windows)z[[paste0("water_",w)]]<-(1-z[[paste(central,w,sep="__")]])/0.1
      terms<-strsplit(sp$terms,"|",fixed=TRUE)[[1L]]
      for(y in years) {
        label<-paste(entry$species,sp$model_id,"omit",y);message("Stability: ",label)
        ans<-tryCatch(fit_one(z[z$year!=y,,drop=FALSE],terms,sp$parity,label),error=function(e)e)
        if(inherits(ans,"error")) {
          report$omission_refits_failed<<-report$omission_refits_failed+1L
          rows<-data.frame(species=entry$species,model_id=sp$model_id,omitted_year=y,term=terms,
            beta_log=NA_real_,full_beta_log=unname(entry$beta[terms]),status="FAILED",error=conditionMessage(ans))
        } else {
          report$omission_refits_completed<<-report$omission_refits_completed+1L
          rows<-data.frame(species=entry$species,model_id=sp$model_id,omitted_year=y,term=terms,
            beta_log=unname(stats::coef(ans)[terms]),full_beta_log=unname(entry$beta[terms]),status="SUCCESS",error="")
        }
        omission[[length(omission)+1L]]<<-rows
      }
      progress()
    }
    if(length(omission)) {
      o<-bind(omission);gr<-split(o,paste(o$species,o$model_id,o$term,sep="|"))
      stable<-lapply(gr,function(q) {
        ok<-q$status=="SUCCESS"&is.finite(q$beta_log)
        data.frame(species=q$species[1],model_id=q$model_id[1],term=q$term[1],successful_omissions=sum(ok),
          minimum_beta_log=if(any(ok))min(q$beta_log[ok]) else NA_real_,
          maximum_beta_log=if(any(ok))max(q$beta_log[ok]) else NA_real_,
          sign_preserved_all_13=if(sum(ok)==13L)all(sign(q$beta_log[ok])==sign(q$full_beta_log[1])) else NA,
          interpretation="Coefficient stability, not held-out prediction or a new confidence interval")
      });write_csv(bind(stable),file.path(out_dir,"coffee_seasonal_water_stability_summary.csv"))
    }
    co<-bind(coefficients);wco<-co[co$family=="W"&co$covariance=="MET_CERES_year",,drop=FALSE]
    sens<-lapply(split(wco,paste(wco$species,wco$control,wco$term,sep="|")),function(q) {
      cent<-q[q$scenario_id==central,,drop=FALSE]
      assert(nrow(q)==12L&&nrow(cent)==1L,"Incomplete scenario coefficient envelope.")
      data.frame(species=q$species[1],control=q$control[1],term=q$term[1],predictor=q$predictor[1],window_id=q$window_id[1],
        scenario_count=nrow(q),central_association_pct=cent$association_pct,
        minimum_association_pct=min(q$association_pct),maximum_association_pct=max(q$association_pct),
        negative_scenarios=sum(q$beta_log<0),positive_scenarios=sum(q$beta_log>0),
        interval_type="Parameter sensitivity only, NOT a statistical confidence interval; no best scenario selected")
    })
    write_csv(bind(sens),file.path(out_dir,"coffee_seasonal_water_scenario_sensitivity.csv"))

    # 5. Optional diagnostic plot. Tables survive any graphics failure.
    if(make_plot) {
      report$plot_status<<-tryCatch({
        assert(requireNamespace("ggplot2",quietly=TRUE),"ggplot2 not installed; estimates are already saved.")
        plt<-co[co$covariance=="MET_CERES_year"&((co$family=="P"&co$predictor=="rain")|
          (co$family=="W"&co$scenario_id==central&co$predictor=="water")),,drop=FALSE]
        plt$species<-factor(plt$species,levels=species_set)
        plt$indicator<-factor(ifelse(plt$predictor=="rain","Precipitation: +100 mm","Water satisfaction: -0.10"),
          levels=c("Precipitation: +100 mm","Water satisfaction: -0.10"))
        plt$control_label<-factor(ifelse(plt$control=="Trend_year","Trends and year effects","Trends, parity and year effects"))
        plt$position<-match(plt$window_id,rev(windows))+ifelse(plt$control=="Trend_year",-0.12,0.12)
        ciok<-plt[is.finite(plt$lower95_pct)&is.finite(plt$upper95_pct),,drop=FALSE]
        family<-"serif"
        if(requireNamespace("systemfonts",quietly=TRUE)&&"Times New Roman" %in% systemfonts::system_fonts()$family)family<-"Times New Roman"
        theme_rogerio<-function()ggplot2::theme_minimal(base_size=14,base_family=family)+ggplot2::theme(
          panel.grid=ggplot2::element_blank(),axis.line=ggplot2::element_line(colour="black"),
          axis.ticks=ggplot2::element_line(colour="black"),axis.text=ggplot2::element_text(colour="black"),
          legend.position="top",legend.title=ggplot2::element_blank(),strip.text=ggplot2::element_text(face="bold"),
          panel.spacing=grid::unit(0.8,"lines"),plot.title=ggplot2::element_text(face="bold"),plot.subtitle=ggplot2::element_text(size=11))
        facet<-if("axes" %in% names(formals(ggplot2::facet_grid)))ggplot2::facet_grid(species~indicator,axes="all",axis.labels="all") else ggplot2::facet_grid(species~indicator)
        p<-ggplot2::ggplot(plt,ggplot2::aes(x=association_pct,y=position,shape=control_label))+
          ggplot2::geom_vline(xintercept=0,linetype="dotted")+
          ggplot2::geom_segment(data=ciok,ggplot2::aes(x=lower95_pct,xend=upper95_pct,yend=position),linewidth=0.5)+
          ggplot2::geom_point(size=2.5)+facet+ggplot2::scale_y_continuous(breaks=1:4,labels=rev(windows),limits=c(0.5,4.5))+
          ggplot2::labs(x="Conditional productivity association (%)",y=NULL,
            subtitle="Same sample. All four windows fitted together. Water: CAD 100 mm, Kc 0.95, rainfed.",
            caption="Exploratory 95% intervals clustered by MET cell, CERES cell and year. Missing intervals are withheld, not zero.")+theme_rogerio()
        png<-file.path(fig_dir,"Figure04_seasonal_water_diagnostic.png")
        if(requireNamespace("ragg",quietly=TRUE))ggplot2::ggsave(png,p,width=12,height=8,dpi=300,device=ragg::agg_png)
        else ggplot2::ggsave(png,p,width=12,height=8,dpi=300)
        write_csv(plt,file.path(out_dir,"coffee_seasonal_water_plot_data.csv"))
        print(p);"PNG_SAVED"
      },error=function(e){warnlog[[length(warnlog)+1L]]<<-data.frame(context="Plot",message=conditionMessage(e));"PLOT_FAILED_TABLES_SAVED"})
    } else report$plot_status<<-"SKIPPED_BY_SETTING"
    notes<-c("COFFE-STUDY | STEP24 | SEASONAL HYDROCLIMATE ASSOCIATIONS",paste("Executed",format(Sys.time(),usetz=TRUE)),
      "H=heat; P=heat+precipitation; W=heat+water limitation, all four windows jointly.",
      "Water limitation=(1-local seasonal ETr/ETc)/0.10; coefficient unit means 10 percentage points LOWER satisfaction.",
      "Rain +100 mm; heat +10 Tmax>=32 C days. No threshold is a validated species injury limit.",
      "All scenarios and families share exactly the same response records; no significance selection.",
      "Source eligibility is step20 plus complete all-scenario window ISNA, followed by reapplication of >=10 years and >=4 per parity.",
      "The time-support check is performed after water gaps. Excluded series remain in the membership report.",
      "Descriptive common-geography support is not imposed on regression records.",
      "Two joint nuisance sets: municipal intercepts + municipal trends + year effects; alternative additionally municipal parity.",
      "Equal included record weights, not coffee-area or production weights.",
      "Primary covariance clusters separately on MET, CERES and reporting year. MET/year is only a labelled sensitivity.",
      "13 year clusters: asymptotic multiway intervals remain exploratory. No automatic PSD repair, no substitution of invalid intervals.",
      "No covariance choice fully represents all distance-dependent spatial/time dependence.",
      "Partial R2 and RMSE compare in-sample fit. They do not quantify prediction skill or total climate importance.",
      "Year omission refits all coefficients/nuisance controls; it is not a forecasting exercise.",
      "CAD/Kc envelope is sensitivity, not confidence or adaptation effectiveness. Scenarios have no observed irrigation.",
      "W1-W4 keep step21 dates. Species-specific local phenology/harvest is unverified; W4 can overlap harvest.",
      "Water balance is continuous; summary reuse preserves memory and exclusions inherited from step22.",
      "Step23 status reconciliation is verified from archived labels/counters/hashes. No upstream field is changed.",
      "No source downloads, daily recalculation, causal loss estimate, station validation, or climate-change attribution.",
      "https://lrberge.github.io/fixest/reference/feols.html", "https://lrberge.github.io/fixest/reference/vcov_cluster.html",
      "https://lrberge.github.io/fixest/reference/ssc.html")
    writeLines(notes,file.path(out_dir,"coffee_seasonal_water_method_notes.txt"))
    assert(all(vapply(provenance$file,hash,character(1))==provenance$md5),"An input changed during step24.")
    report$execution_status<<-if(report$omission_refits_failed>0L||report$primary_covariances_withheld>0L)
      "COMPLETED_SEASONAL_ASSOCIATIONS_WITH_CAVEATS" else "COMPLETED_SEASONAL_ASSOCIATIONS"
    report$last_stage<<-"All prespecified seasonal comparisons exported; no causal or predictive claim."
    progress();message("\n",report$execution_status,"\nOutputs: ",out_dir)
  }
  tryCatch(worker(),error=function(e){report$error<<-conditionMessage(e);try(progress(),silent=TRUE)
    stop(conditionMessage(e),"\nPrevious data remain unchanged. See coffee_seasonal_water_association_summary.csv.",call.=FALSE)},
    interrupt=function(e){report$execution_status<<-"INTERRUPTED";report$error<<-"Interrupted; tables up to last completed fit retained."
      try(progress(),silent=TRUE);message("Interrupted. Source data were not modified.");invisible(NULL)})
}

coffee_step24()

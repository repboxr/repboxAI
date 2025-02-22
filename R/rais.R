# rais is a collection of similar rai
# that are called for different rows of an input data set
rais_init = function(li, prod=NULL, to_r0=TRUE, df=NULL) {
  restore.point("rais_init")
  if (is(li,"repbox_rai")) li = list(li)
  rai = li[[1]]
  rais = list(rai=rai, li=li, nrow = max(length(li), NROW(df)), to_r0=to_r0, prod=prod, issues=list())
  fields = c("project_dir","version","pid","json_mode")
  rais[fields] = rai[fields]
  if (is.null(df)) {
    rais$df = tibble(rai_ind = seq_len(rais$nrow))  
  } else {
    rais$df = add_col_left(df, rai_ind = seq_len(rais$nrow))
  }
  version = rais$version
  
  project_dir = rais$project_dir
  version_dir = file.path(project_dir, "rai", "prod_runs", version$pid, version$vid)
  if (to_r0) {
    run_dir = file.path(version_dir, "r0")
  } else {
    run_dirs = list.dirs(version_dir, full.names=FALSE, recursive=TRUE)
    if (length(run_dirs)==0) {
      run_dir = file.path(version_dir, "r1")
    } else {
      run_nums = as.integer(substr(run_dirs, 2))
      max_run = max(run_nums)
      run_dir = file.path(version_dir, paste0("r", max_run+1))
    }
  }
  rais$version_dir = version_dir
  rais$run_dir = run_dir
  class(rais) = c("repbox_rais","list")
  rais
  
}

rais_add_issue = function(rais, type,details="") {
  issue = tibble(type=type, details=details) 
  rais$issues = c(rais$issues, list(issue))
  rais
}

rais_combine_content = function(rais,var=NULL,na_val=NA_character_, content_schema=rais[["content_schema"]]) {
  if (is.null(var) & is.null(content_schema)) {
    stop("You must provide a variable name or a content_schema")
  }
  if (!rais$json_mode & is.null(var)) {
    stop("For content not in json mode you must specify a variable var.")
  }
  
  
  if (!is.null(var)) {
    content = lapply(rais$li, function(rai) {
      cont = rai$content
      if (is(cont, "try-error")) return(na_val)
      cont
    })
    rais$df[[var]] = content
  } else {
    stop("not yet implemented for content_schema")
  }
  rais
}

rais_rai_stats = function(rais, li=rais$li) {
  restore.point("rais_stats")
  i = 2
  rai_stats = bind_rows(lapply(seq_along(li), function(i) {
    #cat("\n",i)
    rai = li[[i]]
    httr_resp = rai$httr_resp
    num_rows = NROW(rai$content)
    res_li = list(
      rai_ind = i,
      time_stamp = rai$time_stamp,
      run_sec = rai$run_sec,
      status_code = rai$status_code,
      has_error = rai$has_error,
      err_msg = rai$err_msg,
      finish_reason = rai$res_df$finishReason,
      num_rows = num_rows
    )
    as_tibble(res_li)
  }))
  rai_stats
}



rais_save = function(rais, prod_df=rais[["prod_df"]], save_rais = TRUE) {
  restore.point("rais_save")
  to_r0 = rais$to_r0
  version = rais$version
  vid = version$vid
  project_dir = rais$project_dir
  if (is.null(rais$run_dir) | is.null(rais$version_dir)) {
    stop("rais has no run_dir or version_dir")
  }
  run_dir = rais$run_dir
  version_dir = rais$version_dir
    
  if (!dir.exists(rais$version_dir)) dir.create(rais$version_dir,recursive = TRUE)
  if (!dir.exists(rais$run_dir)) dir.create(rais$run_dir)
  
  if (save_rais) {
    saveRDS(rais,"rais.Rds")
  }
  rai_stats=rais_rai_stats(rais)
  if (length(rais["issues"]>0)) {
    issue_df = bind_rows(rais$issues)
    saveRDS(issue_df, file.path(run_dir, "issues.Rds"))
  }
  err_file = file.path(run_dir, "has_err.txt")
  if (file.exists(err_file)) file.remove(err_file)
  
  saveRDS(rai_stats, file.path(run_dir, "rai_stats.Rds"))
  saveRDS(version, file.path(run_dir, "version.Rds"))
  saveRDS(prod_df, file.path(run_dir, "prod_df.Rds"))
  rais$prod_df = prod_df
  rais$rai_stats = rai_stats
  invisible(rais)
}

rais_has_error = function(rais) {
  restore.point("rais_has_error")
  err_res = TRUE
  if (is(rais, "try-error")) {
    attr(err_res, "err_msg") = paste0(as.character(rais), collapse="\n")
    return(err_res)
  }
    
  has_errors = sapply(rais$li, function(rai) rai$has_error)
  if (any(has_errors)) {
    attr(err_res, "err_msg") = paste0("error in rai", collapse="\n")
    return(err_res)
  }
  return(FALSE)
}

rais_save_error = function(rais, err_msg="unspecified error") {
  if (is.null(rais$run_dir) | is.null(rais$version_dir)) {
    stop("rais has no run_dir or version_dir")
  }
  if (!dir.exists(rais$version_dir)) dir.create(rais$version_dir,recursive = TRUE)
  if (!dir.exists(rais$run_dir)) dir.create(rais$run_dir)
  
  rais$has_error = TRUE
  saveRDS(rais,"rais.Rds")
  if (is.null(err_msg)) err_msg="unspecified error"
  writeLines(err_msg, file.path(rais$run_dir,"has_error.txt"))
  invisible(rais)
}

rais_save_if_error = function(rais) {
  restore.point("rais_save")
  has_error = rais_has_error(rais)
  if (!has_error) return(FALSE) 
  rais_save_error(rais, attr(has_error,"err_msg"))
  return(TRUE)
}


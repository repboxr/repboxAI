# rais is a collection of similar rai
# that are called for different rows of an input data set
rais_init = function(li, prod=NULL, to_r0=TRUE, df=NULL) {
  restore.point("rais_init")
  if (is(li,"repbox_rai")) li = list(li)
  rai = li[[1]]
  rais = list(rai=rai, li=li, nrow = max(length(li), NROW(df)), to_r0=to_r0, prod=prod)
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
  i = 1
  rai_stats = bind_rows(lapply(seq_along(li), function(i) {
    rai = rais[[i]]
    httr_res = rai$httr_res
    if (is(rai$content, "try-error")) {
      num_rows = NA_integer_
    } else {
      num_rows = NROW(rai$content)
    }
    data.frame(
      rai_ind = i,
      time_stamp = rai$time_stamp,
      run_sec = rai$run_sec,
      status_code = httr_res$status_code,
      parse_error = httr_res$parse_error,
      finish_reason = httr_res$candidates$finishReason,
      num_rows = num_rows
    )
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
    
  if (!dir.exists(rais$version_dir)) dir.create(rais$version_dir,recursive = TRUE)
  if (!dir.exists(rais$run_dir)) dir.create(rais$run_dir)
  
  if (save_rais) {
    saveRDS(rais,"rais.Rds")
  }
  rai_stats=rais_rai_stats(rais)
  saveRDS(rai_stats, file.path(run_dir, "rai_stats.Rds"))
  saveRDS(version, file.path(run_dir, "version.Rds"))
  saveRDS(prod_df, file.path(run_dir, "prod_df.Rds"))
  rais$prod_df = prod_df
  rais$rai_stats = rai_stats
  invisible(rais)
}

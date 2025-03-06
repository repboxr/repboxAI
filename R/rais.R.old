# rais is a collection of similar rai
# that are called for different rows of an input data set
rais_init = function(li=NULL, prod=NULL, to_r0=TRUE, df=NULL, input_info=NULL, rai=NULL, version=NULL, finish_function=NULL) {
  restore.point("rais_init")
  if (is(li,"repbox_rai")) li = list(li)
  if (is.null(rai) & !is.null(li))
    rai = li[[1]]
  if (is.null(version) & !is.null(rai)) {
    version = rai$version
  }
  
  rais = list(rai=rai, li=li, nrow = max(length(li), NROW(df)), to_r0=to_r0, prod=prod, issues=list(), input_info=input_info, finish_function=finish_function)

  
  fields = c("project_dir","version","pid","json_mode")
  rais[fields] = rai[fields]
  if (is.null(df)) {
    rais$df = tibble(rai_ind = seq_len(rais$nrow))  
  } else {
    rais$df = add_col_left(df, rai_ind = seq_len(rais$nrow))
  }
  version = rais$version
  
  project_dir = rais$project_dir
  dirs = hx_make_version_run_dir(rais)
  rais[names(dirs)] = dirs
  
  class(rais) = c("repbox_rais","list")
  rais
}

rais_finish = function(rais) {
  if (is.null(rais$finish_function)) stop("rais$finish_function not defined")
  do.call(rais$finish_function, list(rais=rais))
}

rai_is_run_dir_incomplete = function(run_dir) {
  file.exists(file.path(run_dir, "incomplete_rais.Rds"))
}

rais_run_all_rai = function(rais, run_fun, fill_incomplete=isTRUE(rais$is_incomplete), backup_incomplete=TRUE, verbose = TRUE) {
  restore.point("rais_run_all_rai")
  if (fill_incomplete & !is.null(rais$li)) {
    rows = rais$incomplete_rows
  } else {
    rows = seq_len(rais$nrow)
  }
  start_time = as.numeric(Sys.time())
  if (verbose) cat(paste0("\nMake ",NROW(rows)," AI calls "))
  if (is.null(rais[["li"]])) {
    rais$li = vector("list", rais$nrow)
  }
  for (row in rows) {
    rais$li[[row]] = rai = run_fun(row, rais)
    rais$li[[row]]
  }
  rais$li = lapply(rows, function(row) {
    rai = run_fun(row, rais)
    if (!isTRUE(rai$status_code==200)) {
      if (verbose) cat("x")
    } else {
      if (verbose) cat(".")
    }
    rai
  })
  restore.point("rais_run_all_rai_post")
  if (verbose) cat(paste0(round(as.numeric(Sys.time())-start_time), " sec.\n"))
  rais$status_codes = sapply(rais$li, function(rai) {
    if (is.null(rai)) return(-1)
    rai$status_code
  })
  rais$incomplete_rows = which(!is.true(rais$status_codes == 200))
  rais$complete_rows = which(is.true(rais$status_codes == 200))
  
  rais$num_incomplete = length(rais$incomplete_rows)
  rais$num_complete =  length(rais$complete_rows)

  incomplete_file = file.path(rais$run_dir, "incomplete_rais.Rds")  
  if (backup_incomplete & rais$num_incomplete>0) {
    if (!dir.exists(rais$run_dir)) dir.create(rais$run_dir, recursive = TRUE)
    problem_codes = rais$status_codes[rais$status_codes != 200]
    saveRDS(rais,incomplete_file )
    
    cat(paste0("\n ", length(problem_codes), " of ", length(status_codes), "  AI calls returned problems (status code: ", paste(unique(status_codes), collapse=", "), "). Incomplete rais object saved to ", rais$run_dir))
    
  } else if (rais$num_incomplete == 0 & file.exists(incomplete_file)) {
    try(file.remove(incomplete_file))
  }
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
    saveRDS(rais,file.path(rais$run_dir,"rais.Rds"))
  }
  rai_stats=rais_rai_stats(rais)
  if (length(rais["issues"]>0)) {
    issue_df = bind_rows(rais$issues)
    saveRDS(issue_df, file.path(run_dir, "issues.Rds"))
  }
  err_file = file.path(run_dir, "has_err.txt")
  if (file.exists(err_file)) file.remove(err_file)
  
  if (!is.null(rais$input_info)) {
    saveRDS(rais$input_info,  file.path(run_dir, "input_info.Rds") )
  }
  
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
  saveRDS(rais,file.path(rais$run_dir,"rais.Rds"))
  if (is.null(err_msg)) err_msg="unspecified error"
  writeLines(err_msg, file.path(rais$run_dir,"has_error.txt"))
  
  if (!is.null(rais$input_info)) {
    saveRDS(rais$input_info,  file.path(rais$run_dir, "input_info.Rds") )
  }
  
  
  invisible(rais)
}

rais_save_if_error = function(rais) {
  restore.point("rais_save")
  has_error = rais_has_error(rais)
  if (!has_error) return(FALSE) 
  rais_save_error(rais, attr(has_error,"err_msg"))
  return(TRUE)
}


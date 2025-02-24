
hx_init = function(project_dir, version, prod=NULL, to_r0=TRUE, input_info=NULL, run_dir=NULL) {
  hx = list(project_dir=project_dir, version=version, prod=prod, to_r0=to_r0, input_info=input_info)
  dirs = hx_make_version_run_dir(hx)
  hx[names(dirs)] = dirs
  if (!is.null(run_dir)) {
    hx$run_dir = run_dir
  }
  hx
}

hx_save = function(hx, prod_df) {
  restore.point("hx_save")
  if (is.null(hx$run_dir)) stop("no hx$run_dir specified")
  if (!dir.exists(hx$version_dir)) dir.create(hx$version_dir,recursive = TRUE)
  if (!dir.exists(hx$run_dir)) dir.create(hx$run_dir)
  
  run_dir = hx$run_dir
  saveRDS(hx,file.path(hx$run_dir,"hx.Rds"))
  err_file = file.path(run_dir, "has_err.txt")
  if (file.exists(err_file)) file.remove(err_file)
  
  if (!is.null(hx$input_info)) {
    saveRDS(hx$input_info,  file.path(run_dir, "input_info.Rds") )
  }
  saveRDS(hx$version, file.path(run_dir, "version.Rds"))
  saveRDS(prod_df, file.path(run_dir, "prod_df.Rds"))
  hx$prod_df = prod_df
  invisible(hx)
}

hx_backport_save = function(hx,bp_prod, prod_df=hx$prod_df) {
  restore.point("hx_backport_save")
  if (is.null(bp_prod)) stop("Need bp_prod.")
  bp_prod_df = df_to_prod_df(prod_df, bp_prod)
  hx$version$pid = bp_prod$pid
  dirs = hx_make_version_run_dir(hx)
  hx[names(dirs)] = dirs
  bp = hx_save(hx,bp_prod_df)
  invisible(bp)
}

hx_make_version_run_dir = function(hx, project_dir=hx$project_dir, version=hx$version, to_r0 = hx$to_r0) {
  restore.point("hx_make_version_run_dir")
  
  if (is.null(project_dir)) stop("project_dir does not exist.")
  if (!dir.exists(project_dir)) stop("project_dir does not exist.")
  
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
  
  list(version_dir=version_dir,run_dir=run_dir)
}


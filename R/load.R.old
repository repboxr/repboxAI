example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  
  rai_all_vid(project_dir,"cell_base")
  pid = "cell_base"
  vid = "tab_html_hx_pdf"
  rai_newest_run_dir(project_dir, pid, vid)
  
  
  df = rai_all_run_info(project_dir)
  
  df = rai_load_all_prod_df(project_dir, "cell_base")
}

rai_load_newest_prod_df = function(project_dir, pid, vid, add_ids=TRUE) {
  run_dir = rai_newest_run_dir(project_dir, pid, vid)
  rai_load_prod_df(run_dir,add_ids=add_ids)
} 

rai_newest_run_dir = function(project_dir, pid, vid=NULL) {
  info = rai_all_run_info(project_dir, pid=pid, vid=vid)
  info %>%
    group_by(pid, vid) %>%
    arrange(desc(mtime)) %>%
    slice(1) %>%
    pull(run_dir)
}

rai_load_all_prod_df = function(project_dir, pid, add_ids=TRUE, as_df = TRUE) {
  restore.point("rai_load_all_prod_df")
  run_dirs = rai_all_run_dirs(project_dir, pid, only_success = TRUE)
  df_li = lapply(run_dirs, rai_load_prod_df, add_ids=add_ids)  
  if (!as_df) return(df_li)
  df = bind_rows(df_li)
  df
}


rai_run_dir_to_ids = function(run_dir) {
  dname = dirname(run_dir)
  vid = basename(dname)
  pid = basename(dirname(dname))
  iid = paste0(vid,"--", basename(run_dir))
  data.frame(pid=pid, vid=vid, iid=iid, run_dir=run_dir)
}

rai_load_prod_df = function(run_dir = run_dir, prod_df=NULL, add_ids=FALSE) {
  restore.point("rai_load_prod_df")
  if (!is.null(prod_df)) return(prod_df)
  prod_df = readRDS(file.path(run_dir, "prod_df.Rds"))
  if (add_ids) {
    id_df = rai_run_dir_to_ids(run_dir)
    prod_df = add_col_left(prod_df, vid=id_df$vid, iid=id_df$iid)
  }
  prod_df
  
}

rai_all_pid = function(project_dir) {
  parent_dir = file.path(project_dir,"rai","prod_runs")
  list.dirs(parent_dir,full.names = FALSE,recursive = FALSE)
}


rai_all_vid = function(project_dir, pid=NULL) {
  parent_dir = file.path(project_dir,"rai","prod_runs")
  if (is.null(pid)) {
    pid_dirs = list.dirs(parent_dir,full.names = TRUE,recursive = FALSE)
  } else {
    pid_dirs = file.path(parent_dir, pid)
  }
  list.dirs(pid_dirs,full.names = FALSE,recursive = FALSE)
}


rai_all_version_dir = function(project_dir, pid, only_success=TRUE) {
  parent_dir = file.path(project_dir,"rai","prod_runs",pid)
  if (only_success) {
    files = list.files(parent_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
    run_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(run_dirs)
}


rai_all_run_dirs = function(project_dir, pid=NULL,vid=NULL, only_success=TRUE) {
  parent_dir = file.path(project_dir,"rai","prod_runs")
  if (!is.null(pid)) parent.dir = file.path(parent_dir, pid)
  if (!is.null(pid) & !is.null(vid)) parent.dir = file.path(parent_dir, vid)
  
  if (only_success) {
    files = list.files(parent_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
    run_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(run_dirs)
}


rai_all_run_info = function(project_dir, pid=NULL,vid=NULL, only_success=TRUE) {
  restore.point("rai_all_run_info")
  parent_dir = file.path(project_dir,"rai","prod_runs")
  if (!is.null(pid)) parent_dir = file.path(parent_dir, pid)
  if (!is.null(pid) & !is.null(vid)) parent_dir = file.path(parent_dir, vid)
  
  if (only_success) {
    files = list.files(parent_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
    run_dirs = dirname(files)
    df = rai_run_dir_to_ids(run_dirs) %>%
      mutate(
        mtime = file.mtime(files),
        prod_df_file = files,
        prod_df_mb = file.size(files) / 1e6
      )
    return(df)
    
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
}




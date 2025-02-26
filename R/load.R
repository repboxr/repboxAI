example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  
  df = rai_load_all_prod_df(project_dir, "cell_base")
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
  vid = basename(dirname(run_dir))
  iid = paste0(vid,"--", basename(run_dir))
  data.frame(vid=vid, iid=iid, run_dir=run_dir)
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

rai_all_run_dirs = function(project_dir, pid, only_success=TRUE) {
  parent_dir = file.path(project_dir,"rai","prod_runs",pid)
  if (only_success) {
    files = list.files(parent_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
    run_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(run_dirs)
}





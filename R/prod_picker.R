# Tools to pick a particular instance of a product

# This function should become much more sophisticated
rai_prod_pick_run_dir = function(project_dir, pid, ...) {
  runs_dir = file.path(project_dir, "rai", "prod_runs", pid)
  files = list.files(runs_dir, glob2rx("prod_df.Rds"), full.names = TRUE, recursive = TRUE)
  if (length(files)==0) return(NULL)

  # current default: select youngest
  time = file.mtime(files)
  ind = which.max(time)
  dirname(files[ind])
}

rai_load_prod_df = function(run_dir = rai_prod_pick_run_dir(project_dir, pid), project_dir=NULL, pid=NULL) {
  if (is.null(run_dir)) return(NULL)
  file = file.path(run_dir, "prod_df.Rds")
  readRDS(file)
}

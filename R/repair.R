# Functions to repir stuff, e.g.
# remove products that are renamed etc

example = function() {
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)
  prod_id = "readme_data_descr"
  #prod_id = "tab_tino"
  proc_id = NULL
}

fp_prod_dir_to_trash = function(parent_dir, prod_id) {
  rem_ver_dirs = unique(c(
    fp_all_ver_dirs(parent_dir, prod_id),
    fp_all_error_ver_dirs(parent_dir,prod_id),
    fp_all_outage_ver_dirs(parent_dir, prod_id)
  ))
  if (NROW(rem_ver_dirs)==0) {
    cat("\nNo prod_dir found for ", prod_id,"\n")
    return(NULL)
  }
  rem_prod_dirs = unique(fp_ver_dir_to_prod_dir(rem_ver_dirs))  
}

remove_old_rai_dirs = function(project_dirs) {
  rai_dirs = file.path(project_dirs,"rai")
  rai_dirs = rai_dirs[dir.exists(rai_dirs)]
  dir_to_trash(rai_dirs)
}

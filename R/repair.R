# Functions to repir stuff, e.g.
# remove products that are renamed etc

example = function() {
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)
  prod_id = "readme_data_descr"
  #prod_id = "tab_tino"
  proc_id = NULL
}

rename_map_reg_static_proc_id = function() {
  library(repboxAI)
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)
  proc_dirs = list.files(paste0(project_dirs,"/fp"), glob2rx("g25pe"),recursive = TRUE,include.dirs = TRUE, full.names = TRUE) 
  proc_dirs = proc_dirs[has.substr(proc_dirs, "/map_reg_static/") ]
  proc_dirs = proc_dirs[dir.exists(proc_dirs)]
  
  for (proc_dir in proc_dirs) {
    ver_dir = file.path(proc_dir, "v0")
    pru_files = file.path(ver_dir, c("pru.Rds", "outage_pru.Rds", "error_pru.Rds"))
    pru_files = pru_files[file.exists(pru_files)]
    
    
    proc_id = basename(proc_dir)
    new_procid = NULL
    for (pru_file in pru_files) {
      pru = readRDS(pru_files)
      if (pru$proc_id != "g25pe") next
      tabmain_procid = pru$tab_main_info$proc_id
      new_procid = paste0(pru$proc_id, "-", tabmain_procid)
      new_proc_dir = file.path(dirname(proc_dir),new_procid)
      new_ver_dir = file.path(new_proc_dir, "v0")
      new_ver_id = fp_ver_dir_to_ids(new_ver_dir)$ver_id
      pru$proc_info$proc_id = new_procid
      pru$proc_id = new_procid
      pru$ver_dir = new_ver_dir
      pru$ver_id = new_ver_id
      saveRDS(pru, pru_file)
    }
    if (!is.null(new_procid)) {
      file.rename(proc_dir, new_proc_dir)
    }
  }
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

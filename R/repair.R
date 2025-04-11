# Functions to repir stuff, e.g.
# remove products that are renamed etc

example = function() {
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)
  prod_id = "readme_data_descr"
  #prod_id = "tab_tino"
  proc_id = NULL
}

# previously some cellid had form cell-2_4
# others just 2_4
# now common format c2_4
repair_cell_id = function() {
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs(parent_dir)

  ver_dirs = fp_all_ver_dirs(project_dirs, "tab_html")
  ver_dirs = fp_all_ver_dirs(project_dirs, "tab_main")
  
  ver_dir = ver_dirs[33]
  for (ver_dir in ver_dirs) {
    prod_df = fp_load_prod_df(ver_dir)
    html = prod_df$tabhtml
    html = html %>% stri_replace_all_fixed('id = "cell-cell-', 'id="')
    html = html %>% stri_replace_all_regex('id[ ]*=[ ]*"', 'id="')
    html = html %>% stri_replace_all_fixed('id="cell-', 'id="')

    html = sapply(seq_along(html), function(i) {
      str = html[i]
      tabid = prod_df$tabid[i]
      str = stri_replace_all_regex(str, 'id="(\\d+)"', paste0('id="c', tabid, '_$1"'))[[1]]
      str = stri_replace_all_regex(str, 'id="(\\d+)-(\\d+)"','id="c$1_$2"')[[1]]
      str = stri_replace_all_regex(str, 'id="(\\d+)_(\\d+)"','id="c$1_$2"')[[1]]
      str
    })
    prod_df$tabhtml = html
    fp_save_prod_df(prod_df, ver_dir)
  }

  ver_dirs = fp_all_ver_dirs(project_dirs, "cell_base")
  ver_dirs = fp_all_ver_dirs(project_dirs, "cell_list")
  ver_dir = ver_dirs[1]
  
  for (ver_dir in ver_dirs) {
    prod_df = fp_load_prod_df(ver_dir)
    cellid = prod_df$cellid
    cellid = stri_replace_first_fixed(cellid, "cell-","")
    cellid = stri_replace_all_fixed(cellid,  '-',"_")
    
    rows = stri_detect_regex(cellid,"^\\d+_\\d+$" )
    cellid[rows] = paste0("c", cellid[rows])
    rows = stri_detect_regex(cellid,"^\\d+$" )
    cellid[rows] = paste0("c", prod_df$tabid[rows], "_", cellid[rows])
    prod_df$cellid = cellid
    fp_save_prod_df(prod_df, ver_dir, overwrite=TRUE)
  }

  
  ver_dirs = fp_all_ver_dirs(project_dirs, "map_reg_static")
  ver_dir = ver_dirs[1]
  
  for (ver_dir in ver_dirs) {
    prod_df = fp_load_prod_df(ver_dir)
    cellids = prod_df$cell_ids
    i = 1
    cellids = sapply(seq_along(cellids), function(i) {
      str = cellids[i]
      tabid = prod_df$tabid[i]
      str = stri_replace_all_fixed(str, "cell-","")
      str = stri_replace_all_regex(str,  '(?<=^|,)(\\d+)(?=,|$)', paste0(tabid,"_$1"))
      str = stri_replace_all_fixed(str,  '-',"_")
      str = stri_replace_all_regex(str,  '(?<=^|,)(\\d+_\\d+)(?=,|$)', paste0("c$1"))
      str
    })
    prod_df$cell_ids = cellids
    prod_df = rename.col(prod_df, "do_file", "script_file")
    fp_save_prod_df(prod_df, ver_dir, overwrite=TRUE)
  }
  

  ver_dirs = fp_all_ver_dirs(project_dirs, "reg_classify_static")
  ver_dir = ver_dirs[1]
  
  for (ver_dir in ver_dirs) {
    prod_df = fp_load_prod_df(ver_dir)
    cellids = prod_df$cell_id_coef_of_interest
    i = 1
    cellids = sapply(seq_along(cellids), function(i) {
      str = cellids[i]
      tabid = prod_df$tabid[i]
      str = stri_replace_all_fixed(str, "cell-","")
      str = stri_replace_all_regex(str,  '(?<=^|,)(\\d+)(?=,|$)', paste0(tabid,"_$1"))
      str = stri_replace_all_fixed(str,  '-',"_")
      str = stri_replace_all_regex(str,  '(?<=^|,)(\\d+_\\d+)(?=,|$)', paste0("c$1"))
      str
    })
    prod_df$cell_id_coef_of_interest = cellids
    fp_save_prod_df(prod_df, ver_dir, overwrite=TRUE)
  }
  
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

example = function() {
  
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  proc_all_tab_html_to_cell_list(project_dir, overwrite=TRUE)
  proc_all_cell_list_to_cell_base(project_dir, overwrite=TRUE)
  
  project_dirs = list.dirs("~/repbox/projects_share", full.names = TRUE,recursive = FALSE)
  proc_all_tab_html_to_cell_list(paste0(project_dirs,"fp"))
  proc_all_cell_list_to_cell_base(paste0(project_dirs,"fp"))
  
  ver_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/tab_html/raw_tab_html-n-g2f-0/v0"
  
  parent_dir = "~/repbox/projects_share/aeri_1_2_6"
  proc_raw_tab_html_to_cell_base(ver_dir = ver_dir)
  
  rstudioapi::filesPaneNavigate(project_dir)
  
  
}

proc_all_tab_html_to_cell_list = function(project_dir, overwrite=FALSE) {
  ddp_derive_all_instances(project_dir, from_prod_id = "tab_html", to_prod_id = "cell_list",convert_fun = proc_tab_html_to_cell_list, overwrite=overwrite)
}

proc_all_tab_html_to_cell_list = function(project_dir, overwrite=FALSE) {
  ddp_derive_all_instances(project_dir, from_prod_id = "tab_html", to_prod_id = "cell_list",convert_fun = proc_tab_html_to_cell_list, overwrite=overwrite)
}

proc_all_raw_tab_html_to_cell_base = function(parent_dir) {
  restore.point("proc_all_raw_tab_html_to_cell_base")
  ver_dirs = fp_all_ver_dirs(parent_dir)
  proc_ids = fp_ver_dir_to_proc_id(ver_dirs)
  ver_dirs = ver_dirs[startsWith(proc_ids, "raw_tab_html")]
  for (ver_dir in ver_dirs) {
    proc_raw_tab_html_to_cell_base(ver_dir=ver_dir)
  }
}

proc_raw_tab_html_to_cell_base = function(org_pru=NULL, ver_dir=org_pru$ver_dir, tab_df = org_pru$prod_df) {
  restore.point("proc_raw_tab_html_to_cell_base")
  if (is.null(org_pru)) {
    org_pru = readRDS(file.path(ver_dir, "pru.Rds"))
  }
  prod_id = "cell_base"
  proc_id = paste0(prod_id, str.right.of(org_pru$proc_id,"tab_html"))
  pru = ddp_init_pru(org_pru, ver_dir=ver_dir,ddp_prod_id=prod_id, ddp_proc_id = proc_id)
  if (is.null(tab_df)) {
    tab_df = fp_load_prod_df(ver_dir=org_pru$ver_dir)
  }
  
  i = 1
  cell_df = bind_rows(lapply(seq_len(NROW(tab_df)), function(i) {
    restore.point("shkfjhksfdo")
    cell_df = normalized_html_tab_to_cell_df(tab_df$tabhtml[[i]])
    cell_df = add_col_left(cell_df, tabid=tab_df$tabid[[i]])
    cell_df = cells_add_cell_base(cell_df,split_multi_num = TRUE)
    cell_df$cellid = paste0(tab_df$tabid[i],"_", stri_sub(cell_df$cellid,6))
    cell_df      
  }))
  prod = repbox_prod("cell_base")
  prod_df = df_to_prod_df(cell_df, prod)
  pru = pru_save(pru,prod_df)

  pru_backport_save(pru, repbox_prod("cell_list"),prod_df)
  cell_df$content = cell_df$text
  cell_li = split(cell_df, cell_df$tabid)
  tab_df$tabhtml =  sapply(cell_li, cell_df_to_simple_tabhtml)
  tab_pru = pru_backport_save(pru, repbox_prod("tab_html"), tab_df)
  # rstudioapi::filesPaneNavigate(tab_pru$ver_dir)
  rai_write_all_tables_html(tab_df, "tables.html", out_dir=tab_pru$ver_dir, info=tab_pru$proc_info)
  invisible(pru)
  
}


# cell_list is a derived prod of tab_html
proc_tab_html_to_cell_list = function(pru=NULL, ver_dir=pru$ver_dir, prods=repbox_prods(), prod_df=pru$prod_df, also_cell_base=FALSE) {
  restore.point("proc_tab_html_to_cell_list")
  prod = prods[["cell_list"]]
  df = fp_load_prod_df(ver_dir=ver_dir, prod_df=prod_df)

  pru = ddp_init_pru(pru, ver_dir=ver_dir,ddp_prod_id="cell_list")
  i = 1
  cell_df = bind_rows(lapply(seq_len(NROW(df)), function(i) {
    restore.point("shkfjhksfdo")
    cell_df = normalized_html_tab_to_cell_df(df$tabhtml[[i]])
    cell_df = add_col_left(cell_df, tabid=df$tabid[i],otabid = df$otabid[i])
    cell_df$cellid = paste0(df$tabid[i],"_", stri_sub(cell_df$cellid,6))
    cell_df      
  }))
  prod_df = df_to_prod_df(cell_df, prod)
  pru = pru_save(pru,prod_df)
  
  if (also_cell_base) {
    pru = proc_cell_list_to_cell_base(pru)
  }
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)
  
}


proc_all_cell_list_to_cell_base = function(project_dir, overwrite=FALSE) {
  restore.point("proc_all_cell_list_to_cell_base")
  ddp_derive_all_instances(project_dir, from_prod_id = "cell_list", to_prod_id = "cell_base",convert_fun = proc_cell_list_to_cell_base, overwrite=overwrite)
}


proc_cell_list_to_cell_base = function(pru=NULL, ver_dir=pru$ver_dir, prods=repbox_prods(), prod_df=pru$prod_df) {
  restore.point("proc_cell_list_to_cell_base")
  cell_df = fp_load_prod_df(ver_dir=ver_dir, prod_df=prod_df)
  prod = repbox_prod("cell_base", prods)
  pru = ddp_init_pru(pru, ver_dir=ver_dir,ddp_prod_id="cell_base")
  prod_df = cell_list_to_cell_base_prod(cell_df, prod=prod)
  pru = pru_save(pru,prod_df)
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)  
}


cell_list_to_cell_base_prod = function(cell_list, prod=repbox_prods()[["cell_base"]]) {
  restore.point("cell_list_to_cell_base_prod")
  # From repboxTableTools
  cell_df = cells_add_cell_base(cell_list)
  df_to_prod_df(cell_df,  prod)
}

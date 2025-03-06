example = function() {
  
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  proc_all_tab_html_to_cell_list(project_dir, overwrite=TRUE)
  proc_all_cell_list_to_cell_base(project_dir, overwrite=TRUE)
}

proc_all_tab_html_to_cell_list = function(project_dir, overwrite=FALSE) {
  ddp_derive_all_instances(project_dir, from_pid = "tab_html", to_pid = "cell_list",convert_fun = proc_tab_html_to_cell_list, overwrite=overwrite)
}


# cell_list is a derived prod of tab_html
proc_tab_html_to_cell_list = function(pru=NULL, ver_dir=pru$ver_dir, prods=repbox_prods(), prod_df=pru$prod_df) {
  restore.point("proc_tab_html_to_cell_list")
  prod = prods[["cell_list"]]
  df = fp_load_prod_df(ver_dir=ver_dir, prod_df=prod_df)

  pru = ddp_init_pru(pru, ver_dir=ver_dir,ddp_prod_id="cell_list")
  i = 1
  cell_df = bind_rows(lapply(seq_len(NROW(df)), function(i) {
    restore.point("shkfjhksfdo")
    cell_df = normalized_html_tab_to_cell_df(df$tabhtml[[i]]) %>% rename(text=content)
    cell_df = add_col_left(cell_df, tabid=df$tabid[i],otabid = df$otabid[i])
    cell_df$cellid = paste0(df$tabid[i],"_", stri_sub(cell_df$cellid,6))
    cell_df      
  }))
  prod_df = df_to_prod_df(cell_df, prod)
  pru = pru_save(pru,prod_df)
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)  
}


proc_all_cell_list_to_cell_base = function(project_dir, overwrite=FALSE) {
  restore.point("proc_all_cell_list_to_cell_base")
  ddp_derive_all_instances(project_dir, from_pid = "cell_list", to_pid = "cell_base",convert_fun = proc_cell_list_to_cell_base, overwrite=overwrite)
}


proc_cell_list_to_cell_base = function(pru=NULL, ver_dir=pru$ver_dir, prods=repbox_prods(), prod_df=pru$prod_df, version=pru$version) {
  restore.point("proc_cell_list_to_cell_base")
  cell_df = fp_load_prod_df(ver_dir=ver_dir, prod_df=prod_df)
  prod = get_repbox_prod("cell_base", prods)
  pru = ddp_init_pru(pru, ver_dir=ver_dir,ddp_pid="cell_base", version = version)
  prod_df = cell_list_to_cell_base_prod(cell_df, prod=prod)
  pru = pru_save(pru,prod_df)
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)  
}


cell_list_to_cell_base_prod = function(cell_list, prod=repbox_prods()[["cell_base"]]) {
  restore.point("cell_df_to_cell_base_prof")
  # From repboxTableTools
  cell_df = cells_add_cell_base(cell_list)
  df_to_prod_df(cell_df,  prod)
}

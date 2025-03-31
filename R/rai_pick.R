rai_pick_tab_main = function(project_dir, doc_type, pref =  tab_main_default_pref(), pru = NULL) {
  if (is.null(pru)) pru = list()
  
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  pru$tab_main_info = fp_pick_prod_ver(fp_dir, "tab_main", pref=pref)
  pru$tab_main = fp_load_prod_df(pru$tab_main_info$ver_dir)
  pru
}

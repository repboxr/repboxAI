
doc_file_form_default_pref = function() {
  c("html","pdf","mocr_md", "pdf_txt")
}

tab_df_default_pref = function(prod_id = "tab_main") {
  glob2rx(c("html","pdf-g*","mocr","pdf_txt"))
}


map_reg_static_default_pref = function() {
  glob2rx(c("gp25*mocr*","gp25*"))
}


tab_ref_default_pref = function() {
  c("html", "mocr","pdf")
}


rai_pick_tab_df = function(project_dir, prod_id = "tab_main", doc_type, pref =  tab_df_default_pref(prod_id), pru = NULL) {
  if (is.null(pru)) pru = list()
  
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  pru$tab_df_info = fp_pick_prod_ver(fp_dir, prod_id, pref=pref)
  pru$tab_df = fp_load_prod_df(pru$tab_df_info$ver_dir)
  pru
}

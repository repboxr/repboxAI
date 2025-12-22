
doc_file_form_default_pref = function(manual=FALSE) {
  if (!manual) {
    c("html","pdf","mocr_md", "pdf_txt")
  } else {
    c("mocr_md", "pdf_txt","html")
  }
}

tab_df_default_pref = function(prod_id = "tab_main") {
  rai_prefs(c("mocr", "html","pdf-g*","pdf_txt"), prio_mocr=FALSE)
}



map_reg_default_pref = map_reg_run_default_pref = map_reg_static_default_pref = function(map_version=NULL) {
  rai_prefs()
}

rai_prefs = function(globs = c("gp30*","gp25*","gf30*","gf25*","gfl25*"), prio_rev=TRUE, prio_mocr=TRUE) {
  if (prio_rev) {
    globs = unlist(lapply(globs, function(glob) c(paste0("r*_", glob),glob)))
  }
  if (prio_mocr) {
    globs = c(paste0(globs,"mocr*"), globs)
  }
  glob2rx(globs)
}


pref_prio_revision = function(prefs) {
  unlist(lapply(prefs, function(pref) c(paste0("r*_", pref),pref)))
}
pref_prio_mocr = function(prefs) {
  c(prefs)
}



map_reg_static_default_pref = function() {
  map_reg_default_pref()
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

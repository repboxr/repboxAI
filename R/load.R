rai_file_cache = function(...) {
  new.env(parent=emptyenv())
}


rai_pick_tab_ver = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=NULL) {
  restore.point("rai_pick_tab_ver")
  if (is.null(pref)) {
    if (prod_id == "tab_notes") {
      pref = fp_pref(proc_regex = c("html","^pdf-g2f.*"))
    } else {
      pref = fp_pref(proc_regex = c("html", "mocr","pdf-.*","pdf_txt"))
      
    }
  }
  fp_pick_prod_ver(fp_dir, prod_id=prod_id,proc_id=proc_id, pref=pref)
}

# Shorten file paths if files are unique
# to reduce length of output tokens
script_df_shorten_file = function(script_df) {
  if (!anyDuplicated(script_df$file_path)) {
    script_df$script_file = basename(script_df$file_path)
  } else {
    script_df$script_file = script_df$file_path
  }
  script_df
}

rai_load_do_source = function(project_dir, parcels=list()) {
  parcels = repdb_load_parcels(project_dir, "stata_source",parcels)
  script_df = parcels$stata_source$script_source
  script_df = script_df_shorten_file((script_df))
  script_df
}

rai_load_tab_prod_df = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=NULL, cache=NULL) {
  restore.point("rai_load_tab_prod_df")
  ver_dir = rai_pick_tab_ver(fp_dir, prod_id, pref, proc_id)$ver_dir
  file = file.path(ver_dir, "prod_df.Rds")
  if (!is.null(cache)) {
    prod_df = cache[[file]]
    if (!is.null(prod_df)) return(prod_df)
  }
  prod_df = fp_load_prod_df(ver_dir)
  if (!is.null(cache)) {
    cache[[file]] = prod_df
  }
  prod_df
}

rai_pick_ref_li_doc_dir = function(project_dir, doc_type="art", tab_ref_pref = tab_ref_default_pref()) {
  restore.point("rai_load_ref_li")
  files = paste0(project_dir, "/doc/", doc_type, "_", tab_ref_pref, "/ref_li.Rds")
  files = files[file.exists(files)]
  if (length(files)==0) return(NULL)
  file = files[[1]]
  dirname(file)
}


rai_doc_file = function(project_dir, doc_type, pref = doc_file_form_default_pref(), doc_form=NULL) {
  repbox_doc_file_select(project_dir, doc_type = doc_type, doc_file_form_pref = pref, doc_form=doc_form)$doc_file
}

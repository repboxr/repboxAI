# Extra classifications on a table level
example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"

  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_pdf"
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_mocr"

  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  proc_tab_extra(project_dir)
  rstudioapi::filesPaneNavigate(pru$ver_dir)
}

#' Extracts tab_html from articles
proc_tab_extra = function(project_dir, prod_id = c("tab_classify")[1], doc_type="art", doc_file_form_pref = c("html","pdf","mocr_md", "pdf_txt"), prompt_add = c("doc", "html", "notes", "refs")[1],  ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, tpl_id = prod_id, use_schema = FALSE, overwrite=TRUE) {
  restore.point("proc_tab_extra")
  stop()
  library(repboxtab)
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type)) 
  tpl_file = file.path(rai_tpl_dir(), paste0(tpl_id,".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema, tpl_id=tpl_id)
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)
  rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = tab_file)
  
  
  pru$tab_df = tab_df
  pru$project_dir = project_dir
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  pru_next_stage(pru, "pru_tab_run")
}

pru_tab_run = function(pru) {
  restore.point("pru_tab_run")
  project_dir = pru$project_dir
  df = pru$tab_df
  row = 1
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    restore.point("pru_tab_overview_run_inner")
    tab_file = file.path(project_dir, "tab", "txt", df$txt_file[row])
    values = list(tab_file = basename(df$org_file[row]))
    res = ai_run(rai, values=values)
    res
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("pru_tab_run")
  if (!pru_is_ok(pru)) return(invisible(pru))
  
  res_df = ai_combine_content_df(pru$items, df %>% select(tab_file = org_file))
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  return(invisible(pru))
}

rai_doc_file = function(project_dir, doc_type = "art", doc_pref=c("html","pdf", "md", "txt")) {
  
}

rai_pick_tab_ver = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=NULL) {
  restore.point("rai_pick_tab_ver")
  if (is.null(pref)) {
    if (prod_id == "tab_notes") {
      pref = fp_prod_ver_pref(proc_regex = c("html","^pdf-g2f.*"))
    } else {
      pref = fp_prod_ver_pref(proc_regex = c("html", "mocr","pdf-.*","pdf_txt"))
      
    }
  }
  fp_pick_prod_ver(fp_dir, prod_id=prod_id,proc_id=proc_id, pref=pref)
}


rai_load_tab_prod_df = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=proc_id) {
  ver_dir = rai_pick_tab_ver(fp_dir, prod_id, pref, proc_id)
  fp_load_prod_df(ver_dir)
}

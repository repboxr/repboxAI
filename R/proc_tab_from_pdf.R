# Parse tab_base

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-1.5-flash-001")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash-lite")
  set_ai_opts(model = "gemini-2.0-flash")
  get_ai_opts()
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  project_dir = "~/repbox/projects_share/ecta_84_2_6"
  project_dir = "~/repbox/projects_share/jeea_12_1_11"
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
    
  res = proc_tab_html_from_pdf(project_dir, doc_type="app1", overwrite=FALSE)
  
  res = proc_tab_notes_from_pdf(project_dir, overwrite=FALSE)
  
  rstudioapi::filesPaneNavigate(project_dir)
  rstudioapi::filesPaneNavigate(res$ver_dir)
}

proc_tab_html_from_pdf = function(project_dir, doc_type = NULL, overwrite=FALSE, ...) {
  doc_dirs = repbox_doc_dirs(project_dir,doc_form = "pdf", doc_type=doc_type)
  
  
  for (doc_dir in doc_dirs) {
    cat(paste0("\nExtract tables from PDF in ", doc_dir,"\n"))
    proc_doc_tab_html_from_pdf(doc_dir,overwrite=overwrite, ...)
  }
}

#' Extracts tab_html from articles
proc_doc_tab_html_from_pdf = function(doc_dir, tpl_num=1,prods=repbox_prods(), ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, overwrite=TRUE) {
  restore.point("proc_doc_tab_html_from_pdf")
  if (!rai_has_input(doc_dir, "pdf","tab_list")) return(NULL)
  
  doc_type = rdoc_type(doc_dir)
  fp_dir = doc_dir_to_fp_dir(doc_dir)
  project_dir = rai_fp_dir_to_project_dir(fp_dir)
  
  
  prod_id = "tab_html"
  doc_form = "pdf"
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id, "-", doc_form, "-", tpl_num, ".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=FALSE, use_schema = FALSE, raw=TRUE)

  
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)
  
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)

  # Prefer following table lists:
  # 1. extracted with same model
  # 2. mocr
  proc_postfix = str.right.of(proc_info$proc_id,"-") %>% str.right.of("-")
  proc_ids = fp_all_proc_id(fp_dir, "tab_list")
  input_pref = fp_input_pref(proc_id = c(
    proc_ids[endsWith(proc_ids, proc_postfix)],
    proc_ids[endsWith(proc_ids, "_mocr")]  
  ))
  pru = pru_pick_inputs(pru, "tab_list")
  if (pru_has_input_err(pru)) return(pru)

  context = rai_context(project_dir,doc_type_pdf = doc_type)
  pru$rai = rai_init(fp_dir, context=context, proc_info=proc_info, ai_opts=ai_opts)
  pru_next_stage(pru, "pru_tab_html_pdf_run")
}

pru_tab_html_pdf_run = function(pru) {
  restore.point("pru_tab_html_pdf_ai_run")
  df = pru_get_input(pru, "tab_list")
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    values = as.list(df[row,])
    ai_run(pru$rai, values=values)
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("pru_post_run")
  if (!pru_is_ok(pru)) return(invisible(pru))

  df$raw_html = ai_combine_content_str(pru$items,err_val = NA_character_)
  df$tabhtml = sapply(df$raw_html,html_table_add_cellnum_row_col)
  prod_df = df_to_prod_df(df, repbox_prod("tab_html"))
  pru_save(pru, prod_df)
  rai_write_all_tables_html(prod_df, "tables.html", out_dir=pru$ver_dir, info=pru$proc_info)
  
  proc_raw_tab_html_to_cell_base(pru, prod_df=prod_df)
  
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  
  return(invisible(pru))
  
}

proc_tab_notes_from_pdf = function(project_dir, doc_type=NULL, overwrite=FALSE,...) {

  doc_dirs = repbox_doc_dirs(project_dir,doc_form = "pdf", doc_type=doc_type)
  
  for (doc_dir in doc_dirs) {
    cat(paste0("\nExtract tables from PDF in ", doc_dir,"\n"))
    proc_doc_tab_notes_from_pdf(doc_dir,overwrite=overwrite, ...)
  }
  
}



#' Extracts tab_list and tab_notes from articles
proc_doc_tab_notes_from_pdf = function(doc_dir, tpl_num=1,use_schema=FALSE, to_v0=TRUE, ai_opts = get_ai_opts(), overwrite=TRUE) {
  if (!rai_has_input(doc_dir, "pdf")) return(NULL)
  
  fun_call = preserve_call("proc_doc_tab_notes_from_pdf")
  restore.point("proc_doc_tab_notes_from_pdf")
  
  doc_type = rdoc_type(doc_dir)
  prod_id = "tab_notes"
  art_source = "pdf"
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id, "-", art_source, "-", tpl_num, ".txt"))

  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema)
  
  fp_dir = doc_dir_to_fp_dir(doc_dir)
  project_dir = rai_fp_dir_to_project_dir(fp_dir)
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0, fun_call=fun_call)
  
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  
  # In the prompt we name variables differently to make it easier for AI
  schema = NULL
  pru$prod_to_df_cols = c(table_number="tabid",table_title="tabtitle", table_notes="tabnotes")
  if (use_schema) {
    schema = prod_to_schema(prod, "arr") %>%
      schema_reduce(pru$prod_to_df_cols)
  }
  context = rai_context(project_dir,doc_type_pdf = doc_type)
  pru$rai = rai_init(project_dir, proc_info = proc_info, schema=schema, context=context)
  
  pru$rai = rai_run(pru$rai)
  pru = pru_set_status(pru, pru$rai)
  if (!pru_is_ok(pru)) return(invisible(pru))
  prod = repbox_prod(prod_id)
  prod_df = df_to_prod_df(pru$rai$content, prod, prod_to_df_cols = pru$prod_to_df_cols)
  old_tabid = prod_df$tabid
  prod_df$tabid = tabid_normalize(prod_df$tabid)
  if (!all(old_tabid==prod_df$tabid)) {
    pru = pru_add_issue(pru, "tabid_was_standardized")
  }
  
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  pru = pru_save(pru, prod_df)
  
  pru_backport_save(pru, repbox_prod("tab_list"), prod_df=prod_df)
  rstudioapi::filesPaneNavigate(pru$ver_dir)
  return(invisible(pru))
}



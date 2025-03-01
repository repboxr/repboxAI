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
  project_dir = "~/repbox/projects_share/ecta_84_2_6"
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  
  res = proc_tab_tino_pdf(project_dir)
  res = proc_tab_html_pdf(project_dir)
  
  rstudioapi::filesPaneNavigate(res$ver_dir)
}

project_dir_to_fp_dir = function(project_dir) {
  fp_dir = file.path(project_dir, "rai", "prod_runs")
  fp_dir
}


#' Extracts tab_tino from articles
proc_tab_html_pdf = function(project_dir, tpl_num=1,prods=repbox_prods(), ai_opts = get_ai_opts(), verbose=TRUE, incomplete_pru = NULL, to_v0=TRUE) {
  restore.point("proc_tab_html_pdf")
  prod_id = "tab_html"
  art_source = "pdf"
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id, "-", art_source, "-", tpl_num, ".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=FALSE, use_schema = FALSE)

  fp_dir = project_dir_to_fp_dir(project_dir)
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)

  pru = pru_pick_inputs(pru, "tab_tino")
  if (pru_has_input_err(pru)) return(pru)

  context = rai_context(project_dir, add_art_pdf = TRUE)
  pru$rai = rai_init(project_dir, context=context, proc_info=proc_info, ai_opts=ai_opts)
  pru_next_stage(pru, "pru_tab_html_pdf_run")
}

pru_tab_html_pdf_run = function(pru) {
  restore.point("pru_tab_html_pdf_ai_run")
  df = pru_get_input(pru, "tab_tino")
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    values = as.list(df[row,])
    ai_run(pru$rai, values=values)
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("pru_post_run")
  if (!pru_is_ok(pru)) return(invisible(pru))

  df$raw_html = ai_combine_content_str(pru$items,err_val = NA_character_)
  df$tabhtml = sapply(df$raw_html,html_table_add_cellnum_row_col)
  prod_df = df_to_prod_df(df, get_repbox_prod("tab_html"))
  pru_save(pru, prod_df)
  rai_write_all_tables_html(prod_df, "tables.html", out_dir=pru$ver_dir, info=pru$proc_info)
  
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  
  return(invisible(pru))
  
}


#' Extracts tab_tino from articles
proc_tab_tino_pdf = function(project_dir, tpl_num=1,use_schema=FALSE, to_v0=TRUE, ai_opts = get_ai_opts()) {
  restore.point("proc_tab_tino_pdf")
  # stop()
  prod_id = "tab_tino"; prod = prods[[prod_id]]
  art_source = "pdf"
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id, "-", art_source, "-", tpl_num, ".txt"))

  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema)
  
  fp_dir = project_dir_to_fp_dir(project_dir)
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)
  
  
  # In the prompt we name variables differently to make it easier for AI
  schema = NULL
  pru$prod_to_df_cols = c(table_number="tabid",table_title="tabtitle", table_notes="tabnotes")
  if (use_schema) {
    schema = prod_to_schema(prod, "arr") %>%
      schema_reduce(pru$prod_to_df_cols)
  }
  context = rai_context(project_dir, add_art_pdf = TRUE)
  pru$rai = rai_init(project_dir, proc_info = proc_info, schema=schema, context=context)
  pru_next_stage(pru, "proc_tab_tino_pdf_run")
}

#' Extracts tab_tino from articles
proc_tab_tino_pdf_run = function(pru) {
  restore.point("proc_tab_tino_pdf_run")
  pru$rai = rai_run(pru$rai)
  pru = pru_set_status(pru, pru$rai)
  if (!pru_is_ok(pru)) return(invisible(pru))
  prod = get_repbox_prod("tab_tino")
  prod_df = df_to_prod_df(pru$rai$content, prod, prod_to_df_cols = pru$prod_to_df_cols)
  old_tabid = prod_df$tabid
  prod_df$tabid = tabid_normalize(prod_df$tabid)
  if (!all(old_tabid==prod_df$tabid)) {
    pru = pru_add_issue("tabid_was_standardized")
  }
  
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  pru = pru_save(pru, prod_df)
  rstudioapi::filesPaneNavigate(pru$ver_dir)
  return(invisible(pru))
}


tabid_normalize = function(tabid) {
  restore.point("tabid_normalize")
  tabid <- stri_replace_all_regex(tabid, "\\.", "_")
  tabid <- stri_replace_all_regex(tabid, "[^a-zA-Z0-9_]", "")
  tabid
}

tabid_to_otabid <- function(tabid, digits = 3) {
  # Vectorized extraction of the first sequence of digits
  matches <- stri_extract_first_regex(tabid, "[0-9]+")
  
  # For each non-NA match, create a padded version
  padded <- ifelse(is.na(matches), NA, sprintf(paste0("%0", digits, "d"), as.numeric(matches)))
  
  # Replace the first occurrence of digits with the padded number where a match was found
  result <- ifelse(is.na(matches), tabid, stri_replace_first_regex(tabid, "[0-9]+", padded))
  
  return(result)
}



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
  
  prods = repbox_prods()
  res = proc_tab_html_pdf(project_dir, prods=prods)
  
  res = raix_tab_tino_pdf(project_dir, prods=prods)
  rstudioapi::filesPaneNavigate(res$run_dir)
}

project_dir_to_fp_dir = function(project_dir) {
  fp_dir = file.path(project_dir, "rai", "prod_runs")
  fp_dir
}


#' Extracts tab_tino from articles
proc_tab_html_pdf = function(project_dir, tpl_num=1,prods=repbox_prods(), ai_opts = get_ai_opts(), verbose=TRUE, incomplete_pru = NULL, to_v0=TRUE) {
  restore.point("proc_tab_html_pdf")
  stop()
  prod_id = "tab_html"
  art_source = "pdf"
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id, "-", art_source, "-", tpl_num, ".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=FALSE, use_schema = FALSE)

  fp_dir = project_dir_to_fp_dir(project_dir)
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)

  pru = pru_pick_inputs(pru, "tab_tino")
  if (pru_has_input_err(pru)) return(pru)

  context = rai_context(project_dir, add_art_pdf = TRUE)
  pru$ai = rai_init(project_dir, context=context, proc_info=proc_info, ai_opts=ai_opts)

  # For faster testing
  # df = df[1,]

  pru_next_stage(pru, "pru_tab_html_pdf_ai_run")
}

pru_tab_html_pdf_ai_run = function(pru) {
  restore.point("pru_tab_html_pdf_ai_run")
  stop()
  df = pru_get_input(pru, "tab_tino")
  pru = pru_run_stage_items(pru, df=df, function(row, pru) {
    values = as.list(df[row,])
    rai = rai_run(pru$rai, values=values)
    rai    
  })
  restore.point("pru_post_run")
  if (pru$num_incomplete > 0 & pru$num_complete ==0) return(invisible(pru))
  
  pru = pru_combine_content(pru, var="raw_html")
  df = pru$df
  df$tabhtml = sapply(df$raw_html,html_table_add_cellnum_row_col)
  prod_df = df_to_prod_df(df, get_repbox_prod("tab_html"))
  pru_save(pru, prod_df)
  hx_write_all_tables_html(prod_df, "tables.html", run_dir=pru$run_dir)
  
  #rstudioapi::filesPaneNavigate(pru$run_dir)
  
  return(invisible(pru))
  
}

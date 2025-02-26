# Heuristic extraction of table infos

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  library(repboxArt)
  
  prods = repbox_prods()
  hx = hx_tab_html(project_dir, prods)
  rstudioapi::filesPaneNavigate(hx$run_dir)
}


hx_tab_html = function(project_dir, prods=repbox_prods(), to_r0=TRUE, add_cell_list=TRUE) {
  restore.point("hx_tab_html")
  pid = "tab_html"; prod = prods[[pid]]
  df = repboxArt::art_load_tab_df(project_dir) %>%
    rename(raw_tabhtml = tab_html)
  
  version = data.frame(pid=pid, vid = "tab_html_hx_pdf", art_source="pdf_txt")
  df$tabhtml = sapply(df$raw_tabhtml,html_table_add_cellnum_row_col)
  
  prod_df = df_to_prod_df(df, prod)
  
  old_tabid = prod_df$tabid
  prod_df$tabid = tabid_normalize(prod_df$tabid)
  if (!all(old_tabid==prod_df$tabid)) {
    rais = rais_add_issue("tabid_was_standardized")
  }
  
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  
  
  hx = hx_init(project_dir, version, prod, to_r0=to_r0, input_info=NULL)
  hx = hx_save(hx, prod_df)
  hx_backport_save(hx, prods[["tab_tino"]], prod_df)
  hx_backport_save(hx, prods[["tab_list"]], prod_df)
  hx_tab_html_to_cell_list(hx=hx, prod_df=prod_df)
  hx_write_all_tables_html(prod_df, "tables.html",run_dir = hx$run_dir)
  invisible(hx)
}

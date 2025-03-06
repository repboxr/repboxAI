# Heuristic extraction of table infos

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  project_dir = "~/repbox/projects_share/ecta_84_2_6"
  
  library(repboxArt)
  
  prods = repbox_prods()
  pru = proc_tab_html_hx(project_dir, prods)
  rstudioapi::filesPaneNavigate(hx$run_dir)
}


proc_tab_html_hx = function(project_dir, prods=repbox_prods(), to_v0=TRUE, add_cell_list=TRUE) {
  restore.point("proc_tab_html_hx")
  prod_id = "tab_html"; prod = prods[[prod_id]]
  df = repboxArt::art_load_tab_df(project_dir) %>%
    rename(raw_tabhtml = tab_html)
  
  proc_info = data.frame(prod_id=prod_id, proc_id = "tab_html_hx_pdf", art_source="pdf_txt")
  fp_dir = project_dir_to_fp_dir(project_dir)
  pru = pru_init(fp_dir,prod_id=prod_id, proc_info=proc_info, to_v0=to_v0)

  df$tabhtml = sapply(df$raw_tabhtml,html_table_add_cellnum_row_col)
  
  prod_df = df_to_prod_df(df, prod)
  
  old_tabid = prod_df$tabid
  prod_df$tabid = tabid_normalize(prod_df$tabid)
  if (!all(old_tabid==prod_df$tabid)) {
    pru = pru_add_issue(pru,"tabid_was_standardized")
  }
  
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  
  pru = pru_save(pru, prod_df)
  pru_backport_save(pru, prods[["tab_tino"]], prod_df)
  pru_backport_save(pru, prods[["tab_list"]], prod_df)
  proc_tab_html_to_cell_list(pru=pru, prod_df=prod_df)
  rai_write_all_tables_html(prod_df, "tables.html",out_dir = pru$ver_dir)
  rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)
}

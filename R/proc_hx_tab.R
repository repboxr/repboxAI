# Heuristic extraction of table infos

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  project_dir = "~/repbox/projects_share/ecta_84_2_6"
  project_dir = "~/repbox/projects_share/jeea_12_1_11"
  project_dir = "~/repbox/projects_share/jep_31_1_2"
  project_dir = "~/repbox/projects_share/jole_33_3_5"
  project_dir = "~/repbox/projects_share/jpe_123_4_4"
  project_dir = "~/repbox/projects_share/ms_65_10_17"
  project_dir = "~/repbox/projects_share/qje_3036349" # hx wrong col align
  project_dir = "~/repbox/projects_share/restat_2689366" 
  project_dir = "~/repbox/projects_share/restud_82_2_12" 
  
  
  library(repboxRun)
  steps = repbox_steps_from(art = TRUE,reproduction = FALSE)
  repbox_run_project(project_dir,lang="stata", steps=steps)
  
  
  library(repboxArt)
  
  prods = repbox_prods()
  pru = proc_tab_html_hx(project_dir, prods)
  rstudioapi::filesPaneNavigate(pru$ver_dir)
}


proc_tab_html_hx = function(project_dir, prods=repbox_prods(), to_v0=TRUE, add_cell_list=TRUE, route = repbox_art_routes(project_dir)) {
  if (length(route)>1) {
    for (ro in route) {
      #cat("\nRoute: ", route)
      res = proc_tab_html_hx(project_dir, prods, to_v0, add_cell_list, route=ro)
    }
    return(res)
  } else if (length(route)==0) {
    cat("\nNo heuristic table extraction route exists for ", project_dir,".\n")
    return(invisible())
  }
  restore.point("proc_tab_html_hx")
  prod_id = "tab_html"; prod = prods[[prod_id]]
  df = repbox_art_load_tabs_from_route(project_dir, route)
  if (has_col(df,"tab_html")) { # pdf extracted
    df$raw_tabhtml = df$tab_html
  } else if (has_col(df, "tabsource")) { # html extracted
    df$raw_tabhtml = df$tabsource
  }
  
  if (route=="html") {
    # Changes content of all cells to pure text
    df$raw_tabhtml = sapply(df$raw_tabhtml, html_table_cells_to_text)
  }
  #df$raw_tabhtml = stri_replace_all_fixed(df$raw_tabhtml, "\u00A0", "", vectorize_all = FALSE)
  df$raw_tabhtml <- stri_replace_all_fixed(df$raw_tabhtml, "\u2003", "")
  
  
  proc_id = paste0("tab_html_hx_", route)
  proc_info = data.frame(prod_id=prod_id, proc_id = proc_id, art_source=route)
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

repbox_art_load_tabs_from_route = function(project_dir, route) {
  file = file.path(project_dir, "art", "routes", route, "arttab.Rds")
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

repbox_art_routes = function(project_dir) {
  list.dirs(file.path(project_dir, "art", "routes"), full.names = FALSE, recursive = FALSE)
}

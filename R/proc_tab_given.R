# Use tables already (heuristically) extracted by repboxDoc and stored
# in corresponding table_df.Rds in doc_dir

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
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  
  project_dir = "~/repbox/projects_share/restud_82_2_12" 
  proc_tab_given(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)
}

proc_tab_given = function(project_dir, to_v0=TRUE, doc_form=NULL, doc_type=NULL) {
  restore.point("proc_tab_given")
  doc_dirs = repboxDoc::repbox_doc_dirs(project_dir,doc_form = doc_form, doc_type=doc_type)
  doc_dir = first(doc_dirs)
  for (doc_dir in doc_dirs) {
    proc_doc_tab_given(doc_dir, to_v0=to_v0)
  }
}

proc_doc_tab_given = function(doc_dir, to_v0=TRUE) {
  restore.point("proc_doc_tab_given")
  prod_id = "tab_main"; prod = repbox_prod(prod_id)
  doc_form = rdoc_form(doc_dir)
  
  # Creates page_df.Rds, parts_df.Rds, tabs_df.Rds etc
  # if already exists skips
  rdoc_process(doc_dir,overwrite=FALSE)
  
  rdoc_is_processed(doc_dir)
  
  
  df = readRDS.or.null(file.path(doc_dir, "tab_df.Rds"))

  if (NROW(df)==0) return(NULL)
  
  just_raw=FALSE
  proc_id = doc_form
  if (doc_form=="html") {
    # Changes content of all cells to pure text
    df$raw_tabhtml = df$tabsource
    df$raw_tabhtml = sapply(df$raw_tabhtml, html_table_cells_to_text)
    just_raw = TRUE
    
  } else if (doc_form=="pdf") {
    df$raw_tabhtml = df$tab_html
    just_raw = TRUE
    proc_id = "pdf_txt"
  }
  if (just_raw) {
    df$raw_tabhtml <- stri_replace_all_fixed(df$raw_tabhtml, "\u2003", "")
    df$tabhtml = sapply(seq_len(NROW(df)), function(i) {
      html_table_add_cellnum_row_col(df$raw_tabhtml[i],tabid=df$tabid[i])
    })
  }
  #proc_id = paste0("doc_", doc_form)
  proc_info = data.frame(prod_id=prod_id, proc_id = proc_id, doc_form=doc_form)
  fp_dir = doc_dir_to_fp_dir(doc_dir)
  pru = pru_init(fp_dir,prod_id=prod_id, proc_info=proc_info, to_v0=to_v0)
  prod_df = df_to_prod_df(df, prod)
  old_tabid = prod_df$tabid
  prod_df$tabid = tabid_normalize(prod_df$tabid)
  if (!all(old_tabid==prod_df$tabid)) {
    pru = pru_add_issue(pru,"tabid_was_standardized")
  }
  
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  
  # Don't save main: just save all backports...
  #pru = pru_save(pru, prod_df)
  pru_backport_save(pru, repbox_prod("tab_list"), prod_df)
  if (doc_form != "mocr") {
    pru_backport_save(pru, repbox_prod("tab_notes"), prod_df)
  }
  pru_backport_save(pru, repbox_prod("tab_html"), prod_df)
  proc_tab_html_to_cell_list(pru=pru, prod_df=prod_df, also_cell_base = TRUE)
  #rai_write_all_tables_html(prod_df, "tables.html",out_dir = pru$ver_dir)
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)
}

# rdoc_process instead
# 
# # Think of what should be called by make_ejd
# rdoc_prepare_hx = function(doc_dir) {
#   doc_form = rdoc_form(doc_dir)
#   if (doc_form=="mocr") {
#     repboxDoc::rdoc_mocr_process(doc_dir)
#   } else if (doc_form == "pdf") {
#     rdoc_pdf_to_txt_pages(doc_dir)
#     rdoc_pdf_pages_to_parts(doc_dir)
#     rdoc_pdf_extract_tabs(doc_dir)
#   } else if (doc_form == "html") {
#     rdoc_html_to_parts(doc_dir) 
#   }
# }

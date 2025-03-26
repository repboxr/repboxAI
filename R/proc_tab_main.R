# tab_main combines tab_html and tab_notes
# not all our extraction methods generate tab_html and tab_notes
# in particular we have not yet included table notes extraction
# for mocr (mistral ocr)

# tab_main essentially just joins a tab_html and tab_notes
# per default we take notes from the same process than tab_html
# if it exists, otherwise we map the notes from our preferred AI 
# notes extraction


example = function() {
  library(repboxAI)
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  proc_tab_main(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)
}

proc_tab_main = function(project_dir, ver_ind = 0, overwrite=TRUE) {
  restore.point("proc_tab_main")
  fp_dirs = rai_fp_dirs(project_dir)
  ver_dirs = fp_all_ver_dirs(fp_dirs, "tab_html", ver_ind=ver_ind)
  
  html_df = tibble(html_ver_dir = ver_dirs, proc_id = fp_ver_dir_to_proc_id(ver_dirs), fp_dir = fp_ver_dir_to_fp_dir(ver_dirs))

  ver_dirs = fp_all_ver_dirs(fp_dirs, "tab_notes", ver_ind=ver_ind)
  notes_df = tibble(notes_ver_dir = ver_dirs, proc_id = fp_ver_dir_to_proc_id(ver_dirs),  fp_dir = fp_ver_dir_to_fp_dir(ver_dirs))
  
   
  
  # 1. Create tab main for processes which are expected to use their own note extraction
  sel_df = html_df %>% 
    filter(startsWith(proc_id, "pdf-") | proc_id %in% c("html","pdf_txt")) %>%
    left_join(notes_df, by=c("fp_dir","proc_id"))
  for (i in seq_len(NROW(sel_df))) {
    proc_one_tab_main(sel_df$fp_dir[i],sel_df$html_ver_dir[i], sel_df$notes_ver_dir[i], overwrite=overwrite)
  }
  
  # 2. Create tab main for processes that use default tab notes
  sel_df = html_df %>% 
    filter(proc_id == "mocr")
  i = 1
  for (i in seq_len(NROW(sel_df))) {
    proc_one_tab_main(sel_df$fp_dir[i],sel_df$html_ver_dir[i], overwrite=overwrite)
  }

}

proc_one_tab_main = function(fp_dir, ver_dir_html=NULL, ver_dir_notes = NULL, to_v0=TRUE, overwrite=FALSE) {
  restore.point("proc_one_main_tab")
  add_notes_id=TRUE
  if (is.null(ver_dir_notes)) {
    add_notes_id = FALSE
    ver_dir_notes = rai_pick_tab_ver(fp_dir, "tab_notes")$ver_dir
  }
  if (is.null(ver_dir_html)) {
    ver_dir_html = rai_pick_tab_ver(fp_dir, "tab_html")$ver_dir
  }
  proc_id_notes = fp_ver_dir_to_proc_id(ver_dir_notes)
  proc_id_html = fp_ver_dir_to_proc_id(ver_dir_html)
  short_html = proc_id_html %>%
    stri_replace_all_regex("(tab_html_doc_)|(cell_base-n-)|(_tab_html-n-)","")
  short_notes = proc_id_notes %>%
    stri_replace_all_regex("(tab_html_doc_)|(cell_base)|(tab_notes-j-)","")

  proc_id = ifelse(short_html==short_notes | !add_notes_id, short_html, paste0(short_html, "-", short_notes))
  
  fp_dir = fp_ver_dir_to_fp_dir(ver_dir_html)
  proc_dir = file.path(fp_dir, "tab_main",proc_id)
  ver_dir = fp_proc_dir_to_new_ver_dir(proc_dir,to_v0 = to_v0)
  if (!overwrite) {
    if (fp_has_prod_df(ver_dir)) {
      return(invisible())
    }
  }
  
  
  tab_html = fp_load_prod_df(ver_dir_html)
  tab_notes = fp_load_prod_df(ver_dir_notes)
  df = left_join_overwrite(tab_html, tab_notes, by ="tabid")
  prod_df = df_to_prod_df(df, repbox_prod("tab_main"))
  fp_save_prod_df(prod_df, ver_dir)
  rai_write_all_tables_html(prod_df, "tables.html",out_dir = ver_dir)
  
  invisible(list(ver_dir=ver_dir, prod_df=prod_df))
}


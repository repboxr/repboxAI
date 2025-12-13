# Generate typical media and context used in rebox analyses
example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  outfile = rai_media_all_static_do(project_dir)
  tab_main = rai_pick_tab_main(project_dir,"art")$tab_main
  outfile = rai_media_all_tab_html(project_dir, tab_main)
  rstudioapi::filesPaneNavigate(dirname(outfile))
}

rai_media_all_static_do = function(project_dir, script_df = rai_load_do_source(project_dir)) {
  restore.point("rai_make_all_static_do")
  if (!has_col(script_df,"script_file")) {
    script_df = script_df_shorten_file(script_df)
  }
  
  txt = paste0(
    paste(rep("*", 60), collapse=""),"\n",
    script_df$script_file,"\n",
    paste(rep("*", 60), collapse=""),"\n\n",
    "Below is the code of ", script_df$file_path, " with line numbers\n\n",
    add_line_numbers_to_code(script_df$text),
    "\n"
  )
  outfile = rai_save_prompt_media_file(txt, project_dir, base="static_do.txt")
  outfile
  
}

add_line_numbers_to_code = function(code, pad_len) {
  if (length(code)>1) {
    return(sapply(code, add_line_numbers_to_code))
  }
  code = sep.lines(code)
  pad_len = max(2, ceiling(log(length(code)+1,10)))
  paste0(
    stri_pad_right(seq_along(code),pad_len),
    ": ",
    code,
    collapse="\n"
  )
}

rai_media_all_tab_html = function(project_dir, tab_main, doc_type="art", base=NULL) {
  restore.point("rai_make_all_tab_html")
  fp_dir = rai_fp_dir(project_dir, doc_type)
  
  tab_df = tab_main
  title_col = "tabtitle"; notes_col = "tabnots"
  tabtitles =   
  html = paste0(paste0("<h2>",tab_df$tabtitle, "</h2>", tab_df$tabhtml, "<p>", na_val(tab_df$tabnotes,""),"</p>"))
  
  html = paste0(html, collapse="\n")
  
  if (is.null(base)) {
    base = paste0(doc_type,"_tabs.html")
  }
  outfile = rai_save_prompt_media_file(html, project_dir, base)
  outfile
}

rai_media_tab_html = function(project_dir, tabid, tab_main, all_ref_li=NULL, all_part_df = NULL, outfile=NULL, doc_type="art") {
  restore.point("rai_make_tab_prompt_html")
  fp_dir = rai_fp_dir(project_dir, doc_type)
  
  tab_main = tab_df = tab_main[tab_main$tabid == tabid,]
  title_col = "tabtitle"; notes_col = "tabnots"
  tabtitles =   
    tab_html = paste0(paste0("<h2>",tab_df$tabtitle, "</h2>", tab_df$tabhtml, "<p>", na_val(tab_df$tabnotes,""),"</p>"))
  
  ref_txt = NULL
  if (!is.null(all_ref_li)) {
    ref_txt = sapply(seq_along(all_ref_li), function(i) {
      paste0(repboxDoc::rdoc_tab_ref_text(tabid = tabid, ref_li = all_ref_li[[i]],part_df = all_part_df[[i]],sep_str = "<p>[...]</p>")$text, collapse="\n")
    })
    ref_txt = ref_txt[nchar(ref_txt)>0]
    ref_txt = paste0(ref_txt, collapse = "<p>[...]</p>")
  }
  if (length(ref_txt)>0) {
    ref_html = paste0("<p>", ref_txt, "</p>")
    html = paste0(tab_html, "\n<h2>Parts in the text that reference to the table </h2>", ref_html)
  } else {
    html = tab_html
  }
  html = paste0(html, collapse="\n")
  
  if (!is.null(outfile)) {
    outdir = dirname(outfile)
    if (!dir.exists(outdir)) dir.create(outdir,recursive = TRUE)
    writeUtf8(html, outfile)
    
  }
  invisible(html)
}


rai_save_prompt_media_file = function(txt, project_dir, base, ind=NULL) {
  restore.point("rai_save_prompt_media_file")
  dir = file.path(project_dir,"fp","prompt_files")
  if (!dir.exists(dir)) dir.create(dir)
  if (is.null(ind)) {
    outfile = paste0(dir, "/", base)
  } else {
    outfile = paste0(dir, "/", tools::file_path_sans_ext(base),"--", ind, tools::file_ext(base))
  }
  writeUtf8(paste0(txt, collapse="\n"), outfile)
  outfile
}

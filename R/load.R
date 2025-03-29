rai_file_cache = function(...) {
  new.env(parent=emptyenv())
}


rai_pick_tab_ver = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=NULL) {
  restore.point("rai_pick_tab_ver")
  if (is.null(pref)) {
    if (prod_id == "tab_notes") {
      pref = fp_pref(proc_regex = c("html","^pdf-g2f.*"))
    } else {
      pref = fp_pref(proc_regex = c("html", "mocr","pdf-.*","pdf_txt"))
      
    }
  }
  fp_pick_prod_ver(fp_dir, prod_id=prod_id,proc_id=proc_id, pref=pref)
}



rai_load_tab_prod_df = function(fp_dir, prod_id = "tab_html", pref=NULL, proc_id=NULL, cache=NULL) {
  restore.point("rai_load_tab_prod_df")
  ver_dir = rai_pick_tab_ver(fp_dir, prod_id, pref, proc_id)$ver_dir
  file = file.path(ver_dir, "prod_df.Rds")
  if (!is.null(cache)) {
    prod_df = cache[[file]]
    if (!is.null(prod_df)) return(prod_df)
  }
  prod_df = fp_load_prod_df(ver_dir)
  if (!is.null(cache)) {
    cache[[file]] = prod_df
  }
  prod_df
}

rai_pick_ref_li_doc_dir = function(project_dir, doc_type="art", tab_ref_pref = tab_ref_default_pref()) {
  restore.point("rai_load_ref_li")
  files = paste0(project_dir, "/doc/", doc_type, "_", tab_ref_pref, "/ref_li.Rds")
  files = files[file.exists(files)]
  if (length(files)==0) return(NULL)
  file = files[[1]]
  dirname(file)
}

doc_file_form_default_pref = function() {
  c("html","pdf","mocr_md", "pdf_txt")
}

tab_main_default_pref = function() {
  c("html","pdf-g*","mocr","pdf_txt")
}

tab_ref_default_pref = function() {
  c("html", "mocr","pdf")
}


rai_doc_file = function(project_dir, doc_type, pref = doc_file_form_default_pref(), doc_form=NULL) {
  repbox_doc_file_select(project_dir, doc_type = doc_type, doc_file_form_pref = pref, doc_form=doc_form)$doc_file
}

rai_make_tab_prompt_html = function(project_dir, tabid, tab_main, all_ref_li=NULL, all_part_df = NULL, outfile=NULL, doc_type="art") {
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

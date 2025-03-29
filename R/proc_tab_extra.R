# Extra classifications on a table level
example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash")
  
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  proc_by_tab_extra(project_dir, "tab_classify", add_doc=TRUE, add_tab_main = TRUE, add_tab_ref=TRUE, doc_type="art")
  proc_by_tab_extra(project_dir, "tab_classify", add_doc=FALSE, add_tab_main = TRUE, add_tab_ref=TRUE, doc_type="art")
  proc_tab_extra(project_dir, "tab_classify", doc_type="art")
  rstudioapi::filesPaneNavigate(pru$ver_dir)
}


#' Extracts tab_html from articles
proc_by_tab_extra = function(project_dir, prod_id = c("tab_classify")[1], doc_type="art", doc_file_form_pref = doc_file_form_default_pref(), tab_main_pref = tab_main_default_pref(), tab_ref_pref = tab_ref_default_pref(), add_doc = TRUE, add_tab_main=TRUE, add_tab_ref = TRUE,  ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE,  use_schema = FALSE, overwrite=TRUE) {
  restore.point("proc_by_tab_extra")
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  
  postfix = ""
  if (!add_doc) {
    postfix = paste0(postfix, "_nodoc")
  }
  if (!add_tab_main) {
    add_tab_ref  = FALSE
    tpl_id = paste0(postfix, "_notab")
  } else if (!add_tab_ref) {
    tpl_id = paste0(postfix, "_noref")
  }
  tpl_id = paste0("by_",prod_id, postfix)
  
  tpl_file = file.path(rai_tpl_dir(), paste0(tpl_id,".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema, tpl_id=tpl_id,proc_id_prefix = "bytab_", proc_id_postfix = postfix)
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0, ai_opts=ai_opts, tab_main_pref=tab_main_pref, tab_ref_pref=tab_ref_pref, add_tab_main=add_tab_main, add_tab_ref=add_tab_ref, project_dir=project_dir, doc_type=doc_type)
  
  
  pru$tab_main_info = fp_pick_prod_ver(fp_dir, "tab_main", pref=tab_main_pref)
  pru$tab_main = fp_load_prod_df(pru$tab_main_info$ver_dir)
  # references will be loaded for all documents
  pru$doc_types = doc_types = repbox_doc_types(project_dir)
  if (add_tab_ref) {
    pru$ref_li_doc_dirs = sapply(doc_types, function(dt) {
      rai_pick_ref_li_doc_dir(project_dir,doc_type=dt,tab_ref_pref)
    })
    
    if (length(pru$ref_li_doc_dir)>0) {
      pru$ref_li_form = rdoc_form(pru$ref_li_doc_dirs[[1]])
      pru$all_ref_li = lapply(pru$ref_li_doc_dirs, rdoc_load_ref_li)
      pru$all_part_df = lapply(pru$ref_li_doc_dirs,rdoc_load_part_df)
    }
  } else {
    pru$all_ref_li = NULL
  }
  
  
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  if (add_doc) {
    # We add always all documents: article and possible appendices
    pru$doc_files = rai_doc_file(project_dir,doc_type = NULL,pref = doc_file_form_pref)
    pru$context = rai_context(project_dir,model = ai_opts$model,media_files = pru$doc_files)
  } else {
    pru$context = NULL
  }
  pru_next_stage(pru, "proc_by_tab_extra_run")
}

proc_by_tab_extra_run = function(pru) {
  restore.point("pru_by_tab_extra_run")
  project_dir = pru$project_dir
  df = pru$tab_main
  
  if (length(pru$doc_types)<=1) {
    doc_type_descr = "scientific article"
  } else if (length(pru$doc_types)==2) {
    doc_type_descr = "scientific article and online appendix"
  } else {
    doc_type_descr = paste0("scientific article and ", length(pru$doc_types)-1," online appendices")
  }
  
  # base name of table html file that will be added as a media file
  tab_html_base = paste0(pru$doc_type,"-", pru$tab_main_info$proc_id)
  if (!is.null(pru[["all_ref_li"]])) {
    tab_html_base = paste0(tab_html_base,"__", pru$ref_li_form)
  }
  row = 1
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    restore.point("pru_tab_overview_run_inner")
    media_files = NULL
    tabid = df$tabid[[row]]
    if (pru$add_tab_main) {
      tab_html_file = file.path(project_dir,"fp","prompt_files",paste0(tab_html_base, "--",row,".html"))
      html = rai_make_tab_prompt_html(pru$project_dir,tabid = tabid, tab_main = df, all_ref_li=pru[["all_ref_li"]], all_part_df = pru[["all_part_df"]], outfile=tab_html_file)
      if (file.exists(tab_html_file)) {
        media_files = tab_html_file
      }
    }
    
    rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = media_files,context = pru$context)
    
    values = list(doc_type_descr = doc_type_descr, tabtitle = df$tabtitle[row])
    res = ai_run(rai, values=values)
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("post_pru_by_tab_extra_run")
  if (!pru_is_ok(pru)) return(invisible(pru))
  
  prod = repbox_prod(pru$prod_id)
  rclasses = prod_to_rclasses(prod)
  res_df = ai_combine_content_df(pru$items, df %>% select(tabid),rclasses = rclasses)
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  return(invisible(pru))
}



#' Extracts tab_html from articles
proc_tab_extra = function(project_dir, prod_id = c("tab_classify")[1], doc_type="art", doc_file_form_pref = doc_file_form_default_pref(), tab_main_pref = tab_main_default_pref(), ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, tpl_id = paste0(prod_id), use_schema = FALSE, overwrite=TRUE,...) {
  restore.point("proc_tab_extra")
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  
  tpl_file = file.path(rai_tpl_dir(), paste0(tpl_id,".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema, tpl_id=tpl_id)
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0, ai_opts=ai_opts, tab_main_pref=tab_main_pref, project_dir=project_dir, doc_type=doc_type)
  
  pru$doc_types = doc_types = repbox_doc_types(project_dir)
  
  pru$tab_main_info = fp_pick_prod_ver(fp_dir, "tab_main", pref=tab_main_pref)
  pru$tab_main = fp_load_prod_df(pru$tab_main_info$ver_dir)

  
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  if (add_doc) {
    # We add always all documents: article and possible appendices
    pru$doc_files = rai_doc_file(project_dir,doc_type = NULL,pref = doc_file_form_pref)
    pru$context = rai_context(project_dir,model = ai_opts$model,media_files = pru$doc_files)
  } else {
    pru$context = NULL
  }
  pru_next_stage(pru, "proc_tab_extra_run")
}

proc_tab_extra_run = function(pru) {
  restore.point("proc_tab_extra_run")
  project_dir = pru$project_dir
  df = pru$tab_main

  if (length(pru$doc_types)<=1) {
    doc_type_descr = "scientific article"
  } else if (length(pru$doc_types)==2) {
    doc_type_descr = "scientific article and an online appendix"
  } else {
    doc_type_descr = paste0("scientific article and ", length(pru$doc_types)-1," online appendices")
  }
  if (pru$doc_type=="art") {
    cur_doc = "the article"
  } else if (length(pru$doc_types)<=2) {
    cur_doc = "the online appendix"
  }  else {
    cur_doc = "an online appendix"
  }
  
  tablist = text_table(pru$tab_main[c("tabid", "tabtitle")], header=c("tabid","tab_title"))
  
  # cat(tablist)
  
  rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = NULL,context = pru$context)
    
  values = list(doc_type_descr = doc_type_descr, tablist=tablist, cur_doc=cur_doc)
  res = ai_run(rai, values=values)
  pru$items = list(res)
  pru = pru_set_status(pru, pru$items)
  
  restore.point("post_pru_tab_extra_run")
  if (!pru_is_ok(pru)) return(invisible(pru))
  
  prod = repbox_prod(pru$prod_id)
  rclasses = prod_to_rclasses(prod)
  res_df = ai_combine_content_df(pru$items, df %>% select(tabid),rclasses = rclasses)
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  return(invisible(pru))
}


text_table = function(df, header = cols, col_sep = "|", cols=names(df), header_line_char="-", header_line_col_sep="+") {
  restore.point("text_table")
  txt = rep("", NROW(df)+2)
  i = 1
  for (i in seq_along(cols)) {
    col = cols[[i]]
    val = as.character(df[[col]])
    nc = max(nchar(c(header[i],val)))
    val_str = stri_pad_right(val, nc+1)
    header_str = stri_pad_right(header[i],nc+1)
    add_left = (i>1)
    if (add_left) {
      val_str = paste0(col_sep, " ", val_str)
      header_str = paste0(col_sep, " ", header_str)
    }
    header_line = NULL
    if (nchar(header_line_char)>0) {
      header_line = paste0(rep(header_line_char,nc+1+add_left), collapse="")
      if (add_left) {
        header_line = paste0(header_line_col_sep, header_line)
      }
    }
    txt = paste0(txt, c(header_str, header_line, val_str))
  }
  paste0(txt, collapse="\n")
}

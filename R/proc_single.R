# Process that uses a single ai call for the whole document / code base
example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  set_ai_opts(model = "gemini-2.0-flash")
  
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  proc_single(project_dir,doc_type="art", "map_reg_static", add_all_doc = TRUE, add_all_tab = TRUE,add_all_static_do = TRUE)
  rstudioapi::filesPaneNavigate(project_dir)
  
  
}

#' Extracts tab_html from articles
proc_single = function(project_dir, prod_id = c("tab_classify")[1], doc_type="art", doc_file_form_pref = doc_file_form_default_pref(), tab_main_pref = tab_main_default_pref(), add_all_doc=TRUE, add_all_tab = FALSE, add_all_static_do = FALSE, load_tab_main=TRUE, load_do_source = add_all_static_do, ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, tpl_id = paste0(prod_id), use_schema = FALSE, overwrite=TRUE,...) {
  restore.point("proc_single")
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  
  tpl_file = file.path(rai_tpl_dir(), paste0(tpl_id,".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema, tpl_id=tpl_id)
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0, ai_opts=ai_opts, project_dir=project_dir, doc_type=doc_type, doc_file_form_pref =  doc_file_form_pref, tab_main_pref = tab_main_pref, add_all_doc=add_all_doc, add_all_tab = add_all_tab, add_all_static_do = add_all_static_do, load_tab_main=load_tab_main, load_do_source = load_do_source)

  
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  
  pru$doc_types = repbox_doc_types(project_dir)
  
  load_tab_main = load_tab_main & add_all_tab
  if (load_tab_main) {
    pru = rai_pick_tab_main(project_dir, doc_type, tab_main_pref,pru = pru)
  }
  
  load_do_source = load_do_source | add_all_static_do
  if (load_do_source) {
    pru$do_df = rai_load_do_source(project_dir) 
  }
  pru$doc_files = rai_doc_file(project_dir,doc_type = NULL,pref = doc_file_form_pref)

  media_files = NULL
  if (add_all_doc) {
    media_files = c(media_files, pru$doc_files)
  }
  if (add_all_tab) {
    base = paste0(doc_type, "_tabs-", pru$tab_main_info$proc_id,".html")
    outfile = rai_media_all_tab_html(project_dir,pru$tab_main,doc_type, base)
    media_files =c(media_files, outfile)
  }
  if (add_all_static_do) {
    outfile = rai_media_all_static_do(project_dir,script_df = pru$do_df)
    media_files =c(media_files, outfile)
  }
  if (length(media_files)>0) {
    pru$context = rai_context(project_dir,model = ai_opts$model,media_files = media_files)
  }
  
  pru_next_stage(pru, "proc_single_run")
}

proc_single_run = function(pru) {
  restore.point("proc_single_run")
  project_dir = pru$project_dir
  values = list()

  if (length(pru$doc_types)<=1) {
    values$doc_type_descr = "scientific article"
  } else if (length(pru$doc_types)==2) {
    values$doc_type_descr = "scientific article and an online appendix"
  } else {
    values$doc_type_descr = paste0("scientific article and ", length(pru$doc_types)-1," online appendices")
  }
  if (pru$doc_type=="art") {
    values$cur_doc = "the article"
  } else if (length(pru$doc_types)<=2) {
    values$cur_doc = "the online appendix"
  }  else {
    values$cur_doc = "an online appendix"
  }
  
  if (!is.null(pru[["tab_main"]]))
    values = rai_prompt_value_tablist(pru$tab_main, values)
  
  if (!is.null(pru$do_df)) {
    values = rai_prompt_value_dolist(pru$do_df, values)
  }

  rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = NULL,context = pru$context)
    
  res = ai_run(rai, values=values)
  pru$items = list(res)
  pru = pru_set_status(pru, pru$items)
  
  restore.point("post_single_rai_run")
  temp_pru_save(pru); #pru = temp_pru
  if (!pru_is_ok(pru)) return(invisible(pru))
  prod = repbox_prod(pru$prod_id)
  schema = prod_to_schema(prod, "obj")
  res_df = ai_combine_content_df(pru$items,schema=schema)
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  return(invisible(pru))
}


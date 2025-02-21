# Parse tab_base

example = function() {
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  prods = repbox_prods()
  ai_extract_tab_title_notes_pdf(project_dir, prods)
}

ai_extract_tab_title_notes_pdf = function(project_dir, prods=repbox_fp_prods(), ai_opts = get_ai_opts(), tpl_num=1,use_schema=FALSE) {
  restore.point("ai_extract_tab_title_notes")
  stop()
  pid = "tab_title_notes"; prod = prods[[pid]]
  art_source = "pdf"

  tpl_file = file.path(repbox_ai_tpl_dir(), paste0(pid, "-", art_source, "-", tpl_num, ".txt"))
  version = rai_version(pid, ai_opts=ai_opts,tpl_file=tpl_file,use_json=TRUE, use_schema=use_schema, art_source="pdf", tpl_num=tpl_num)

  # In the prompt we name variables differently to make it easier for AI
  schema = NULL
  prod_to_df_cols = c(table_number="tabid",table_title="tabtitle", table_notes="tabnotes")
  if (use_schema) {
    schema = prod_to_schema(prod, "arr") %>%
      schema_reduce(prod_to_df_cols)
  }

  media_files = repbox_art_pdf_file(project_dir)
  values = list()
  rai = rai_init(project_dir, version=version, schema = schema, media_files = media_files)
  rai = rai_run(rai, values=values)
  prod_df = df_to_prod_df(rai$content, prod, prod_to_df_cols = prod_to_df_cols)
  rai_store_prod_run(rai, prod_df)

  return(invsible(list(rai=rai, prod_df=prod_df)))
}




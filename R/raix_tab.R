# Parse tab_base

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  prods = repbox_prods()
  res = raix_tab_tino_pdf(project_dir, prods=prods)
  res = raix_tab_html_pdf(project_dir, prods=prods)
}

#' Extracts tab_tino from articles
raix_tab_html_pdf = function(project_dir, tpl_num=1,prods=repbox_prods(), ai_opts = get_ai_opts()) {
  restore.point("raix_tab_html_pdf")
  stop()
  pid = "tab_html"; prod = prods[[pid]]
  art_source = "pdf"

  tpl_file = file.path(repbox_ai_tpl_dir(), paste0(pid, "-", art_source, "-", tpl_num, ".txt"))
  version = rai_version(pid, ai_opts=ai_opts,tpl_file=tpl_file,json_mode =TRUE, use_schema=FALSE, art_source="pdf", tpl_num=tpl_num)


  df = rai_load_prod_df(project_dir=project_dir, pid = "tab_tino")
  if (is.null(df)) {
    stop("No version of the intermediate product tab_tino available.")
  }
  media_files = repbox_art_pdf_file(project_dir)
  rai = rai_init(project_dir, version=version, media_files = media_files)

  # Temporal for testing computation
  df = df[1,]
  # REMOVE LINE ABOVE!!!!!
  
  tab_rows = seq_len(NROW(df))
  rai_li = lapply(tab_rows, function(tab_row) {
    values = as.list(df[tab_row,])
    rai = rai_run(rai, values=values)
  })
  rais = rais_init(rai_li,df = df)
  rais = rais_combine_content(rais, var="raw_html")
  df = rais$df
  df$tabhtml = sapply(df$raw_html,html_table_add_cellnum_row_col)
  prod_df = df_to_prod_df(df, prod)
  rais_save(rais, prod_df)
  #rstudioapi::filesPaneNavigate(rais$run_dir)

  return(invisible(rais))
}




#' Extracts tab_tino from articles
raix_tab_tino_pdf = function(project_dir, tpl_num=1,use_schema=FALSE,  prods=repbox_prods(), ai_opts = get_ai_opts()) {
  restore.point("raix_tab_tino_pdf")
  stop()
  pid = "tab_tino"; prod = prods[[pid]]
  art_source = "pdf"

  tpl_file = file.path(repbox_ai_tpl_dir(), paste0(pid, "-", art_source, "-", tpl_num, ".txt"))
  version = rai_version(pid, ai_opts=ai_opts,tpl_file=tpl_file,json_mode =TRUE, use_schema=use_schema, art_source="pdf", tpl_num=tpl_num)

  # In the prompt we name variables differently to make it easier for AI
  schema = NULL
  prod_to_df_cols = c(table_number="tabid",table_title="tabtitle", table_notes="tabnotes")
  if (use_schema) {
    schema = prod_to_schema(prod, "arr") %>%
      schema_reduce(prod_to_df_cols)
  }

  media_files = repbox_art_pdf_file(project_dir)
  rai = rai_init(project_dir, version=version, schema = schema, media_files = media_files)
  rai = rai_run(rai)
  prod_df = df_to_prod_df(rai$content, prod, prod_to_df_cols = prod_to_df_cols)
  prod_df$otabid = tabid_to_otabid(prod_df$tabid)
  rais = rais_init(rai, to_r0=TRUE)
  rais = rais_save(rais, prod_df)
  rstudioapi::filesPaneNavigate(rais$run_dir)

  return(invisible(list(rai=rai, prod_df=prod_df, run_dir=run_dir)))
}


tabid_to_otabid <- function(tabid, digits = 3) {
  # Vectorized extraction of the first sequence of digits
  matches <- stri_extract_first_regex(tabid, "[0-9]+")

  # For each non-NA match, create a padded version
  padded <- ifelse(is.na(matches), NA, sprintf(paste0("%0", digits, "d"), as.numeric(matches)))

  # Replace the first occurrence of digits with the padded number where a match was found
  result <- ifelse(is.na(matches), tabid, stri_replace_first_regex(tabid, "[0-9]+", padded))

  return(result)
}


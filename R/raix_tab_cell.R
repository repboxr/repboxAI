# Alternative attempt to extract tables using JSON spec
# instead of HTML table as desired output.
# Yet does not work so well...

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-1.5-flash-001")
  set_ai_opts(model = "gemini-2.0-flash-lite")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  get_ai_opts()
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  res = raix_tab_cells_pdf(project_dir)
}

raix_tab_cells_pdf = function(project_dir, tpl_num=1,prods=repbox_prods(), ai_opts = get_ai_opts(), verbose=TRUE) {
  restore.point("raix_tab_cells_pdf")
  stop()
  pid = "tab_html"; prod = prods[[pid]]
  art_source = "pdf"
  
  tpl_file = file.path(rai_tpl_dir(), paste0("tab-cells-", art_source, "-", tpl_num, ".txt"))
  version = rai_version("tab-cells", ai_opts=ai_opts,tpl_file=tpl_file,json_mode = TRUE, use_schema=FALSE, art_source="pdf", tpl_num=tpl_num)
  version$pid = pid
  
  inputs = rai_pick_input_prod(project_dir, c("tab_tino"))
  if (input_prod_err(inputs)) return(NULL)
  
  context = repbox_ai_context(project_dir, add_art_pdf = TRUE)
  rai = rai_init(project_dir, version=version, context=context)
  
  df = rai_load_input_prods(inputs)[[1]]
  # For faster testing
  df = df[2,]
  
  tab_rows = seq_len(NROW(df))
  if (verbose) cat("\nExtract ", NROW(df), " tables from PDF ")
  rai_li = lapply(tab_rows, function(tab_row) {
    if (verbose) cat(".")
    values = as.list(df[tab_row,])
    rai = rai_run(rai, values=values)
  })
  restore.point("post_run")
  rais = rais_init(rai_li,df = df, input_info=inputs)
  if (rais_backup_if_incomplete(rais)) return(NULL)
  
  rais = rais_combine_content(rais, var="raw_html")
  df = rais$df
  df$tabhtml = sapply(df$raw_html,html_table_add_cellnum_row_col)
  prod_df = df_to_prod_df(df, prod)
  rais_save(rais, prod_df)
  rai_write_all_tables_html(prod_df, "tables.html", run_dir=rais$run_dir)
  
  #rstudioapi::filesPaneNavigate(rais$run_dir)
  
  return(invisible(rais))
}


tab_cells_schema=function() {
  schema = schema_obj(list(
    tabtitle = schema_str("Table title"),
    columns = schema_arr(
      descr = "List of all columns shown in the main part of the table. Not every row may have a value for every column and column titles may often span more than one column. Different numbers should be in different columns.",
      items = schema_obj(list(
        col = schema_int("Number of the column. Start with 1 for the first column."),
        col_desc = schema_str("A short description what is shown in the column", maxLength=100)
      ))
    ),
    cells = schema_arr(
      descr = "Add an item for every table cell. Go through the table row-by-row and column-by-column and add the cells. Every separate line in the original is a separate row. Create separate cells for separate rows, even if you think that the text content could be merged to a single cell.",
      items = schema_obj(list(
        row = schema_int("The row in the table. Remember every line of text or numbers in the original table is a separate row."),
        col = schema_int("The column of the cell. If it is a title column that spans more than one column enter the first column and set the colspan argument correspondingly."),
        colspan = schema_int("Will usually be 1 except for title cells that span more than one column."),
        text = schema_str("The cell content as text EXACTLY as given in the original table."),
        content_type = schema_str("Pick a content type that best describes this cell. Often cells contain numbers in brackets like (4.31). Classify them also as number", enum=c("number","label_title","other"))
      ))
    )
  ))
  library(DataSchema)
  json = toJSON(to_json_schema(schema, add_description = TRUE), auto_unbox = TRUE,pretty=TRUE)
  cat(json)
}


example = function() {
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  ver_dirs = repbox_process_all_manual(project_dir, prod_id = "reg_classify")
  rstudioapi::filesPaneNavigate(project_dir)
}

#' Export a prompt as a text file including the content of all media files
rai_pru_export_manual_prompt = function(pru, rai, values, row = NULL, view=TRUE) {
  restore.point("rai_pru_export_manual_prompt")
  
  if (is.null(rai[["prompt"]])) 
    rai = ai_glue(rai, values)
 
  # 2. Collect content from media files
  all_media = unique(c(pru$context_media_files, rai$media_files))
  
  media_content = ""
  for (f in all_media) {
    if (!file.exists(f)) next
    ext = tools::file_ext(f)
    # Only textualize formats we can easily read
    if (ext %in% c("txt", "html", "md", "do", "csv", "json")) {
      content = paste0(readLines(f, warn = FALSE), collapse = "\n")
      media_content = paste0(
        media_content, 
        "\n\n# FILE: ", basename(f), "\n",
        "\n\n",
        content, "\n",
        "\n\n# END FILE: ", basename(f), "\n"
      )
    }
  }
  
  full_prompt = paste0(
    "#INSTRUCTIONS:\n\n", rai[["prompt"]], 
    "\n\n#CONTENT OF ATTACHED FILES:\n", media_content
  )
  
  fname = paste0("full_prompt", row, ".txt")
  prompt_file = file.path(pru$ver_dir, fname)
  if (!dir.exists(pru$ver_dir)) dir.create(pru$ver_dir, recursive=TRUE)
  writeUtf8(full_prompt, prompt_file)
  
  resp_file = file.path(pru$ver_dir, paste0("rai_resp",row,".md"))
  if (!file.exists(resp_file)) writeLines(paste0("INSERT AI RESPONSE FOR ", pru$ver_dir, " HERE."), resp_file)
  
  
  if (view) {
    rstudioapi::navigateToFile(resp_file)
    rstudioapi::navigateToFile(prompt_file)
  }
  invisible(fname)
}

#' Process manual responses for a specific version directory
rai_pru_process_manual = function(ver_dir) {
  restore.point("rai_pru_process_manual")
  pru_file = file.path(ver_dir, "temp_pru.Rds")
  if (!file.exists(pru_file)) {
    # Try outage_pru or error_pru if pru.Rds doesn't exist yet
    pru_file = list.files(ver_dir, pattern = ".*pru.Rds", full.names = TRUE)[1]
  }
  if (is.na(pru_file)) stop("No pru object found in ", ver_dir)
  
  pru = readRDS(pru_file)
  
  # Look for responses. If itemized, we look for rai_resp_1.md, rai_resp_2.md etc.
  num_items = length(pru$items)
  
  i = 1
  for (i in seq_along(pru$items)) {
    resp_file = if (num_items == 1) "rai_resp.md" else paste0("ai_resp_", i, ".md")
    resp_path = file.path(ver_dir, resp_file)
    
    if (!file.exists(resp_path)) {
      stop(paste0("Response file not found: ", resp_file))
    }
    
    resp_text = paste0(readLines(resp_path, warn = FALSE), collapse = "\n")
    
    
    
    
    # Extract JSON or standard content from the markdown response
    # FuzzyProduction usually has helpers for this
    content = rai_extract_content_from_ai_resp(resp_text, json_mode = pru$proc_info$json_mode)
    
    pru$items[[i]] = list(
      success = TRUE,
      content = content,
      raw_response = resp_text,
      manual = TRUE
    )
  }
  
  # Finalize the PRU
  pru = pru_set_status(pru, pru$items)
  if (!pru_is_ok(pru)) stop("Validation failed for manual response.")
  
  prod = repbox_prod(pru$prod_id)
  schema = prod_to_schema(prod, "obj")
  
  res_df = ai_combine_content_df(pru$items, schema = schema)
  prod_df = df_to_prod_df(res_df, prod)
  
  pru_save(pru, prod_df)
  cat("\nSuccessfully processed manual version: ", ver_dir)
  return(invisible(pru))
}

rai_extract_content_from_ai_resp = function(txt, json_mode=FALSE) {
  restore.point("rai_extract_content_from_ai_resp")
  if (!json_mode) return(ai_resp_txt)
  start_pos = stri_locate_first_regex(txt,pattern = "(\\[|\\{)")[,1]
  end_pos = stri_locate_last_regex(txt,pattern = "(\\]|\\})")[,2]
  json = stri_sub(txt,start_pos,end_pos)
  jsonlite::fromJSON(json)
  
}

#' Scan project for manual versions that have responses and process them
repbox_process_all_manual = function(project_dir, prod_id=NULL, overwrite=FALSE) {
  restore.point("repbox_process_all_manual")
  fp_dirs = rai_fp_dirs(project_dir)
  # Find all ver_dirs that have a 'man_' prefix in their path
  
  resp_file = "rai_resp.md"
  resp_files = list.files(fp_dirs, "((rai_resp[0-9]*\\.md)|(rai_resp[0-9]*\\.json))$",full.names = TRUE, recursive = TRUE)
  ver_dirs = dirname(resp_files)
  
  if (!is.null(prod_id)) {
    ver_dirs = ver_dirs[has.substr(ver_dirs, paste0("/", prod_id,"/"))]
  }
  
  
  if (!overwrite) {
    prod_files = file.path(ver_dirs, "prod_df.Rds")
    ver_dirs = ver_dirs[!file.exists(prod_files)]
  }

  vd = ver_dirs[1]  
  for (vd in ver_dirs) {
    rai_pru_process_manual(vd)
  }
}

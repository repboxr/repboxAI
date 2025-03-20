# Parse tab_base

example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  get_ai_opts()
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  
    
  res = proc_readme(project_dir,"readme_var", overwrite=FALSE)

  rstudioapi::filesPaneNavigate(project_dir)
  rstudioapi::filesPaneNavigate(res$ver_dir)
  
  # Analyse readme overview
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  readme_df = fp_pick_and_load_prod_df(project_dirs,prod_id = "readme_overview")
    
}

#' Extracts tab_html from articles
proc_readme_overview = function(project_dir, ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, use_schema = FALSE, overwrite=TRUE) {
  restore.point("proc_readme_overview")
  library(repboxReadme)
  readme_df = repbox_load_or_make_readme_ranks(project_dir)
  if (NROW(readme_df)==0) {
    cat("\nNo readme files found for ", project_dir, ".\n")
    return(NULL)
  }
  readme_df = readme_df %>% filter(!ignore)
  fp_dir = file.path(project_dir, "fp", "readme") 
    
  prod_id = "readme_overview"
  tpl_file = file.path(rai_tpl_dir(), paste0("readme_overview.txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema)

  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)
  
  pru$readme_df = readme_df
  pru$project_dir = project_dir
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  pru_next_stage(pru, "pru_readme_overview_run")
}

pru_readme_overview_run = function(pru) {
  restore.point("pru_readme_overview_run")
  project_dir = pru$project_dir
  df = pru$readme_df
  row = 1
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    restore.point("pru_readme_overview_run_inner")
    readme_file = file.path(project_dir, "readme", "txt", df$txt_file[row])
    rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = readme_file)
    values = list(readme_file = basename(df$org_file[row]))
    res = ai_run(rai, values=values)
    res
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("pru_readme_overview_run")
  if (!pru_is_ok(pru)) return(invisible(pru))

  res_df = ai_combine_content_df(pru$items, df %>% select(readme_file = org_file))
  prod_df = df_to_prod_df(res_df, repbox_prod("readme_overview"))
  pru_save(pru, prod_df)
  return(invisible(pru))
}


#' Extracts tab_html from articles
proc_readme = function(project_dir, prod_id = c("readme_overview","readme_var","readme_map")[1], ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, use_schema = FALSE, overwrite=TRUE) {
  restore.point("proc_readme")
  library(repboxReadme)
  readme_df = repbox_load_or_make_readme_ranks(project_dir)
  if (NROW(readme_df)==0) {
    cat("\nNo readme files found for ", project_dir, ".\n")
    return(NULL)
  }
  readme_df = readme_df %>% filter(!ignore)
  fp_dir = file.path(project_dir, "fp", "readme") 
  
  tpl_file = file.path(rai_tpl_dir(), paste0(prod_id,".txt"))
  
  proc_info = rai_make_proc_info(prod_id=prod_id,ai_opts = ai_opts,tpl_file = tpl_file, json_mode=TRUE, use_schema = use_schema)
  
  pru = pru_init(fp_dir,prod_id,proc_info=proc_info,to_v0=to_v0)
  
  pru$readme_df = readme_df
  pru$project_dir = project_dir
  if (!overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)
  pru_next_stage(pru, "pru_readme_run")
}

pru_readme_run = function(pru) {
  restore.point("pru_readme_run")
  project_dir = pru$project_dir
  df = pru$readme_df
  row = 1
  pru = pru_make_items(pru, df=df, function(row, pru,...) {
    restore.point("pru_readme_overview_run_inner")
    readme_file = file.path(project_dir, "readme", "txt", df$txt_file[row])
    rai = rai_init(project_dir,proc_info = pru$proc_info,media_files = readme_file)
    values = list(readme_file = basename(df$org_file[row]))
    res = ai_run(rai, values=values)
    res
  })
  pru = pru_set_status(pru, pru$items)
  restore.point("pru_readme_run")
  if (!pru_is_ok(pru)) return(invisible(pru))
  
  res_df = ai_combine_content_df(pru$items, df %>% select(readme_file = org_file))
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  return(invisible(pru))
}

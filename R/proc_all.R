example = function() {
  #ai_clear_cache()
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  project_dir = "/home/rstudio/repbox/projects_share/ecta_84_2_6" 
  #steps = repbox_fp_steps_from(tab_given = TRUE, tab_notes_pdf = FALSE, tab_html_pdf = FALSE, tab_main = TRUE, readme = FALSE)
  steps = repbox_fp_steps_from(tab_given = TRUE,readme = FALSE)
  repbox_run_fp(project_dir, steps,overwrite = TRUE)
  rstudioapi::filesPaneNavigate(project_dir)
  
  
  repbox_rerun_outages(project_dir, steps)
  
  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  parent_dir = "~/repbox/projects_share"
  steps = repbox_fp_steps(readme_data = TRUE)
  steps = repbox_fp_steps_from(tab_given = TRUE,readme = FALSE)
  steps = repbox_fp_steps(tab_main=TRUE)
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  repbox_error_ver_dirs(project_dirs, steps)
  repbox_outage_ver_dirs(project_dirs, steps)
  for (project_dir in project_dirs) {
    cat("\n", project_dir, "\n")
    repbox_run_fp(project_dir,steps, overwrite = FALSE)
  }
  
  repbox_rerun_outages(project_dirs,steps = steps,max_repeat = 5, sleep_sec = 30)

  
  repbox_run_fp(project_dir,steps)
  rstudioapi::filesPaneNavigate(project_dir)
  
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  
}

repbox_fp_steps = function(tab_given=FALSE, tab_notes_pdf=FALSE, tab_html_pdf=FALSE, tab_main=FALSE, ev_tab=FALSE, readme=FALSE, readme_overview=readme, readme_var=readme, readme_script_tab_fig = readme, readme_data=readme) {
  as.list(sys.frame(sys.parent(0)))
}


repbox_fp_steps_from = function(tab_given=FALSE, tab_notes_pdf=tab_given, tab_html_pdf=tab_notes_pdf, tab_main=tab_html_pdf, ev_tab=tab_main, readme=ev_tab, readme_overview=readme, readme_var=readme, readme_script_tab_fig = readme, readme_data=readme) {
  as.list(sys.frame(sys.parent(0)))
}

repbox_run_fp = function(project_dir, steps = repbox_fp_steps_from(TRUE), overwrite=FALSE, overwrite_hx = overwrite, doc_type = NULL) {
  restore.point("repbox_run_fp")
  all_doc_types = repbox_doc_types(project_dir)
  if (!is.null(doc_type)) {
    doc_type = intersect(doc_type, all_doc_types)
  }
  if (steps$tab_given) {
    proc_tab_given(project_dir, doc_type=doc_type)
  }
  pdf_doc_dirs = repbox_doc_dirs(project_dir, doc_form="pdf", doc_type=doc_type)
  if (steps$tab_notes_pdf) {
    proc_tab_notes_from_pdf(project_dir, doc_type=doc_type, overwrite = overwrite)
  }
  if (steps$tab_html_pdf) {
    proc_tab_html_from_pdf(project_dir,doc_type = doc_type, overwrite=overwrite)
  }
  if (steps$tab_main) {
    proc_tab_main(project_dir, overwrite=overwrite)
  }
  
  if (steps$ev_tab) {
    for (dt in doc_type) {
      repbox_ev_tab(project_dir, doc_type=dt)
    }
  }
  if (steps$readme_overview) {
    proc_readme(project_dir,"readme_overview", overwrite=overwrite)
  }
  if (steps$readme_var) {
    proc_readme(project_dir,"readme_var", overwrite=overwrite)
  }
  if (steps$readme_script_tab_fig) {
    proc_readme(project_dir,"readme_script_tab_fig", overwrite=overwrite)
  }
  if (steps$readme_data) {
    proc_readme(project_dir,"readme_data", overwrite=overwrite)
  }

}

repbox_error_ver_dirs = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_error_ver_dirs")
  parent_dirs = file.path(project_dirs, "fp") 
  ver_dirs = NULL
  if (steps$tab_notes_pdf) {
    ver_dirs = union(ver_dirs, fp_all_error_ver_dirs(parent_dirs, "tab_notes"))
  }
  if (steps$tab_html_pdf) {
    ver_dirs = union(ver_dirs, fp_all_error_ver_dirs(parent_dirs, "tab_html"))
  }
  if (steps$readme) {
    ver_dirs = union(ver_dirs, fp_all_error_ver_dirs(parent_dirs, "readme_overview"))
  }
  ver_dirs
} 


repbox_outage_ver_dirs = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_outage_ver_dirs")
  parent_dirs = file.path(project_dirs, "fp") 
  ver_dirs = NULL
  if (steps$tab_notes_pdf) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_notes"))
  }
  if (steps$tab_html_pdf) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_html"))
  }
  if (steps$readme) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs,c( "readme_overview","readme_var", "readme_script_tab_fig","readme_data","readme_data_descr")))
  }
  ver_dirs
}  

repbox_rerun_outages = function(project_dirs, steps = repbox_fp_steps_from(TRUE), max_repeat=0, sleep_sec = 30) {
  restore.point("repbox_rerun_outages")
  ver_dirs = repbox_outage_ver_dirs(project_dirs, steps)
  cat(paste0("\n", length(ver_dirs), " outages found.\n\n"))
  if (length(ver_dirs)==0) return(NULL)
  
  counter = 0
  while(counter <= max_repeat) {
    counter = counter+1
    fp_rerun_all_outage_ver(ver_dirs=ver_dirs)
    rem_ver_dirs = repbox_outage_ver_dirs(project_dirs, steps)
    if (length(rem_ver_dirs)==0 | counter > max_repeat) break
    cat(paste0("\n\n", length(ver_dirs)==0, " remaining outages.\nSleep for ", sleep_sec, " sec...\n"))
    Sys.sleep(sleep_sec)
  }
  invisible(ver_dirs)
}

repbox_rerun_errors = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_rerun_errors")
  ver_dirs = repbox_error_ver_dirs(project_dirs, steps)
  if (length(ver_dirs)==0) return(NULL)
  fp_rerun_all_error_ver(ver_dirs=ver_dirs)
  invisible(ver_dirs)
}

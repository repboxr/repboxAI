example = function() {
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  #set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  steps = repbox_fp_steps(readme_overview = TRUE)
  project_dir = project_dirs =  "/home/rstudio/repbox/projects_share/restat_2689366"
  ai_clear_cache()
  repbox_run_fp(project_dir, steps,overwrite = TRUE)
  
  repbox_error_ver_dirs(project_dirs, steps)
  repbox_outage_ver_dirs(project_dirs, steps)
  
  repbox_rerun_outages(project_dir, steps)
  
  
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  for (project_dir in project_dirs) {
    cat("\n", project_dir, "\n")
    repbox_run_fp(project_dir,steps)
  }
  
  repbox_outage_ver_dirs(project_dirs, steps)
  
  
  repbox_run_fp(project_dir,steps)
  rstudioapi::filesPaneNavigate(project_dir)
}

repbox_fp_steps = function(tab_given=FALSE, tab_notes_pdf=FALSE, tab_html_pdf=FALSE, ev_tab=FALSE, readme_overview=FALSE) {
  as.list(sys.frame(sys.parent(0)))
}


repbox_fp_steps_from = function(tab_given=FALSE, tab_notes_pdf=tab_given, tab_html_pdf=tab_notes_pdf, ev_tab=tab_html_pdf, readme_overvoew=ev_tab) {
  as.list(sys.frame(sys.parent(0)))
}

repbox_run_fp = function(project_dir, steps = repbox_fp_steps_from(TRUE), overwrite=FALSE, overwrite_hx = overwrite, doc_type = NULL) {
  restore.point("repbox_run_fp")
  all_doc_types = repbox_doc_types(project_dir)
  doc_type = intersect(doc_type, all_doc_types)
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
  if (steps$ev_tab) {
    for (dt in doc_type) {
      repbox_ev_tab(project_dir, doc_type=dt)
    }
  }
  if (steps$readme_overview) {
    proc_readme_overview(project_dir, overwrite=overwrite)    
  }
  
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
  if (steps$readme_overview) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "readme_overview"))
  }
  ver_dirs
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
  if (steps$readme_overview) {
    ver_dirs = union(ver_dirs, fp_all_error_ver_dirs(parent_dirs, "readme_overview"))
  }
  ver_dirs
} 

repbox_rerun_outages = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_rerun_outages")
  ver_dirs = repbox_outage_ver_dirs(project_dirs, steps)
  if (length(ver_dirs)==0) return(NULL)
  fp_rerun_all_outage_ver(ver_dirs=ver_dirs)
  invisible(ver_dirs)
}

repbox_rerun_errors = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_rerun_errors")
  ver_dirs = repbox_error_ver_dirs(project_dirs, steps)
  if (length(ver_dirs)==0) return(NULL)
  fp_rerun_all_error_ver(ver_dirs=ver_dirs)
  invisible(ver_dirs)
}

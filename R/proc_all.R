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
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  
  parent_dir = "~/repbox/projects_share"
  steps = repbox_fp_steps(map_reg_static = TRUE)
  steps = repbox_fp_steps(reg_classify_static = TRUE)
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  #project_dir = project_dirs[1]
  for (project_dir in project_dirs) {
    cat("\n", project_dir, "\n")
    repbox_run_fp(project_dir,steps, overwrite = FALSE)
  }
  
  repbox_rerun_outages(project_dirs,steps = steps,max_repeat = 5, sleep_sec = 30)

  
  repbox_error_ver_dirs(project_dirs, steps)
  repbox_outage_ver_dirs(project_dirs, steps)
  

  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  repbox_rerun_outages(project_dirs,max_repeat = 5, sleep_sec = 30)
  
    
  repbox_run_fp(project_dir,steps)
  rstudioapi::filesPaneNavigate(project_dir)
  
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  
}

repbox_fp_steps = function(tab_given=FALSE, tab_notes_pdf=FALSE, tab_html_pdf=FALSE, tab_main=FALSE, ev_tab=FALSE, readme=FALSE, readme_overview=readme, readme_var=readme, readme_script_tab_fig = readme, readme_data=readme, tab_classify = FALSE, by_tab_classify = FALSE, by_tab_classify_nodoc=FALSE, map_reg_static = FALSE, reg_classify_static=FALSE) {
  as.list(sys.frame(sys.parent(0)))
}


repbox_fp_steps_from = function(tab_given=FALSE, tab_notes_pdf=tab_given, tab_html_pdf=tab_notes_pdf, tab_main=tab_html_pdf, ev_tab=tab_main, readme=ev_tab, readme_overview=readme, readme_var=readme, readme_script_tab_fig = readme, readme_data=readme, tab_classify = readme_data, by_tab_classify = tab_classify, by_tab_classify_nodoc=by_tab_classify, map_reg_static = by_tab_classify_nodoc, reg_classify_static=map_reg_static) {
  as.list(sys.frame(sys.parent(0)))
}

repbox_run_fp = function(project_dir, steps = repbox_fp_steps_from(TRUE), overwrite=FALSE, overwrite_hx = overwrite, doc_type = NULL) {
  restore.point("repbox_run_fp")
  all_doc_types = repbox_doc_types(project_dir)
  if (!is.null(doc_type)) {
    doc_type = intersect(doc_type, all_doc_types)
  } else {
    doc_type = all_doc_types
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

  if (steps$tab_classify) {
    for (dt in doc_type) {
      proc_tab_extra(project_dir, "tab_classify", doc_type="art",overwrite = overwrite)
    }
  }
  if (steps$by_tab_classify) {
    for (dt in doc_type) {
      proc_by_tab_extra(project_dir, "tab_classify", add_doc=TRUE, add_tab_main = TRUE, add_tab_ref=TRUE, doc_type=dt,overwrite = overwrite)
    }
  }
  if (steps$by_tab_classify_nodoc) {
    for (dt in doc_type) {
      proc_by_tab_extra(project_dir, "tab_classify", add_doc=FALSE, add_tab_main = TRUE, add_tab_ref=TRUE, doc_type=dt,overwrite = overwrite)
    }
  }
  if (steps$map_reg_static) {
    for (dt in doc_type) {
      proc_single(project_dir,doc_type=dt, "map_reg_static", add_all_doc = TRUE, add_all_tab = TRUE,add_all_static_do = TRUE,overwrite = overwrite)
    }
  }
  if (steps$reg_classify_static) {
    for (dt in doc_type) {
      cat("\nreg_classify_static for", dt, "of", project_dir, "\n")
      pru = 
        rai_pru_base(project_dir, "reg_classify_static", doc_type="art", overwrite=overwrite) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() %>%
        rai_pru_add_reg_list_static() %>%
        rai_pru_add_tab_media(in_context=FALSE)
      
      proc_rai_pru(pru)
    }
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
  if (steps$tab_classify | steps$by_tab_classify | steps$by_tab_classify_nodoc) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_classify"))
  }
  ver_dirs
}  

repbox_rerun_outages = function(project_dirs, steps = repbox_fp_steps_from(TRUE), max_repeat=0, sleep_sec = 30) {
  restore.point("repbox_rerun_outages")
  ver_dirs = repbox_outage_ver_dirs(project_dirs, steps)
  cat(paste0("\n", length(ver_dirs), " outages found.\n\n"))
  if (length(ver_dirs)==0) return(NULL)
  
  rem_ver_dirs = ver_dirs
  counter = 0
  while(counter <= max_repeat) {
    counter = counter+1
    fp_rerun_all_outage_ver(ver_dirs=rem_ver_dirs)
    rem_ver_dirs = repbox_outage_ver_dirs(project_dirs, steps)
    if (length(rem_ver_dirs)==0 | counter > max_repeat) break
    cat(paste0("\n\n", length(rem_ver_dirs), " remaining outages.\nSleep for ", sleep_sec, " sec...\n"))
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

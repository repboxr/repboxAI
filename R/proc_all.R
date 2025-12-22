

repbox_default_fp_analysis = function(project_dir) {
  library(repboxAI)
  library(aikit)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_1_2_4"
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)
  
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-3-pro-preview", manual=TRUE)
  #set_ai_opts(model = "gemini-2.5-flash")
  #set_ai_opts(model = "gemini-flash-lite")

  steps = repbox_fp_steps(map_reg_run = TRUE)
  steps = repbox_fp_steps(reg_classify = TRUE)
  repbox_run_fp(project_dir,steps, overwrite = TRUE)
  repbox_rerun_outages(project_dir,steps = steps,max_repeat = 1, sleep_sec = 30)
  
    
  steps_all = repbox_fp_steps_from(tab_given=TRUE)
  steps_hx = repbox_fp_steps(tab_given=TRUE)
  steps2.0 = repbox_fp_steps_from(tab_notes_pdf = TRUE, ev_tab=FALSE)
  steps2.5 = repbox_fp_steps_from(ev_tab=TRUE)
  
  repbox_run_fp(project_dir,steps_hx, overwrite = FALSE)

  set_ai_opts(model = "gemini-2.5-flash-lite-preview-06-17")
  repbox_run_fp(project_dir,steps2.0, overwrite = FALSE)

  
  set_ai_opts(model = "gemini-2.0-flash")
  repbox_run_fp(project_dir,steps2.0, overwrite = FALSE)
  
  
  
  set_ai_opts(model = "gemini-2.5-flash")
  repbox_run_fp(project_dir,steps2.5, overwrite = FALSE)
  
  set_ai_opts(model = "gemini-2.5-flash-lite-preview-06-17")
  repbox_run_fp(project_dir,steps2.5, overwrite = FALSE)
    
  repbox_rerun_outages(project_dir,steps = steps_all,max_repeat = 5, sleep_sec = 30)
}


example = function() {
  #ai_clear_cache()
  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.5-flash")
  #project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  #project_dir = "~/repbox/projects_share/qje_3036349" 
  #project_dir = "~/repbox/projects_share/aeri_1_2_6"
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"

  steps = repbox_fp_steps(map_inv_reg_run = TRUE)
  steps = repbox_fp_steps(map_reg_run = TRUE)
  #steps = repbox_fp_steps_base()
  #set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  repbox_run_fp(project_dir, steps,overwrite = FALSE,doc_type = "art")

  rstudioapi::filesPaneNavigate(project_dir)

    
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  set_ai_opts(model = "gemini-2.0-flash")
  steps = repbox_fp_steps_advanced()
  repbox_run_fp(project_dir, steps,overwrite = FALSE)
  
  
  rstudioapi::filesPaneNavigate(project_dir)
  
  
  repbox_rerun_outages(project_dir, steps)
  
  library(repboxAI)
  library(aikit)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash-thinking-exp")
  set_ai_opts(model = "gemini-2.0-flash")
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  set_ai_opts(model = "gemini-2.5-flash")
  
  parent_dir = "~/repbox/projects_share"
  steps2.0 = repbox_fp_steps_from(tab_given=TRUE, ev_tab=FALSE)
  steps2.5 = repbox_fp_steps_from(ev_tab=TRUE)
  
  
  steps = repbox_fp_steps(map_reg_static = TRUE)
  steps = repbox_fp_steps(reg_classify = TRUE)
  steps = repbox_fp_steps(tab_given = TRUE)
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
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  set_ai_opts(model = "gemini-2.0-flash")
  parent_dir = "~/repbox/projects_share"
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  repbox_rerun_outages(project_dirs,max_repeat = 5, sleep_sec = 30)
  
    
  repbox_run_fp(project_dir,steps)
  rstudioapi::filesPaneNavigate(project_dir)
  
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  
}


# Implement default pipeline
# see pipeline.md
# Implement default pipeline
# see pipeline.md
repbox_fp_steps_pipeline = function(tab_given=TRUE,ev_tab=TRUE, map_reg_run=TRUE,  reg_classify=TRUE, ...) {
  repbox_fp_steps(tab_given=tab_given, map_reg_run=map_reg_run, ev_tab=ev_tab, map_reg_run=map_reg_run, reg_classify=reg_classify, ...)
}

repbox_fp_join_steps = function(...) {
  step_li = list(...)
  restore.point("repbox_fp_join_steps")
  steps = do.call(c,step_li)
  all_steps = c(repbox_fp_steps_from(), repbox_fp_readme_steps_from())
  all_steps[names(steps)] = steps
  all_steps
}

repbox_fp_steps_base = function() {
  repbox_fp_steps_from(tab_given=TRUE,by_tab_classify = FALSE)
}

repbox_fp_steps_advanced = function() {
  repbox_fp_steps_from(map_reg_static = TRUE)
}

repbox_fp_readme_steps_from = function(readme_overview=FALSE, readme_var=readme_overview, readme_script_tab_fig = readme_var, readme_data=readme_script_tab_fig, readme_vs_guide=readme_data) {
  as.list(sys.frame(sys.parent(0)))  
}

repbox_fp_readme_steps = function(readme=FALSE, readme_overview=FALSE, readme_var=FALSE, readme_script_tab_fig = FALSE, readme_data=FALSE, readme_vs_guide=FALSE) {
  as.list(sys.frame(sys.parent(0)))  
}

repbox_fp_steps = function(tab_given=FALSE, tab_notes_pdf=FALSE, tab_html_pdf=FALSE, tab_main=FALSE, ev_tab=FALSE, tab_classify = FALSE, by_tab_classify = FALSE, by_tab_classify_nodoc=FALSE, map_reg_static = FALSE,  map_reg_run=FALSE, map_inv_reg_run=FALSE, reg_classify=FALSE) {
  as.list(sys.frame(sys.parent(0)))
}


repbox_fp_steps_from = function(tab_given=FALSE, tab_notes_pdf=tab_given, tab_html_pdf=tab_notes_pdf, tab_main=tab_html_pdf, ev_tab=tab_main,  tab_classify = ev_tab,reg_classify= TRUE, map_reg_run = TRUE, by_tab_classify = FALSE, by_tab_classify_nodoc=FALSE, map_reg_static = FALSE,  map_inv_reg_run = FALSE) {
  as.list(sys.frame(sys.parent(0)))
}

repbox_run_fp = function(project_dir, steps = repbox_fp_steps_from(TRUE), overwrite=FALSE, overwrite_hx = overwrite, doc_type = NULL, to_v0 = TRUE) {
  restore.point("repbox_run_fp")
  org_steps = steps
  steps = repbox_fp_join_steps(steps)
  all_doc_types = repbox_doc_types(project_dir)
  if (!is.null(doc_type)) {
    doc_type = intersect(doc_type, all_doc_types)
  } else {
    doc_type = all_doc_types
  }
  if (isTRUE(steps$tab_given)) {
    proc_tab_given(project_dir, doc_type=doc_type)
  }
  pdf_doc_dirs = repbox_doc_dirs(project_dir, doc_form="pdf", doc_type=doc_type)
  if (isTRUE(steps$tab_notes_pdf)) {
    proc_tab_notes_from_pdf(project_dir, doc_type=doc_type, overwrite = overwrite)
  }
  if (isTRUE(steps$tab_html_pdf)) {
    proc_tab_html_from_pdf(project_dir,doc_type = doc_type, overwrite=overwrite)
  }
  if (isTRUE(steps$tab_main)) {
    proc_tab_main(project_dir, overwrite=overwrite)
  }
  
  if (isTRUE(steps$ev_tab)) {
    for (dt in doc_type) {
      repbox_ev_tab(project_dir, doc_type=dt)
    }
  }
  if (isTRUE(steps$readme_overview)) {
    proc_readme(project_dir,"readme_overview", overwrite=overwrite)
  }
  if (isTRUE(steps$readme_var)) {
    proc_readme(project_dir,"readme_var", overwrite=overwrite)
  }
  if (isTRUE(steps$readme_script_tab_fig)) {
    proc_readme(project_dir,"readme_script_tab_fig", overwrite=overwrite)
  }
  if (isTRUE(steps$readme_data)) {
    proc_readme(project_dir,"readme_data", overwrite=overwrite)
  }
  if (isTRUE(steps$readme_vs_guide)) {
    proc_readme(project_dir,"readme_vs_guide", overwrite=overwrite)
  }

  if (isTRUE(steps$tab_classify)) {
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "tab_classify",tpl_id = "tab_classify", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() #%>%
        #rai_pru_add_tab_media(by_tab = FALSE, add_ref = FALSE) 
      proc_rai_pru(pru)
    }
  }
  if (isTRUE(steps$by_tab_classify)) {
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "tab_classify",tpl_id = "by_tab_classify", doc_type=dt, overwrite=overwrite, proc_postfix = "_bytab", to_v0 = to_v0) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df(by_tab = TRUE) %>%
        rai_pru_add_tab_media(by_tab = TRUE, add_ref = TRUE) 
      proc_rai_pru(pru)
    }
  }
  if (isTRUE(steps$by_tab_classify_nodoc)) {
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "tab_classify",tpl_id = "by_tab_classify_nodoc", proc_postfix = "_bytab_nodoc", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_tab_df(by_tab = TRUE) %>%
        rai_pru_add_tab_media(by_tab = TRUE, add_ref = TRUE) 
      proc_rai_pru(pru)
    }
  }
  dt = first(doc_type)
  if (isTRUE(steps$map_reg_static)) {
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "map_reg_static", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() %>%
        rai_pru_add_tab_media(in_context=FALSE) %>%
        rai_pru_add_static_do()
      proc_rai_pru(pru)
    }
  }
  if (isTRUE(steps$reg_classify)) {
    for (dt in doc_type) {
      cat("\nreg_classify for", dt, "of", project_dir, "\n")
      pru = 
        rai_pru_base(project_dir, "reg_classify", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_static_do() %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() %>%
        rai_pru_add_reg_list(static=FALSE, filter_tab_df = TRUE) %>% 
        rai_pru_add_tab_media(in_context=FALSE) 
      
      proc_rai_pru(pru)
    }
  }
  
  if (isTRUE(steps$reg_classify_static)) {
    for (dt in doc_type) {
      cat("\nreg_classify for", dt, "of", project_dir, "\n")
      pru = 
        rai_pru_base(project_dir, "reg_classify_static", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() %>%
        rai_pru_add_reg_list_static(filter_tab_df = TRUE) %>%
        rai_pru_add_tab_media(in_context=FALSE)
      
      proc_rai_pru(pru)
    }
  }
  if (isTRUE(steps$map_reg_run)) {
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "map_reg_run", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) %>%
        rai_pru_add_doc() %>%
        rai_pru_add_tab_df() %>%
        rai_pru_add_tab_media(in_context=FALSE) %>%
        rai_pru_add_run_do(in_context = FALSE)
      proc_rai_pru(pru)
    }
  }
  if (isTRUE(steps$map_inv_reg_run)) {
    restore.point("proc_map_inv_reg_run")
    for (dt in doc_type) {
      pru = 
        rai_pru_base(project_dir, "map_inv_reg_run", doc_type=dt, overwrite=overwrite, to_v0 = to_v0) |>
        rai_pru_add_reg_df()
      # no regressions successfully run. we don't perform a mapping
      if (NROW(pru[["reg_df"]])==0) {
        next
      }
      pru = pru |>
        rai_pru_add_doc() |>
        rai_pru_add_tab_df() |>
        rai_pru_add_tab_media(in_context=FALSE) |>
        rai_pru_add_run_do(in_context = FALSE,only_reg_df_output = TRUE)
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

repbox_fp_steps_to_names = function(steps) {
  names(steps[unlist(steps)])
}

repbox_outage_ver_dirs = function(project_dirs, steps = repbox_fp_steps_from(TRUE)) {
  restore.point("repbox_outage_ver_dirs")
  parent_dirs = file.path(project_dirs, "fp") 
  step_names = repbox_fp_steps_to_names(steps)
  ver_dirs = NULL
  ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, step_names))
  
  if (steps$tab_notes_pdf) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_notes"))
  }
  if (steps$tab_html_pdf) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_html"))
  }
  if (steps$tab_classify | steps$by_tab_classify | steps$by_tab_classify_nodoc) {
    ver_dirs = union(ver_dirs, fp_all_outage_ver_dirs(parent_dirs, "tab_classify"))
  }
  ver_dirs
}  

repbox_rerun_outages = function(project_dirs, steps = repbox_fp_steps_from(TRUE), max_repeat=0, sleep_sec = 30) {
  restore.point("repbox_rerun_outages")
  steps = repbox_fp_join_steps(steps)
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

fp_count_prods = function(parent_dir) {
  ver_dirs = fp_all_ver_dirs(parent_dir)
  
  df = tibble(
    ver_dir = fp_all_ver_dirs(parent_dir),
    proc_dir = fp_ver_dir_to_proc_dir(ver_dir),
    prod_dir = fp_proc_dir_to_prod_dir(proc_dir),
    proc_id = fp_proc_dir_to_proc_id(proc_dir),
    prod_id = fp_prod_dir_to_prod_id(prod_dir),
    fp_dir = fp_prod_dir_to_fp_dir(prod_dir)
  )
  
  prod_df = df %>%
    group_by(prod_id) %>%
    summarize(num_fp = n_distinct(fp_dir))
  
  proc_df = df %>%
    group_by(prod_id, proc_id) %>%
    summarize(num_fp = n_distinct(fp_dir))
  
  
  prod_ids = fp_ver_dir_to_prod_id(ver_dirs) 
  
}

fp_find_missing_prod = function(parent_dir, prod_id=NULL) {
  parent_dir = "~/repbox/projects_share"
  restore.point("fp_find_missing_prod")
  ver_dirs = fp_all_ver_dirs(parent_dir)
  if (is.null(prod_id))
    prod_ids = fp_ver_dir_to_prod_id(ver_dirs) 
  
    
}

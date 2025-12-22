# Create an rai_pru object that can be used for an ai-based analysis
example = function() {
  library(repboxAI)
  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.0-flash")
  set_ai_opts(model = "gemini-2.5-pro-exp-03-25")
  
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  pru = 
    rai_pru_base(project_dir, "reg_classify", doc_type="art", overwrite=TRUE) %>%
    rai_pru_add_doc() %>%
    rai_pru_add_tab_df() %>%
    rai_pru_add_reg_list_static() %>%
    rai_pru_add_tab_media(in_context=FALSE)

  proc_rai_pru(pru)
  
  rstudioapi::filesPaneNavigate(project_dir)
}  


rai_pru_base = function(project_dir, prod_id, doc_type="art", ai_opts = get_ai_opts(), verbose=TRUE, to_v0=TRUE, tpl_id = paste0(prod_id), json_mode=TRUE, use_schema = FALSE, overwrite=FALSE, tpl_dir = rai_tpl_dir(), tpl_file = NULL, proc_prefix = "", proc_postfix="", proc_id=NULL, parcels=list(), manual = isTRUE(ai_opts$manual)) {
  fp_dir = file.path(project_dir, "fp", paste0("prod_",doc_type))
  prefix = postfix = ""
  if (manual) prefix = "man_"
  pru = copy_into_list()
  restore.point("rai_pru_base")
  
  pru$doc_types = repbox_doc_types(project_dir)
  pru$media_files = NULL
  pru$context_media_files = NULL
  return(pru)
}  

rai_pru_add_doc = function(pru, add_all_doc=TRUE, doc_file_form_pref = doc_file_form_default_pref(manual), in_context=TRUE, manual=isTRUE(pru$manual)) {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_add_doc")
  
  pru$doc_files = rai_doc_file(pru$project_dir,doc_type = NULL, doc_file_form_pref)
  if (length(pru$doc_files)==0) {
    cat("\nNo doc_files found.")
    return(NULL)
  }
  
  pru$doc_file = rai_doc_file(pru$project_dir, pru$doc_type, doc_file_form_pref)
  
  if (add_all_doc) {
    media_files = pru$doc_files
  } else {
    media_files = pru$doc_file
  }
  rai_pru_add_media(pru, media_files, in_context)
}

# rai_pru_adapt_proc_id = function(pru, prefix="", postfix="", new_proc_id=NULL) {
#   if (is.null(pru)) return(pru)
#   restore.point("rai_pru_adapt_proc_id")
#   if (is.null(new_proc_id)) {
#     new_proc_id = paste0(prefix, pru$proc_id, postfix)
#   }
#   
#   new_proc_dir = file.path(dirname(pru$proc_dir),new_proc_id)
#   new_ver_dir = file.path(new_proc_dir, "v0")
#   new_ver_id = fp_ver_dir_to_ids(new_ver_dir)$ver_id
#   pru$proc_info$proc_id = new_proc_id
#   pru$proc_id = new_proc_id
#   pru$ver_dir = new_ver_dir
#   pru$ver_id = new_ver_id
#   pru
# }

rai_pru_add_tab_df = function(pru, tab_prod_id = "tab_main", tab_df_pref = tab_df_default_pref(tab_prod_id), by_tab = FALSE, tab_chunk_size = if (by_tab) 1 else NULL, tab_df_id = tab_prod_id, just_tabid = NULL) {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_add_tab_df")
  
  pru$tab_df_info = fp_pick_prod_ver(pru$fp_dir, tab_prod_id, pref=tab_df_pref)
  if (NROW(pru$tab_df_info)==0 | fp_is_err_obj(pru$tab_df_info)) {
    cat(paste0("\nPlease first successfully create ", tab_prod_id,".\n"))
    return(NULL)
  }
  
  
  pru$tab_df = fp_load_prod_df(pru$tab_df_info$ver_dir)
  
  if (!is.null(just_tabid)) {
    pru$tab_df = pru$tab_df[pru$tab_df$tabid %in% just_tabid,]
  }
  
  postfix=paste0("-", pru$tab_df_info$proc_id)
  if (isTRUE(tab_chunk_size < NROW(pru$tab_df)) & !by_tab) {
    postfix = paste0(postfix, "_c",tab_chunk_size)     
  }
  pru$postfix = paste0(postfix, pru$postfix)
  
  if (!is.null(tab_chunk_size) & is.null(pru$itemize_by)) {
    pru$itemize_by = "tab_df"
    pru$item_chunk_size = tab_chunk_size
  }
  
  pru
}

rai_pru_add_tab_media = function(pru, tab_df = pru$tab_df, by_tab = FALSE, add_ref = FALSE, in_context=FALSE, tab_ref_pref = tab_ref_default_pref()) {
  if (is.null(pru)) return(pru)
  restore.point("rai_pru_add_tab_media")
  # most overwrite every time: not a unique name
  base = paste0(pru$doc_type, "_tabs.html")
  
  if (add_ref & !by_tab) stop("Currently table references can only be added (add_ref=TRUE) if by_tab=TRUE")
  
  if (!by_tab) {
    outfile = rai_media_all_tab_html(pru$project_dir,tab_df,doc_type=pru$doc_type, base)
    pru = rai_pru_add_media(pru, outfile, in_context)
  } else {
    if (!isTRUE(pru$item_chunk_size==1)) {
      stop("by_tab=TRUE only works if yo have set by_tab=TRUE in rai_pru_add_tab_df.")
    }
    pru$add_by_tab_media = TRUE
    pru$tab_media_add_ref = add_ref
    
    if (add_ref) {
      pru$ref_li_doc_dirs = sapply(pru$doc_types, function(dt) {
        rai_pick_ref_li_doc_dir(project_dir,doc_type=dt,tab_ref_pref)
      })
      if (length(pru$ref_li_doc_dir)>0) {
        pru$ref_li_form = rdoc_form(pru$ref_li_doc_dirs[[1]])
        pru$all_ref_li = lapply(pru$ref_li_doc_dirs, rdoc_load_ref_li)
        pru$all_part_df = lapply(pru$ref_li_doc_dirs,rdoc_load_part_df)
      }
      
    }
    
    
    
  }
  pru
}

rai_pru_add_reg_list = function(pru, map_reg_pref = map_reg_run_default_pref(map_version), filter_tab_df = TRUE, static=TRUE, map_version = if (static) "map_reg_static" else "map_reg_run") {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_add_reg_list")
  
  pru$map_ver_info = fp_pick_prod_ver(pru$fp_dir,map_version, pref = map_reg_pref)
  
  if (NROW(pru$map_ver_info)==0) {
    cat("\nPlease first successfully create map_reg_run.\n")
    return(NULL)
  }
  
  pru$map_df = fp_load_prod_df(pru$map_ver_info$ver_dir)
  if (filter_tab_df) {
    if (is.null(pru$tab_df)) stop("Please make sure to first call rai_pru_add_tab_df.")
    pru$tab_df = pru$tab_df[pru$tab_df$tabid %in% pru$map_df$tabid,]
  }
  pru$postfix = paste0("-", str.left.of(pru$map_ver_info$proc_id,"-"),pru$postfix)
  pru
}

rai_pru_add_reg_list_static = function(pru, map_reg_static_pref = map_reg_static_default_pref(), filter_tab_df = TRUE) {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_add_reg_list_static")
  
  pru$map_ver_info = fp_pick_prod_ver(pru$fp_dir,"map_reg_static", pref = map_reg_static_pref)
  
  if (NROW(pru$map_ver_info)==0) {
    cat("\nPlease first successfully create map_reg_static.\n")
    return(NULL)
  }
  
  pru$map_df = fp_load_prod_df(pru$map_ver_info$ver_dir)
  if (filter_tab_df) {
    if (is.null(pru$tab_df)) stop("Please make sure to first call rai_pru_add_tab_df.")
    pru$tab_df = pru$tab_df[pru$tab_df$tabid %in% pru$map_df$tabid,]
  }
  pru$postfix = paste0("-", str.left.of(pru$map_ver_info$proc_id,"-"),pru$postfix)
  pru
}

rai_pru_add_media = function(pru, media_files, in_context = TRUE) {
  if (is.null(pru)) return(pru)
  if (in_context) {
    pru$context_media_files = unique(c(pru$context_media_file, media_files))
  } else {
    pru$media_files = unique(c(pru$media_file, media_files))
  }
  pru
}

rai_pru_load_parcels = function(pru, parcel_names, parcels=pru[["parcels"]]) {
  if (is.null(parcels)) parcels = list()
  pru$parcels = repdb_load_parcels(pru$project_dir, parcel_names, parcels)
  pru
}

rai_pru_add_static_do = function(pru, in_context=FALSE) {
  restore.point("rai_pru_add_static_do")
  pru = rai_pru_load_parcels(pru, "stata_source")
  pru$do_df = rai_load_do_source(project_dir,pru$parcels) 
  outfile = rai_media_all_static_do(project_dir,script_df = pru$do_df)
  pru = rai_pru_add_media(pru,outfile, in_context=in_context)
  pru
}

rai_pru_add_reg_df = function(pru, parcels = pru$parcels) {
  restore.point("rai_pru_add_reg_run_df")
  pru = rai_pru_load_parcels(pru, "reg_core")
  pru$reg_df = pru$parcels$reg_core$reg
  pru
}

rai_pru_add_run_do = function(pru, in_context=FALSE, only_reg_df_output = FALSE) {
  restore.point("rai_pru_add_run_do")
  pru = rai_pru_load_parcels(pru, "stata_source")
  pru$do_df = rai_load_do_source(project_dir,pru$parcels)
  output_just_runid = NULL
  if (only_reg_df_output) {
    output_just_runid = pru$reg_df$runid
  }
  
  outfile = rai_media_run_do(project_dir,parcels=pru$parcels, output_just_runid = output_just_runid)
  pru = rai_pru_add_media(pru,outfile, in_context=in_context)
  pru
}



rai_pru_set_tpl = function(pru, tpl_id=pru$tpl_id, tpl_dir=pru$tpl_dir, tpl_file = pru[["tpl_file"]], tpl=pru[["tpl"]]) {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_set_tpl")
  
  if (is.null(tpl_file) & is.null(tpl)) {
    tpl_file = file.path(tpl_dir, paste0(tpl_id,".txt"))
  }
  pru$tpl_file = tpl_file
  if (is.null(tpl)) {
    tpl = paste0(readLines(tpl_file), collapse="\n")
  }
  pru$tpl = tpl
  pru$tpl_var = ai_tpl_vars(pru$tpl)
  pru
}

proc_rai_pru = function(pru) {
  restore.point("proc_rai_pru")
  if (is.null(pru)) return(NULL)
  
  if (is.null(pru[["tpl"]]))
    pru = rai_pru_set_tpl(pru)
  
  prefix = paste0(pru$proc_prefix, pru$prefix)
  postfix = paste0(pru$postfix, pru$proc_postfix)
  
  pru$proc_info = rai_make_proc_info(prod_id=pru$prod_id,ai_opts = pru$ai_opts,tpl_file = pru$tpl_file, json_mode=pru$json_mode, use_schema = pru$use_schema, tpl_id=pru$tpl_id,proc_prefix = prefix, proc_postfix = postfix, proc_id = pru[["proc_id"]])
  
  pru$proc_id  = pru$proc_info$proc_id
  pru = pru_init_dirs(pru=pru)
  
  # We check overwrite a bit late, but I don't know a better approach
  # since ver_dir may depend on the different steps
  # when building pru
  if (!pru$overwrite) if (fp_ver_dir_ok(pru$ver_dir)) return(NULL)

  
  # Some values for prompt can be set in advance
  values = list()
  if ("doc_type_descr" %in% pru$tpl_var) {
    if (length(pru$doc_types)<=1) {
      values$doc_type_descr = "scientific article"
    } else if (length(pru$doc_types)==2) {
      values$doc_type_descr = "scientific article and an online appendix"
    } else {
      values$doc_type_descr = paste0("scientific article and ", length(pru$doc_types)-1," online appendices")
    }
  }
  
  if ("cur_doc" %in% pru$tpl_var) {
    if (pru$doc_type=="art") {
      values$cur_doc = "the article"
    } else if (length(pru$doc_types)<=2) {
      values$cur_doc = "the online appendix"
    }  else {
      values$cur_doc = "an online appendix"
    }
  }
  if ("script_list" %in% pru$tpl_var) {
    values = rai_prompt_value_script_list(pru$do_df, values)
  }
  if (!is.null(pru[["values"]])) {
    values[[names(pru$values)]] = pru$values
  }
  
  pru$values = values
  
  pru_next_stage(pru, "proc_rai_pru_run")
}

proc_rai_pru_run = function(pru) {
  restore.point("proc_rai_pru_run")
  
  if (isTRUE(pru$verbose)) {
    cat("\nRun ", pru$ver_dir,"\n")
  }
  
  project_dir = pru$project_dir
  manual = isTRUE(pru$manual)

  
  if (length(pru$content_media_files)>0) {
    pru$context = rai_context(pru$project_dir,model = pru$ai_opts$model,media_files = pru$context_media_files)
  }
  
  values = pru$values
  if ("script_list" %in% pru$tpl_var) {
    values = rai_prompt_value_script_list(pru$do_df, values)
  }
  
  if ("tab_list" %in% pru$tpl_var & !isTRUE(pru$itemize_by=="tab_df")) {
    values = rai_prompt_value_tab_list(pru$tab_df, values)
  }
  if ("reg_run_list" %in% pru$tpl_var) {
    values = rai_prompt_value_reg_run_list(pru[["reg_df"]], values)
  }
  if ("reg_list" %in% pru$tpl_var) {
    values = rai_prompt_value_reg_list(pru$map_df, values, static=FALSE)
  }

  if ("reg_list_static" %in% pru$tpl_var) {
    values = rai_prompt_value_reg_list_static(pru$map_df, values)
  }
  
  rai = rai_init(pru$project_dir,proc_info = pru$proc_info,media_files = pru[["media_files"]],context = pru[["context"]])
  
  
  if (!is.null(pru$itemize_by)) {
    org_media_files = pru$media_files
    item_df = pru[[pru$itemize_by]]
    inds = seq_len(NROW(item_df))
    chunks  = split(inds, ceiling(seq_along(inds) / pru$item_chunk_size))
    num_items = length(chunks)
    
    row = 1
    pru = pru_make_items(pru, num_items = num_items, function(row, pru,...) {
      restore.point("proc_rai_pru_run_item_fun")
      tab_df = pru$tab_df[chunks[[row]],]
      media_files = org_media_files

      if ("tabtitle" %in% pru$tpl_var & isTRUE(pru$itemize_by=="tab_df")) {
        values$tabtitle = tab_df$tabtitle
      }
      
      if ("tabid" %in% pru$tpl_var & isTRUE(pru$itemize_by=="tab_df")) {
        values$tabid = tab_df$tabid
      }

      if ("tab_list" %in% pru$tpl_var & isTRUE(pru$itemize_by=="tab_df")) {
        values = rai_prompt_value_tab_list(tab_df, values)
      }
      
      if (!is.null(pru$by_tab_media_fun)) {
        by_tab_media_files = do.call(pru$by_tab_media_fun, list(pru=pru, tabid = tab_df$tabid))
        media_files = c(media_files, by_tab_media_files)
        rai$media_files = media_files

      }
      
      if (isTRUE(pru$add_by_tab_media)) {
        tabid = tab_df$tabid
        outfile = file.path(pru$project_dir,"fp","prompt_files",paste0("tab--",tabid,".html"))
        html = rai_media_tab_html(pru$project_dir,tabid = tabid, tab_main = tab_df, all_ref_li=pru[["all_ref_li"]], all_part_df = pru[["all_part_df"]], outfile=outfile)
        if (file.exists(outfile)) {
          media_files = c(media_files, outfile)
        }
        rai$media_files = media_files
      }
      if (manual) {
        # Generate the full prompt text and save it
        rai_pru_export_manual_prompt(pru, rai, values, row = if(num_items > 1) row else NULL)
        return(list(success = TRUE, manual = TRUE))
      } else {
        res = ai_run(rai, values=values)
        return(res)
      }
    })
  } else {
    if (manual) {
      rai_pru_export_manual_prompt(pru, rai, values)
      pru$items = list(list(success = TRUE, manual = TRUE))
    } else {
      res = ai_run(rai, values=values)
      pru$items = list(res)
    }
  }
  
  if (manual) {
    cat("\nManual prompt(s) exported to: ", pru$ver_dir)
    cat("\nPaste the response into ai_resp.md in that directory and call rai_pru_process_manual().\n")
    # Save the PRU in an "incomplete" state
    temp_pru_save(pru);
    return(invisible(pru))
  }

  
  pru = pru_set_status(pru, pru$items)
  
  restore.point("proc_rai_run_post")
  temp_pru_save(pru); #pru = temp_pru
  if (!pru_is_ok(pru)) return(invisible(pru))
  prod = repbox_prod(pru$prod_id)
  # Check: obj or arr? -> Need obj
  schema = prod_to_schema(prod, "obj")
  
  res_df = ai_combine_content_df(pru$items,schema=schema)
  prod_df = df_to_prod_df(res_df, repbox_prod(pru$prod_id))
  pru_save(pru, prod_df)
  writeLines(pru$items[[1]]$prompt, file.path(pru$ver_dir,"prompt.txt"))
  if (FALSE)
    rstudioapi::filesPaneNavigate(pru$ver_dir)
  return(invisible(pru))
}

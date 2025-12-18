
example = function(project_dir) {
  library(repboxAI)
  library(repboxRegmap)
  library(repboxReport)
  
  options(warn=1)
  #restore.point.options(display.restore.point = TRUE)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.5-flash")

  rai_patch_rerun(project_dir, prod_id ="map_reg_run", patch_id = "patch")
  
  rai_apply_patches(project_dir, prod_id ="map_reg_run",doc_type = "art")
  
  # Redo mapping  
  rme = rme_init(project_dir)
  rme = rme_add_eval(rme, rme_steps_all())
  issue_df = rme_combine_ev_df(rme)
  # Create map reports
  opts = rr_map_report_opts(embed_data = FALSE)
  rep_file = rr_map_report(project_dir,opts = opts)
  browseURL(rep_file)

    
}

#' Rerun map_reg_run for specific tables with visual patch media
#' 
#' @param project_dir Project directory
#' @param patch_id Identifier for this patch run (e.g., "visual_fix")
#' @param auto_apply If TRUE, automatically applies the patch to create a revised version
rai_patch_rerun = function(project_dir, prod_id="map_reg_run", patch_id = "patch", doc_type = "art",issue_types = c("multicol_reg_plausibility"), to_v0 = TRUE, overwrite = FALSE, auto_apply = FALSE, ignore_map_versions=NULL, just_map_versions=NULL, ai_opts = get_ai_opts()) {
  restore.point("rai_patch_rerun")
  #stop()
  
  # 1. Load Evaluation Results to find problematic tables
  rme = repboxRegmap::rme_load(project_dir, doc_type)
  if (is.null(rme)) stop("No rme.Rds found. Run evaluation first.")
  
  # Identify tables with specific issues (customize filter as needed)
  # Here we look for issues in specific integrity/structure checks
  issues_df = repboxRegmap::rme_combine_ev_df(rme,eval_steps = issue_types)
  if (NROW(issues_df) == 0) {
    cat("\nNo issues found in evaluation. Nothing to rerun.")
    return(NULL)
  }
  
  if (length(ignore_map_versions)>0) {
    issues_df = issues_df %>% filter(!map_version %in% ignore_map_versions)
  }
  if (length(just_map_versions)>0) {
    issues_df = issues_df %>% filter(map_version %in% just_map_versions)
  }

  if (NROW(issues_df) == 0) {
    cat("\nNo issues found in evaluation. Nothing to rerun.")
    return(NULL)
  }
  
  # Get unique tabids that have issues
  target_tabids = unique(issues_df$tabid)
  cat(paste0("\nFound issues in tables: ", paste(target_tabids, collapse=", "), "\n"))
  
  
  # 2. Setup the Patch PRU
  # We base this on the existing map_reg_run logic but modify the input
  base_ver_id = issues_df$map_version[1] 
  
  # Create a patch process ID
  patch_proc_id = paste0(patch_id,"_", base_ver_id)
  patch_proc_id = stri_replace_all_fixed(patch_proc_id, "--","__")
  
  # Initialize pru
  pru = rai_pru_base(project_dir, prod_id = prod_id, doc_type = doc_type, tpl_id = paste0(patch_id,"_",prod_id),overwrite = overwrite, to_v0 = to_v0,proc_id = patch_proc_id, ai_opts = ai_opts) %>%
    rai_pru_add_doc() %>%
    rai_pru_add_run_do(in_context = FALSE) %>%
    rai_pru_add_tab_df() # Loads all tables initially
  
  # 3. FILTER: Only keep the problematic tables in the PRU
  pru$tab_df = pru$tab_df[pru$tab_df$tabid %in% target_tabids, ]
  
  if (nrow(pru$tab_df) == 0) stop("Target tables not found in tab_df.")
  
  # 4. Configure for Itemized Execution (One AI call per table)
  # This enables specific media attachment per chunk
  pru$itemize_by = "tab_df"
  pru$item_chunk_size = 1
  pru$by_tab_media_fun = "rai_patch_by_tab_media"
  
  # Store rme in pru so that rr_table does not need to reload it
  pru$rme = rme
  pru$patch_id = patch_id
  pru$issue_types = unique(issues_df$test_name)
  # 5. Run AI
  cat(paste0("\nRunning correction patch for ", length(target_tabids), " tables...\n"))
  proc_rai_pru(pru)
  
  restore.point("rai_patch_rerun_post_pru")
  
  
  invisible(pru)
}

rai_apply_patch = function(patch_pru) {
  cat("\nApplying patch to create new version...\n")
  # This uses the generic patch application logic
  # It assumes the patch ver_dir is the one we just created
  project_dir = pru$project_dir
  patch_ver_dir = pru$ver_dir
  
  base_verid = pru$
    
    # We need the base_ver_dir corresponding to base_ver_id
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  base_ver_dir = repboxAI::fp_ver_id_to_ver_dir(fp_dir, prod_id, base_ver_id)
    
    # Define new revision directory
    base_proc_id = repboxAI::fp_ver_dir_to_proc_id(base_ver_dir)
    revised_proc_id = repboxAI::fp_get_revised_proc_id(base_proc_id)
    revised_proc_dir = file.path(fp_dir, prod_id, revised_proc_id)
    revised_ver_dir = repboxAI::fp_proc_dir_to_new_ver_dir(revised_proc_dir)
    
    repboxAI::fp_create_revised_version(
      base_ver_dir = base_ver_dir,
      patch_ver_dir = patch_ver_dir,
      revised_ver_dir = revised_ver_dir,
      key_col = "regid" # Assuming regid is the unique key for regressions
    )

}


rai_patch_by_tab_media = function(pru=NULL, tabid=NULL) {
  restore.point("rai_patch_by_tab_media")
  #stop()
  
  if (pru$prod_id != "map_reg_run") stop(paste0("Not yet implemented for ",pru$prod_id)) 
  
  library(repboxReport)
  project_dir = pru$project_dir
  
  # Ensure prompt_files directory exists (standard location for rai_pru media)
  prompt_files_dir = file.path(project_dir, "fp/prompt_files")
  if (!dir.exists(prompt_files_dir)) dir.create(prompt_files_dir, recursive = TRUE)
  
  # Generate report options (ensure we show the discrepancies)
  rr_opts = rr_map_report_opts(only_tests = pru$issue_types)
  res = rr_single_table(project_dir, rme=pru$rme, tabid = tabid, doc_type = pru$doc_type,  opts = rr_opts, table_png = TRUE, output_dir = file.path(prompt_files_dir))
  res_files = unlist(res[c(5,6)])
  res_files
}


rai_load_rme = function(project_dir, doc_type="art") {
  rme_file = file.path(project_dir, paste0("fp/eval_", doc_type,"/rme.Rds"))
  if (!file.exists(rme_file)) return(NULL)
  rme = readRDS(rme_file)
  rme
}


#' Apply all unapplied patches for a given product.
#'
#' Scans a product directory for all "patch" versions, checks if a corresponding
#' "revised" version already exists, and if not, creates one by applying the patch.
#'
#' @param project_dir The project directory.
#' @param prod_id The product ID to apply patches for (e.g., "map_reg_run").
#' @param doc_type The document type (e.g., "art").
#' @param key_col The key column for group-based replacement (e.g., "regid"). Defalt is tabid.
#' @param ... Additional arguments passed to `fp_create_revised_version_by_group_replace`.
rai_apply_patches = function(project_dir, prod_id, doc_type="art", key_col="tabid", patch_id="patch", ...) {
  restore.point("rai_apply_patches")
  #stop()
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_dir = file.path(fp_dir, prod_id)
  
  if (!dir.exists(prod_dir)) return(invisible())

  # 1. Find all patch process directories
  patch_proc_dirs = list.files(prod_dir, pattern = glob2rx(paste0(patch_id, "_*")), full.names = TRUE)
  patch_proc_dirs = patch_proc_dirs[dir.exists(patch_proc_dirs)]
  if (length(patch_proc_dirs) == 0) {
    cat("\nNo patch directories found for", prod_id, "in", doc_type)
    return(invisible())
  }

  
  patch_proc_dir = patch_proc_dirs[1]
  # 2. Iterate through each patch and apply if necessary
  for (patch_proc_dir in patch_proc_dirs) {
    patch_proc_id = basename(patch_proc_dir)

    # Extract the base proc_id that this patch is for
    base_ver_id = stringi::stri_replace_first_fixed(patch_proc_id, paste0(patch_id,"_"), "")
    
    base_ver_dir = fp_ver_id_to_ver_dir(fp_dir, prod_id, base_ver_id)
    patch_ver_dir = fp_pick_latest_ver_dir(patch_proc_dir)

    base_proc_id = fp_ver_dir_to_proc_id(base_ver_dir)
    # Determine the name of the revised version and check if it exists
    revised_proc_id = fp_get_revised_proc_id(base_proc_id)
    revised_proc_dir = file.path(prod_dir, revised_proc_id)

    if (dir.exists(revised_proc_dir) && fp_ver_dir_ok(fp_pick_latest_ver_dir(revised_proc_dir))) {
      cat("\nRevised version for patch '", patch_proc_id, "' already exists. Skipping.")
      next
    }

    # Apply the patch
    cat("\nApplying patch:", basename(patch_ver_dir), "to base:", basename(base_ver_dir))
    fp_create_revised_version(
      base_ver_dir = base_ver_dir,
      patch_ver_dir = patch_ver_dir,
      revised_ver_dir = fp_proc_dir_to_new_ver_dir(revised_proc_dir),
      key_col = key_col
    )
  }
}

fp_pick_latest_ver_dir = function(proc_dir) {
  dirs=list.dirs(proc_dir,full.names = TRUE,recursive = FALSE)
  fi = file.info(dirs)
  ind = which.max(fi$mtime)
  dirs[ind]
}

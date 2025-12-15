# Functions to create and apply patches to existing product versions.

example_run_patch = function() {
  library(repboxAI)
  library(aikit)
  library(repboxRegmap)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"
  base_ver_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6/fp/prod_art/map_reg_run/g25f-mocr/v0"
  rstudioapi::filesPaneNavigate(project_dir)

  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-2.5-flash")

    
  # Stage 1: Generate a patch using AI
  # This finds a base version, checks for issues, and asks the AI for corrections.
  # It saves the AI's output as a new "patch" version.
  proc_patch_map_reg_run(
    project_dir = project_dir,
    doc_type = doc_type,
    base_proc_pref = glob2rx("g25f-mocr"), # Selects the base to correct
    overwrite = TRUE
  )

  # Stage 2: Apply any unapplied patches to create revised versions
  # This deterministically finds all 'patch_*' versions and creates 'rX_*' versions.
  fp_apply_patches(
    project_dir = project_dir,
    prod_id = "map_reg_run",
    doc_type = doc_type,
    key_col = "regid"
  )

  

}
# Functions to create and apply patches to existing product versions.



#' Use AI to generate a patch for a map_reg_run version.
#'
#' Finds a base version of `map_reg_run`, loads evaluation results from
#' `repboxRegmap` to identify issues, and then calls an AI with a specific
#' prompt to generate a corrected mapping. The result is saved as a new
#' "patch" version.
#'
#' @param project_dir The project directory.
#' @param doc_type The document type (e.g., "art").
#' @param base_proc_pref A glob pattern to select the base `map_reg_run` version.
#' @param overwrite Logical, if TRUE, overwrites existing patch versions.
#' @return Invisibly returns the `pru` object from the AI run.
proc_patch_map_reg_run = function(project_dir, doc_type, base_proc_pref, overwrite = FALSE, ai_opts = get_ai_opts(), to_v0=TRUE, base_ver_dir = NULL) {
  restore.point("proc_patch_map_reg_run")
  stop()
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_id = "map_reg_run"

  # 1. Find the base version to be patched
  
  if (is.null(base_ver_dir)) {
    base_info = fp_pick_prod_ver(fp_dir, prod_id, pref = base_proc_pref)
    if (NROW(base_info) == 0) {
      cat("\nNo base version found for", prod_id, "with preference", deparse(base_proc_pref))
      return(NULL)
    }
    base_ver_dir = base_info$ver_dir
  }
  base_ver_id = fp_ver_dir_to_ver_id(base_ver_dir)
  base_proc_id = fp_ver_dir_to_proc_id(base_ver_dir)

  # 2. Load evaluation results to find issues
  # This assumes repboxRegmap has been run and its results are available.
  # I am not implementing rme_load here but assuming it's available.
  # rme = rme_load(project_dir)
  # For this example, let's assume `rme_load` and the regmap function are available.
  # If not, this will error, which is expected.
  # You need to load the repboxRegmap package or source its files.
  if (!exists("rme_load") || !exists("patch_prompt_map_reg_run_base_results")) {
    stop("Please load or source the `repboxRegmap` package to proceed. It is needed to generate the base results for the prompt.")
  }
  rme = rme_load(project_dir)
  base_results_str = patch_prompt_map_reg_run_base_results(base_ver_dir, rme)

  if (base_results_str == "No issues found.") {
    cat("\nNo issues found for base version", base_ver_dir, ". Nothing to patch.")
    return(NULL)
  }

  # 3. Set up the AI process (`pru`)
  patch_proc_id = paste0("patch_", base_ver_id) %>% stri_replace_first_fixed("--","+")

  pru = rai_pru_base(project_dir, prod_id = prod_id, tpl_id = "patch_map_reg_run",doc_type = doc_type, proc_id = patch_proc_id, overwrite = overwrite, to_v0 = to_v0, proc_prefix = "patch_") %>%
    rai_pru_add_tab_df() %>%
    rai_pru_add_run_do(in_context = FALSE) %>%
    rai_pru_add_tab_media(in_context = FALSE)

  if (is.null(pru)) return(NULL) # In case dependencies are missing

  # 4. Add the dynamically generated base_results to the prompt values
  pru$values$base_results = base_results_str
  
  # Run the AI to generate the patch
  proc_rai_pru(pru)

  return(invisible(pru))
}


#' Apply all unapplied patches for a given product.
#'
#' Scans a product directory for all "patch" versions, checks if a corresponding
#' "revised" version already exists, and if not, creates one by applying the patch.
#'
#' @param project_dir The project directory.
#' @param prod_id The product ID to apply patches for (e.g., "map_reg_run").
#' @param doc_type The document type (e.g., "art").
#' @param key_col The key column for group-based replacement (e.g., "regid").
#' @param ... Additional arguments passed to `fp_create_revised_version_by_group_replace`.
fp_apply_patches = function(project_dir, prod_id, doc_type, key_col, ...) {
  restore.point("fp_apply_patches")
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_dir = file.path(fp_dir, prod_id)
  
  if (!dir.exists(prod_dir)) return(invisible())

  # 1. Find all patch process directories
  patch_proc_dirs = list.files(prod_dir, pattern = "^patch_", full.names = TRUE)
  patch_proc_dirs = patch_proc_dirs[dir.exists(patch_proc_dirs)]
  if (length(patch_proc_dirs) == 0) {
    cat("\nNo patch directories found for", prod_id, "in", doc_type)
    return(invisible())
  }

  # 2. Iterate through each patch and apply if necessary
  for (patch_proc_dir in patch_proc_dirs) {
    patch_proc_id = basename(patch_proc_dir)
    # Extract the base proc_id that this patch is for
    base_proc_id = stringi::stri_replace_first_fixed(patch_proc_id, "patch_", "")
    
    # Find the corresponding base version dir (could be an 'rX_' version itself)
    base_proc_dir = file.path(prod_dir, base_proc_id)
    if (!dir.exists(base_proc_dir)) {
      cat("\nWarning: Found patch '", patch_proc_id, "' but no corresponding base directory '", base_proc_id, "'.")
      next
    }
    base_ver_dir = fp_pick_latest_ver_dir(base_proc_dir)
    patch_ver_dir = fp_pick_latest_ver_dir(patch_proc_dir)

    # Determine the name of the revised version and check if it exists
    revised_proc_id = fp_get_revised_proc_id(base_proc_id)
    revised_proc_dir = file.path(prod_dir, revised_proc_id)

    if (dir.exists(revised_proc_dir) && fp_ver_dir_ok(fp_pick_latest_ver_dir(revised_proc_dir))) {
      cat("\nRevised version for patch '", patch_proc_id, "' already exists. Skipping.")
      next
    }

    # Apply the patch
    cat("\nApplying patch:", basename(patch_ver_dir), "to base:", basename(base_ver_dir))
    fp_create_revised_version_by_group_replace(
      base_ver_dir = base_ver_dir,
      patch_ver_dir = patch_ver_dir,
      revised_ver_dir = fp_proc_dir_to_new_ver_dir(revised_proc_dir),
      key_col = key_col,
      ...
    )
  }
}

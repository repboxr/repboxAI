You've provided a clear and well-structured plan. The distinction between an AI-driven `proc_patch_...` function to *generate* a patch and a deterministic `fp_apply_patches` function to *apply* it is an excellent design choice. It separates the stochastic AI part from the predictable file-system operations.

Here are the code modifications to implement this new patching workflow.

First, I will create a new file to house the generic `fp_` functions. As I cannot modify `FuzzyProduction` directly, I'll place this in `repboxAI` but name it `fp_patch_utils.R` to signify its intended purpose as a utility library.

!MODIFICATION NEW FILE R/fp_patch_utils.R
scope = "file"
file = "R/fp_patch_utils.R"
is_new_file = true
description = '''Creates a new utility file to house generic patching and versioning functions. These are prefixed with `fp_` as they are designed to eventually be part of the `FuzzyProduction` package.'''
---
```r
# This file contains generic patching and versioning helper functions.
# They are candidates for inclusion in the FuzzyProduction package.

#' Get the proc_id for a revised version
#'
#' Increments the revision number in a process ID.
#' E.g., 'proc-id' -> 'r1_proc-id', 'r1_proc-id' -> 'r2_proc-id'.
#'
#' @param base_proc_id The process ID of the base version.
#' @return A character string with the new process ID for the revised version.
#' @export
fp_get_revised_proc_id = function(base_proc_id) {
  restore.point("fp_get_revised_proc_id")
  if (stringi::stri_startswith_fixed(base_proc_id, "r") &&
      stringi::stri_detect_regex(base_proc_id, "^r[0-9]+_")) {

    rev_num_str = stringi::stri_extract_first_regex(base_proc_id, "[0-9]+")
    rev_num = as.integer(rev_num_str) + 1

    rest = stringi::stri_sub(base_proc_id, from = nchar(rev_num_str) + 3)

    return(paste0("r", rev_num, "_", rest))
  } else {
    return(paste0("r1_", base_proc_id))
  }
}

#' Create a revised product version by applying a patch via group replacement
#'
#' This function creates a new, revised product version by applying a patch.
#' It works by replacing entire groups of rows from the base data frame with
#' corresponding groups from the patch data frame, based on a key column.
#'
#' @param base_ver_dir The version directory of the original product.
#' @param patch_ver_dir The version directory of the product containing corrections.
#' @param revised_ver_dir The directory where the new, revised version will be saved.
#' @param key_col The column name to group by for patching (e.g., "regid").
#' @return The path to the directory of the newly created revised version, invisibly.
#' @export
fp_create_revised_version_by_group_replace = function(base_ver_dir, patch_ver_dir, revised_ver_dir, key_col) {
  restore.point("fp_create_revised_version_by_group_replace")

  # 1. Load data
  base_df = fp_load_prod_df(base_ver_dir)
  patch_df = fp_load_prod_df(patch_ver_dir)

  if (!key_col %in% names(base_df) || !key_col %in% names(patch_df)) {
    stop(paste0("Key column '", key_col, "' not found in both data frames."))
  }

  # 2. Handle patch counter (.times_patched)
  if (!".times_patched" %in% names(base_df)) {
    base_df$.times_patched = 0L
  }

  base_counts = base_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_col))) %>%
    dplyr::summarize(.old_patched_count = max(.data$.times_patched, 0L, na.rm = TRUE), .groups = "drop")

  patch_df_with_counts = patch_df %>%
    dplyr::left_join(base_counts, by = key_col) %>%
    dplyr::mutate(
      .times_patched = tidyr::replace_na(.data$.old_patched_count, 0L) + 1L
    ) %>%
    dplyr::select(-.data$.old_patched_count)

  # 3. Create revised data frame by replacing patched parts
  keys_to_patch = unique(patch_df[[key_col]])
  base_df_unpatched_part = base_df %>%
    dplyr::filter(!.data[[key_col]] %in% keys_to_patch)

  revised_df = dplyr::bind_rows(base_df_unpatched_part, patch_df_with_counts)

  # 4. Save revised version
  if (!dir.exists(revised_ver_dir)) {
    dir.create(revised_ver_dir, recursive = TRUE)
  }

  fp_save_prod_df(revised_df, revised_ver_dir)

  # Save metadata for provenance
  revision_info = list(
    type = "revised_version_by_group_replace",
    base_ver_dir = base_ver_dir,
    patch_ver_dir = patch_ver_dir,
    key_col = key_col,
    revision_time = Sys.time()
  )
  saveRDS(revision_info, file.path(revised_ver_dir, "revision_info.Rds"))

  cat("\nSuccessfully created revised version in:", revised_ver_dir, "\n")
  return(invisible(revised_ver_dir))
}
```
!END_MODIFICATION NEW FILE R/fp_patch_utils.R

Now, I'll rewrite `R/proc_patches.R` to use these utilities and implement the requested workflow.

!MODIFICATION REWRITE R/proc_patches.R
scope = "file"
file = "R/proc_patches.R"
is_new_file = false
description = '''Rewrites `R/proc_patches.R` to implement a two-stage patching process. It now includes `proc_patch_map_reg_run` to generate patches with AI and `fp_apply_patches` to deterministically create revised versions from those patches.'''
---
```r
# Functions to create and apply patches to existing product versions.

example_patching_workflow = function() {
  library(repboxAI)
  library(aikit)
  # Ensure FuzzyProduction and repboxRegmap are sourced/loaded
  # source_dir("~/repbox/gemini/FuzzyProduction/R")
  # source_dir("~/repbox/gemini/repboxRegmap/R")

  rgemini::set_gemini_api_key(file = "~/repbox/gemini/gemini_api_key.txt")
  set_ai_opts(model = "gemini-1.5-pro-latest")

  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  doc_type = "art"
  
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
proc_patch_map_reg_run = function(project_dir, doc_type, base_proc_pref, overwrite = FALSE, ai_opts = get_ai_opts(), to_v0=TRUE) {
  restore.point("proc_patch_map_reg_run")

  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_id = "map_reg_run"

  # 1. Find the base version to be patched
  base_info = fp_pick_prod_ver(fp_dir, prod_id, pref = base_proc_pref)
  if (NROW(base_info) == 0) {
    cat("\nNo base version found for", prod_id, "with preference", deparse(base_proc_pref))
    return(NULL)
  }
  base_ver_dir = base_info$ver_dir
  base_proc_id = base_info$proc_id

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
  patch_proc_id = paste0("patch_", base_proc_id)

  pru = rai_pru_base(project_dir, prod_id = prod_id, tpl_id = "patch_map_reg_run",
                     doc_type = doc_type, proc_postfix = paste0("-", base_proc_id),
                     overwrite = overwrite, to_v0 = to_v0,
                     proc_prefix = "patch_") %>%
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
```
!END_MODIFICATION REWRITE R/proc_patches.R

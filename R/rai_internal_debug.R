# Correct all prod_df classes

example = function() {
  parent_dir = "~/repbox/projects_share"
  prod_id = proc_id = NULL
  rai_correct_prod_df_classes(parent_dir)
}

rai_correct_prod_df_classes = function(parent_dir, prod_id=NULL, proc_id=NULL) {
  restore.point("rai_correct_prod_df_classes")
  ver_dirs = fp_all_ver_dirs(parent_dir, prod_id=prod_id, proc_id = proc_id)
  #ver_dir = ver_dirs[1]
  prods = repbox_prods()
  for (ver_dir in ver_dirs) {
    prod_file = file.path(ver_dir, "prod_df.Rds")
    if (!file.exists(prod_file)) next
    pid = fp_ver_dir_to_prod_id(ver_dir)
    prod = prods[[pid]]
    if (is.null(prod)) next
    prod_df = readRDS(prod_file)
    new_prod_df = prod_set_df_col_class(prod_df, prod)
    if (!identical(prod_df, new_prod_df)) {
      cat("\nCorrected classes for ", ver_dir)
      saveRDS(new_prod_df, prod_file)
    }
  }
}

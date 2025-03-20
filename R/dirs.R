

project_dir_to_fp_dir = function(project_dir, doc_type = "art") {
  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  fp_dir
}

doc_dir_to_fp_dir = function(doc_dir) {
  if (!basename(dirname(doc_dir))=="doc") stop(paste0("Not a proper doc_dir: ", doc_dir))
  project_dir = dirname(dirname(doc_dir))
  doc_type = repboxDoc::rdoc_type(doc_dir)
  project_dir_to_fp_dir(project_dir, doc_type)
}

rai_fp_dir = function(project_dir, doc_type) {
  paste0(project_dir, "/fp/prod_", doc_type)
}


rai_fp_dirs = function(project_dir) {
  dir(file.path(project_dir, "fp"), pattern=glob2rx("prod_*"), recursive = FALSE, full.names=TRUE)
}

rai_ver_dir_to_project_dir = function(ver_dir) {
  dirname(dirname(dirname(dirname(ver_dir))))
}

rai_ver_dir_to_doc_type = function(ver_dir) {
  fp_dir = basename(fp_ver_dir_to_fp_dir(ver_dir))
  stri_sub(fp_dir, 6)
}


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


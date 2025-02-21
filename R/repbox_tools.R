# Will probably be moved to repboxUtils
# but keep them here during development cycle

repbox_art_pdf_file = function(project_dir) {
  pdf_dir = file.path(project_dir, "art", "pdf")
  pdf_files = list.files(pdf_dir, glob2rx("*.pdf"), full.names = TRUE)
  pdf_files
}

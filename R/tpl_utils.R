find_tpl_placeholders <- function(tpl) {
  pattern <- "\\{\\{(.*?)\\}\\}"
  # Extract matches; returns a list with one matrix.
  match_mat <- stri_match_all_regex(tpl, pattern)[[1]]

  # If no match was found, return an empty character vector.
  if (nrow(match_mat) == 0 || all(is.na(match_mat))) {
    return(character(0))
  }

  # The second column contains the captured variable names.
  unique(match_mat[, 2])
}

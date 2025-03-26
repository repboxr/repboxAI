# Will probably be moved to repboxUtils
# but keep them here during development cycle

null_to_na = function(x, na_val = NA_character_) {
  if (is.null(x)) return(na_val)
  x
}

has_col = function(x, col) {
  col %in% names(x)
}

first_nn = first.non.null = function (...) 
{
  args = list(...)
  for (val in args) {
    if (!is.null(val)) 
      return(val)
  }
  return(NULL)
}

invert_names_values = function(x) {
  y = names(x)
  names(y) = x
  y
}

add_col_left = function(df, ...) {
  args = list(...)
  restore.point("add_col_left")

  len = NROW(df)
  args = lapply(args, function(x) rep(x, length=len))
  bind_cols(as_tibble(args), df)
}

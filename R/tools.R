# Will probably be moved to repboxUtils
# but keep them here during development cycle

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

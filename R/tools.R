# Will probably be moved to repboxUtils
# but keep them here during development cycle

example = function() {
  str = c("a,b,c","b,d","b")
}

copy_into_list <- function(source = parent.frame(), dest = list(), exclude = NULL) {
  # Get all objects in the source environment
  all_vars <- ls(envir = source, all.names = TRUE)
  
  # Filter out variables to exclude
  if (length(exclude)>0) {
    vars_to_copy <- setdiff(all_vars, exclude)
  } else {
    vars_to_copy <- all_vars
  }
  
  # Vectorized assignment using mget() to get multiple objects at once
  if (length(vars_to_copy) > 0) {
    values <- mget(vars_to_copy, envir = source)
    dest[names(values)] <- values
  }
  
  # Return the updated destination list
  return(dest)
}

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

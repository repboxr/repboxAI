# glue as convenient for rai


rai_glue = function(rai, values=rai$values) {
  restore.point("rai_glue")
  if (length(values)>0) {
    rai$prompt = rai_replace_whisker(rai$tpl, values)
  } else {
    rai$prompt = rai$tpl
  }
  rai
}


rai_replace_whisker = function(txt, values) {
  restore.point("rai_replace_whisker")
  # Define regex with a capturing group for content inside {{ }}
  pattern <- "\\{\\{\\s*(.*?)\\s*\\}\\}"

  # Locate positions of all matches in the string
  matches <- stri_match_all_regex(txt, pattern)[[1]]
  if (NROW(matches)==0) return(txt)
  
  symbols = unique(matches[,2])

  vars = names(values)
  missing = setdiff(symbols, vars)
  if (length(missing)>0) {
    stop("The whisker symbols ", paste0(missing, collapse=", "), " are not in values.")
  }
  vals = unlist(values[symbols])
  whiskers =  paste0("{{", symbols,"}}")
  # Replace all occurrences simultaneously using fixed replacement
  result <- stri_replace_all_fixed(txt, whiskers, vals, vectorize_all = FALSE)
  result
}

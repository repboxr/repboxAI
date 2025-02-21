# Will probably be moved to repboxUtils
# but keep them here during development cycle

invert_names_values = function(x) {
  y = names(x)
  names(y) = x
  y
}


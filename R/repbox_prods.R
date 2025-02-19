example = function() {
  prods = repbox_fp_prods()
}

repbox_fp_prods = function() {
  prods = fp_prods(
    fp_prod("tab_base",
      list(
        artid = schema_str(is_key=TRUE, parse=FALSE),
        tabid = schema_str(is_key=TRUE),
        tabnum = schema_int(parse=FALSE)
      )
    )
  )
  prods
}


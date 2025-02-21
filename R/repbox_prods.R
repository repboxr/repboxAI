example = function() {
  prods = repbox_prods()
}

repbox_prods = function() {
  prods = prods_define(
    prod_define("tab_base",
      list(
        artid = schema_str(is_key=TRUE),
        tabid = schema_str(is_key=TRUE,maxLength = 7),
        otabid = schema_str() # ordered tabid by augmenting numbers with 0s from left
      )
    ),
    prod_define("tab_title_notes",
      widens = "tab_base",
      list(
        tabtitle = schema_str(maxLength=400),
        tabnotes = schema_str(maxLength=2000)
      )
    )

  )
  prods
}



example = function() {
  prods = repbox_prods()
}

schema_html_tab = function(...) {
  x = schema_str(...)
  class(x) = union(c("schema_html_tab", "schema_html"), class(x))
  x
}

repbox_prods = function() {
  prods = prods_define(
    prod_define("tab_list",
      descr = "List of article's tables",
      list(
        tabid = schema_str(is_key=TRUE,maxLength = 10),
        otabid = schema_str() # ordered tabid by augmenting numbers with 0s from left
      )
    ),
    prod_define("tab_tino",
      descr = "List of article's tables with extracted title and table notes",
      widens = "tab_list",
      list(
        tabtitle = schema_str(maxLength=400),
        tabnotes = schema_str(maxLength=2000)
      )
    ),
    prod_define("tab_html",
      descr = "Contains normalized HTML of every extracted article table",
      widens = "tab_tino",
      list(
        tabhtml = schema_html_tab()
      )
    )
  )
  prods
}



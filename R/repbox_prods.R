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
    ),
    prod_define(
      "cell_list",
      # means 1 parent row can have multiple children rows
      parent = "tab_html",
      from_parent = c("tabid", "otabid"),
      fields = list(
        schema_str("cellid", is_key=TRUE),
        schema_int("row"),
        schema_int("col"),
        schema_str("celltxt")
      )
    ),
    prod_define(
      "cell_base",
      widens = "cell_list",
      fields = list(
        schema_bool("is_num"),
        schema_bool("has_deci"),
        schema_str("braces", enum=c("", "(","[","{")),
        schema_bool("has_sig_star"),
        schema_str("sig_star","*")
      ),
      descr ="Can be generated purely using heuristics from td_list"
    )
  )
  prods
}



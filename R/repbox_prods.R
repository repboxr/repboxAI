example = function() {
  prods = repbox_prods()
}

schema_html_tab = function(...) {
  x = schema_str(...)
  class(x) = union(c("schema_html_tab", "schema_html"), class(x))
  x
}

get_repbox_prod = function(pid, prods = repbox_prods()) {
  prods[[pid]]
}

repbox_prods = function() {
  prods = prods_define(
    prod_define("tab_list",
      descr = "List of article's tables",
      list(
        tabid = schema_str(is_key=TRUE,maxLength = 10),
        otabid = schema_str() # ordered tabid by augmenting numbers with 0s from left
      ),
      keys = "tabid",
      order_by = "otabid"
      
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
      from_parent = c("tabid","otabid"),
      fields = list(
        cellid = schema_str(),
        row = schema_int(),
        col = schema_int(),
        text = schema_str(),
        colspan = schema_int(),
        rowspan = schema_int()
      ),
      keys = c("cellid"),
      order_by = c("otabid","cellid"),
      test_group_by = c("tabid")
    ),
    prod_define(
      "cell_base",
      widens = "cell_list",
      fields = list(
        has_num = schema_bool(),
        num_str = schema_str(),
        num = schema_num(),
        has_deci = schema_bool(descr = "Did the original string has a decimal point?"),
        num_deci = schema_int(descr = "Number of digits after decimal point in original string"),
        bracket = schema_str(enum=c("", "()","[]","{}")),
        has_sig_star = schema_bool(),
        sig_star_str = schema_str(),
        other_num_str = schema_str(descr = "Not empty if we found another number string looking from the right"),
        nchar = schema_int(),
        nchar_letters = schema_int()
      ),
      descr ="Will be generated with heuristics from cell_list. We have so many fields because they may facilitate consistency checks of the extracted tables."
    )
  )
  prods
}



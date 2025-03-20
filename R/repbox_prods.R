example = function() {
  prods = repbox_prods()
  prod = repbox_readme_prods()[["readme_overview"]]
  prod_to_json_schema(prod)
}

repbox_prod = function(pid, prods = repbox_prods()) {
  prods[[pid]]
}

repbox_prods = function() {
  c(
    repbox_tab_prods(),
    repbox_readme_prods()
  )
}


repbox_tab_prods = function() {
  prods_define(
    prod_define("tab_list",
                descr = "List of article's tables",
                list(
                  tabid = schema_str(is_key=TRUE,maxLength = 10),
                  otabid = schema_str(), # ordered tabid by augmenting numbers with 0s from left
                  tabtitle = schema_str(maxLength=400)
                ),
                keys = "tabid",
                order_by = "otabid"
    ),
    prod_define("tab_notes",
                descr = "List of article's tables with extracted title and table notes",
                widens = "tab_list",
                list(
                  tabnotes = schema_str(maxLength=2000)
                )
    ),
    prod_define("tab_html",
                descr = "Contains normalized HTML of every extracted article table",
                widens = "tab_list",
                list(
                  tabhtml = schema_html_tab()
                )
    ),
    prod_define("tab_main",
                descr = "Article tables with html, title and notes",
                widens = c("tab_html","tab_notes")          
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
        inner_html = schema_str(),
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
        nchar_letters = schema_int(),
        
        # Tests that can be quickly computed and will be added       
        flag_two_num = schema_bool(descr="Are there two numbers in the cell? Can suggests incorrect cell splits."),
        flag_two_deci = schema_bool(descr="Are there two decimal numbers in the cell? More strongly suggests wrong cell split."),
        flag_miss_bracket_below = schema_bool(descr="Do we miss a cell like (3.42) below, because such cells are below other numbers in the row?"),
        flag_miss_num_above_bracket = schema_bool(descr="Complements flag_miss_bracket_below, do we miss a cell with a normal number like 1.32 above a cell like (3.42)?")
      ),
      descr ="Will be generated with heuristics from cell_list. We have so many fields because they may facilitate consistency checks of the extracted tables."
    )
  )
}


repbox_readme_prods = function() {
  prods_define(
    prod_define(
      "readme_overview",
      fields = list(
        readme_file = schema_str("The name of the readme file."),
        is_reproduction_package_readme = schema_bool("Does the file roughly look like a typical readme file for a reproduction package? I.e. does it describe the code file and possibly the data files?"),
        describes_data = schema_bool("Does the README file describes the data used in the analysis?"),
        listed_data_set_files = schema_str("Comma separated string with names of data set files described in the README. "),
        describes_variables = schema_bool("Does the README describe at least some variables contained in the data set?"),
        missing_data = schema_bool("Does the README state that some data sets are missing in the reproduction package, e.g. because the data is confidential or proprietary?"),
        missing_confidential_data = schema_bool("Does the README explicitly state that some data sets are missing due to confidentiality reasons?"),
        missing_proprietary_data = schema_bool("Does the README explicitly state that some data sets are missing because they are propietary?"),
        data_country = schema_str("Is there information that the data sets are from a particular country? If yes, state the countries as comma separated string. If the data is from a larger region state it, e.g. EU or world."),
        data_year_start = schema_int("If the README provides information on the first year the data is from, state it. Otherwise return NA"),
        data_year_end = schema_int("If the README provides information on the last year the data is from, state it. Otherwise return NA")
      ),
      keys = c("readme_file")
    )
  )  
}




schema_html_tab = function(...) {
  x = schema_str(...)
  class(x) = union(c("schema_html_tab", "schema_html"), class(x))
  x
}

example = function() {
  prods = repbox_prods()
  names(prods)
  prod = repbox_prod("map_reg_static")
  prod_to_json_schema(prod, "arr",allow_null_def = FALSE)
}

repbox_prod = function(pid, prods = repbox_prods()) {
  prods[[pid]]
}

repbox_prods = function() {
  c(
    repbox_tab_prods(),
    repbox_readme_prods(),
    repbox_map_prods()
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
    prod_define("tab_classify",
      list(
        tabid = schema_str("The table id"),
        tab_title = schema_str("The table title"),
        panels = schema_str("Some tables exist of different panels shown above each other. If that is the case return a comma separated string with short panel IDs e.g. 'A,B,C' if it has a panel A, panel B and panel C. If no separate panels are marked just return null."),
        num_panels = schema_int("The number of explicit panels in the table. If the table does not distinguish panels, write 0."),
        shows_descriptive = schema_bool("true if the table shows descriptive statistics"),
        is_balancing_table = schema_bool("true if the table is a balancing table that shows whether certain characteristics are similarily distributed between control and treatment groups."),
        shows_regression = schema_bool("true if results of one or several regressions are shown in the table."),
        shows_did = schema_bool("true if results of a difference-in-difference regression are shown in the table."),
        shows_rdd = schema_bool("true if results of regression discontinuity design are shown in the table."),
        shows_iv_results =  schema_bool("true if results of an instrumental variable regression are shown in the table"),
        shows_iv_first_stage = schema_bool("true if results of a first stage instrumental variable regression are shown in the table."),
        shows_placebo_test = schema_bool("true if results of a placebo test are shown in the table"),
        num_regression = schema_int("The results of how many separate regressions are shown in the table?"),
        uses_panel_data =  schema_bool("true if the data set underlying the table is a panel data set."),
        short_descr =  schema_str("A short description of what is shown in the table.", allow_null = FALSE)
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
    ),
    prod_define(
      "readme_var",
      fields = list(
        readme_file = schema_str("The name of the readme file."),
        varname = schema_str("Name of the variable"),
        vardescr = schema_str("Description or label of the variable as given in the readme file."),
        dataset_files = schema_str("If the readme file mentions in which data set file(s) the variable occurs, list those data set file names as a comma separated string.")
      ),
      keys = c("readme_file")
    ),
    prod_define(
      "readme_script_tab_fig",
      fields = list(
        readme_file = schema_str("The name of the readme file."),
        script_file = schema_str("Filename of the script"),
        table_names = schema_str("Name of the table or tables that according to the README file are wholly or partially created by the script given in script_file. If the script creates multiple tables, write a comma separated list, e.g. 'Table 1, Table 3, Table A2.'"),
        figure_names = schema_str("Name of the figure or figures that according to the README file are wholly or partially created by the script. If the script creates multiple figures, write a comma separated list, e.g. 'Figure 2, Figure 5'")
      ),
      keys = c("readme_file")
    ),
    prod_define(
      "readme_data",
      fields = list(
        readme_file = schema_str("The name of the readme file."),
        dataset_file = schema_str("Name of the data set."),
        dataset_descr = schema_str("Based on the information in the README a short description of the data set in 1 to 4 sentences."),
        dataset_source = schema_str("If the README provides any information on the data set source, please state the source here."),
        is_included = schema_bool("TRUE if the README says that the data set is included in the reproduction package. FALSE if the README states that the data set is not included, e.g. because it is proprietary. If not info is given, set to null."),
        instructions_how_to_obtain_data = schema_bool("Only relevant for data sets that are not included. Does the README contain instructions of how to obtain the data set?"),
        is_intermediate_data = schema_bool("Does the README state that it is an intermediate data set, generated from other raw data sets?"),
        table_names = schema_str("If the README describes that the data set is used to generate certain tables in the article, please list all thoise tables as a comma separated list, e.g. 'Table 2, Table 3, Table A1.' If nothing is stated leave the field empty."),
        figure_names = schema_str("If the README describes that the data set is used to generate certain figures in the article, please list all thoise tables as a comma separated list, e.g. 'Figure 1, Figure 5'. If nothing is stated, leave the field empty."),
        data_country = schema_str("Is there information that the data is from one or multiple countries? If yes, state the countries as comma separated string. If the data is from a larger region state it, e.g. EU or world."),
        explicitly_stated_data_country = schema_bool("TRUE if the information about 'data_country' explicitly stated in the README file, FALSE if you guessed the information."),
        data_year_start = schema_int("If the README provides information on the first year of observatons in the data is from, state it."),
        data_year_end = schema_int("If the README provides information on the last year of observations in the data, state it."),
        explicitly_stated_data_year_start = schema_bool("TRUE if the information about 'data_year_start' explicitly stated in the README file, FALSE if you guessed the information."),
        explicitly_stated_data_year_end = schema_bool("TRUE if the information about 'data_year_end' explicitly stated in the README file, FALSE if you guessed the information."), 
        dataset_type = schema_str("Can one infer from the readme whether it is a 'panel', 'cross-section' or 'time series' data set? A 'panel' data set has a time dimensions and at least one cross sectional dimension. Example 1: A panel data set that has observations for multiple industries (cross-section dimension 1) in multiple countries (cross-section dimension 2) for multiple years (time dimension). Example 2: A panel data set with observations for multiple subjects (cross-section dimension 1) for multiple experimental rounds (time dimension). A cross-section data set has no explicit time dimension i.e. no multiple periods of observations for one cross section unit. A time-series data set only has a time dimension and just a single cross-section unit (e.g. a time series for a single country).",enum = c("panel", "cross-section","time series")),
        explicitly_stated_data_set_type = schema_bool("TRUE if the information in the README really explicitly allows to infer the data set type, FALSE if you rather guessed the information."),
        num_cross_section_dimensions = schema_int("For panel and cross-section data, can you infer the number of cross section dimensions of the data set from the README?"),
        names_cross_section_dimensions = schema_str("For cross section or panel data sets can you infer suitable names for the cross section dimensions from the README? If there are multiple cross-section dimensions return a comma separated list."),
        explicitly_stated_names_cross_section_dimensions = schema_bool("TRUE if the information in the README really explicitly allows to infer the cross section dimensions, FALSE if you rather guessed the dimesions."),
        id_cross_section_dimensions = schema_str("For cross section or panel data sets can you infer from the README which variables are the ID variables contained in the data set for each cross section dimensions (e.g. subject id, sector id, country name, etc)? If there are multiple cross-section dimensions return a comma separated list."),
        explicitly_stated_id_cross_section_dimensions = schema_bool("TRUE if the information in the README really explicitly allows to infer the cross section dimensions, FALSE if you rather guessed the dimenisons."),
        name_time_dimension = schema_str("For time series or panel data sets can you infer from the README a suitable description of the time dimenions / frequency (e.g. 'year', 'year-month', 'day', 'experimental round') and write it down?"),
        explicitly_stated_name_time_dimension = schema_bool("TRUE if the information in the README really explicitly allows to infer the type of time dimension of the data set, FALSE if you rather guessed the dimension."),
        id_time_dimension = schema_str("For time series or panel data sets can you infer from the README which variables are ID variables for the time dimension (e.g. year, month, t, period) and write it down?")
      ),
      keys = c("readme_file")
    ),
    prod_define(
      "readme_data_descr",
      fields = list(
        readme_file = schema_str("The name of the readme file."),
        dataset_file = schema_str("Name of the data set."),
        dataset_descr = schema_str("Based on the information in the README a short description of the data set in 1 to 4 sentences."),
        dataset_source = schema_str("If the README provides any information on the data set source, please state the source here.")
      ),
      keys = c("readme_file")
    )
    
    
    
  )  
}


repbox_map_prods = function() {
  prods_define(
    prod_define("tab_reg_comma",
      list(
        tabid = schema_str(),
        reg_num = schema_int("An index number indexing the regression shown in the table. Start with 1 for the first regression and increment by 1."),
        reg_cell_ids = schema_str("A comma separated list of all cell ids ")
      )
    ),
    prod_define("map_reg_static",
      list(
        tabid = schema_str("The table ID as stated in the list of tables above."),
        reg_ind = schema_int("A counter that provides a uniqe integer number for each regression that you have identified across all tables. Start with 1 and increment for each regression."),
        do_file = schema_str("The name of the do file, as listed in the list of do files above. I.e. include file paths if they are stated in the list above."),
        code_line = schema_int("The code line of the regression command in the do file, corresponding to the particular regression shown in the table. If the command extends over more than one line, write down the first line. If certain cells"),
        cell_ids = schema_str("A comma separated list of all cell ids of those cells in the HTML version of the table that correspond to this specific regression and whose value was computed by the specified code line. Each cell id can be found as the 'id' tag of the corresponding <td> or <th> element of the HTML version of the article's table. Only add cells that show numeric results, e.g. estimated coefficient, or number of observations, but no title cells or cells showing variable labels. Some tables in articles are structured such that some descriptive statistics, like the number of observations are shown on the bottom of a column and apply to multiple regressions shown in that column. Add the corresponding cell id for every regression, that they apply to.")
      )
    )
  )
}




schema_html_tab = function(...) {
  x = schema_str(...)
  class(x) = union(c("schema_html_tab", "schema_html"), class(x))
  x
}

example = function() {
  prods = repbox_prods()
  names(prods)
  prod = repbox_prod("reg_classify_static")
  prod = repbox_prod("map_reg_run")
  prod = repbox_prod("map_inv_reg_run")
  
  prod = repbox_prod("readme_overview")
  prod = repbox_prod("readme_vs_guide")
  prod_to_json_schema(prod, "obj",allow_null_def = TRUE)
  prod = repbox_prod("readme_data")
  prod_to_json_schema(prod, "arr",allow_null_def = FALSE)
  
  
  # write all json schemas so they 
  # can be used in AI prompts
  prods = repbox_prods()
  i = 1
  for (i in seq_along(prods)) {
    prod_name = names(prods[i])
    file = paste0("~/repbox/gemini/repboxAI/inst/prod_schemas/", prod_name, ".json")
    json = prod_to_json_schema(prods[[i]], "obj",, allow_null_def = FALSE)
    writeLines(json, file)  
  }
}

repbox_prod = function(pid, prods = repbox_prods()) {
  prods[[pid]]
}

repbox_prods = function() {
  c(
    repbox_tab_prods(),
    repbox_readme_prods(),
    repbox_map_prods(),
    repbox_other_prods()
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
        data_year_end = schema_int("If the README provides information on the last year the data is from, state it. Otherwise return NA"),
        missing_data_set_files = schema_str("Comma separated string with names of those data set files described flagged in the README as not included in the reproduction package, e.g. because they are proprietary or confidental."),
        included_data_set_files = schema_str("Comma separated string with names of those data set files described as being included in the reproduction package."),
        generated_data_set_files = schema_str("Comma separated string with names of those data set files described as being generated by the code in the reproduction package. Precomputed versions of the generated data sets may sometimes be already included in the reproduction package."),
        analyst_notes = schema_str("Any other relevant information, context, or ambiguities noted in the README related to the fields above. Be brief. Analyst notes can often remain empty.")
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
        dataset_file = schema_str("Name of the data set. If a concrete file name is mentioned use that file name with extension. Sometimes the rows of a larger data set are distrbuted over several files with similar naming convetions. E.g. files like pop_de.csv, pop_fr.csv, pop_uk.csv, ... which all contain population data for a different country. In that case list up to three data set files from the larger data set as a comma separated list and add the glob pattern that matches all files in the next field dataset_file_glob."),
        dataset_file_glob = schema_str("Only relevant if the data set rows are distributed over several files with similar naming convention. Then write down the glob pattern that matches all files. E.g. if the data set is in a set of files like pop_de.csv, pop_fr.csv, pop_uk.csv, ... which all contain population data for a different country. Write pop_*.csv. If not relevant, write just an empty string."),
        dataset_descr = schema_str("Based on the information in the README a short description of the data set in 1 to 4 sentences."),
        dataset_source = schema_str("If the README provides any information on the data set source, please state the source here."),
        is_included = schema_bool("TRUE if the README says that the data set is included in the reproduction package. FALSE if the README states that the data set is not included, e.g. because it is proprietary. If not info is given, set to null.",allow_null = TRUE),
        instructions_how_to_obtain_data = schema_bool("Only relevant for data sets that are not included. Does the README contain instructions of how to obtain the data set?"),
        is_intermediate_data = schema_bool("Does the README state that it is an intermediate data set, generated from other raw data sets?"),
        table_names = schema_str("If the README describes that the data set is used to generate certain tables in the article, please list all thoise tables as a comma separated list, e.g. 'Table 2, Table 3, Table A1.' If nothing is explicitly stated just write an empty string. "),
        figure_names = schema_str("If the README describes that the data set is used to generate certain figures in the article, please list all thoise tables as a comma separated list, e.g. 'Figure 1, Figure 5'. If nothing is explicitly stated just write an empty string."),
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
        id_time_dimension = schema_str("For time series or panel data sets can you infer from the README which variables are ID variables for the time dimension (e.g. year, month, t, period) and write it down?"),
        analyst_notes = schema_str("Any other relevant information, context, or ambiguities noted in the README file regarding this dataset. Be brief. Analyst notes can often remain empty.")
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
    ),
    prod_define(
      "readme_vs_guide",
      fields = list(
        readme_file = schema_str("The name of the readme file being analyzed."),
        overview_is_present = schema_bool("TRUE if an overview section is present at the beginning of the README."),
        overview_mentions_runtime = schema_bool("TRUE if the overview gives an estimate of the total runtime for the replication."),
        data_provenance_is_present = schema_bool("TRUE if a section discussing data availability and provenance is present."),
        data_provenance_no_external_data_is_stated = schema_bool("TRUE if the README explicitly states that no external data is used."),
        data_provenance_rights_statement_is_present = schema_bool("TRUE if any statement about rights to use or redistribute data is included."),
        data_provenance_rights_claims_permission_to_use = schema_bool("TRUE if the author certifies legitimate access and permission to use the data."),
        data_provenance_rights_claims_permission_to_redistribute = schema_bool("TRUE if the author certifies permission to redistribute the data in the package."),
        data_provenance_license_is_present = schema_bool("TRUE if a subsection on data licensing is present."),
        data_provenance_license_summary = schema_str("A brief summary of the data license mentioned (e.g., 'CC-BY-NC', 'Public Domain')."),
        data_provenance_availability_summary = schema_str("The stated summary of data availability."),
        data_provenance_sources_details_is_present = schema_bool("TRUE if detailed descriptions for individual data sources are provided."),
        data_provenance_sources_details_uses_table_format = schema_bool("TRUE if the recommended tabular format (Data.Name, Location, Provided, Citation) is used to list data sources."),
        dataset_list_is_present = schema_bool("TRUE if a section describing each data file in the package is present."),
        dataset_list_uses_table_format = schema_bool("TRUE if a tabular format (Data file, Source, Notes, Provided) is used to list the files."),
        computational_reqs_is_present = schema_bool("TRUE if a section on computational requirements is present."),
        computational_reqs_software_is_present = schema_bool("TRUE if software requirements are listed."),
        computational_reqs_software_mentions_setup_script = schema_bool("TRUE if the README mentions a program/script to install dependencies (e.g., '0_setup.do', 'requirements.txt')."),
        computational_reqs_software_listed = schema_str("A comma-separated list of major software mentioned (e.g., 'Stata, Python, R, Matlab')."),
        computational_reqs_randomness_is_present = schema_bool("TRUE if the README mentions controlled randomness or pseudo-random number generator seeds."),
        computational_reqs_randomness_seed_is_set = schema_bool("TRUE if the README states that a random seed is set and ideally where."),
        computational_reqs_performance_is_present = schema_bool("TRUE if performance requirements (runtime, storage, hardware) are described."),
        computational_reqs_performance_estimated_runtime = schema_str("The approximate time stated to run the analysis (e.g., '1-2 hours', '> 14 days')."),
        computational_reqs_performance_estimated_storage = schema_str("The approximate storage space needed (e.g., '250 MB - 2 GB')."),
        computational_reqs_performance_mentions_hardware = schema_bool("TRUE if specific hardware (e.g., CPU cores, RAM, OS) used for the analysis is described."),
        code_description_is_present = schema_bool("TRUE if a high-level overview of the program files and their purpose is given."),
        code_description_license_is_present = schema_bool("TRUE if a subsection on code licensing is present."),
        code_description_license_summary = schema_str("A brief summary of the code license mentioned (e.g., 'MIT', 'GPL')."),
        replication_instructions_is_present = schema_bool("TRUE if a section with step-by-step instructions for the replicator is present."),
        replication_instructions_is_linear_sequence = schema_bool("TRUE if the instructions are presented as a simple, clear, step-by-step list as recommended by the template."),
        output_mapping_is_present = schema_bool("TRUE if a list or table mapping outputs (tables, figures) to the programs that generate them is present."),
        output_mapping_reproducibility_claim = schema_str("The claim made about which outputs can be reproduced."),
        output_mapping_uses_table_format = schema_bool("TRUE if the recommended tabular format (Figure/Table #, Program, Output file) is used for the mapping."),
        references_section_is_present = schema_bool("TRUE if a 'References' section is present at the end of the README."),
        analyst_summary = schema_str("A brief, one-to-three sentence summary of how well the README conforms to the template, noting any major omissions.")
      ),
      keys = c("readme_file")
    )
  )  
}


repbox_map_prods = function() {
  prods_define(
    prod_define("map_reg_static",
      list(
        tabid = schema_str("The table ID as stated in the list of tables above."),
        reg_ind = schema_int("A counter that provides a uniqe integer number for each regression that you have identified across all tables. Start with 1 and increment for each regression."),
        script_file = schema_str("The name of the script file, as listed in the list of script files above. I.e. include file paths if they are stated in the list above."),
        code_line = schema_int("The code line of the regression command in the script file, corresponding to the particular regression shown in the table. If the command extends over more than one line, write down the first line. If certain cells"),
        cell_ids = schema_str("A comma separated list of all cell ids of those cells in the HTML version of the table that correspond to this specific regression and whose value was computed by the specified code line. E.g. for a table with tabid='2', this comma separated string of cell ids might look like 'c2_10,c2-12,c2-14'. Each cell id can be found as the 'id' tag of the corresponding <td> or <th> element of the HTML version of the article's table. Only add cells that show numeric results, e.g. estimated coefficient, or number of observations, but no title cells or cells showing variable labels. Some tables in articles are structured such that some descriptive statistics, like the number of observations are shown on the bottom of a column and apply to multiple regressions shown in that column. Add the corresponding cell id for every regression, that they apply to.")
      )
    ),
    prod_define("map_reg_run", list(
      tabid = schema_str("The table ID as stated in the list of tables above."),
      reg_ind = schema_int("A counter that provides a uniqe integer number for each regression that you have identified across all tables. Start with 1 and increment for each regression."),
      cell_ids = schema_str("A comma separated list of all cell ids of those cells in the HTML version of the table that correspond to this specific regression and whose value was computed by the specified code line and correspond to the specified output. E.g. for a table with tabid='2', this comma separated string of cell ids might look like 'c2_10,c2_12,c2_14'. Each cell id can be found as the 'id' tag of the corresponding <td> or <th> element of the HTML version of the article's table. Only add cells that show numeric results, e.g. estimated coefficient, or number of observations, but no title cells or cells showing variable labels. Some tables in articles are structured such that some descriptive statistics, like the number of observations are shown on the bottom of a column and apply to multiple regressions shown in that column. Add the corresponding cell id for every regression, that they apply to."),
      runid = schema_int("The unique runid identifying the regression output in the script file. If no output chunk is provided for the regression or post regression command, set null.", allow_null=TRUE),
      script_file = schema_str("The corresponding name of the Stata script file.", allow_null=TRUE),
      script_num = schema_int("The number of the script file, map the script file name to the script_num shown in the table of Stata scripts in the prompt.", allow_null=TRUE),
      code_line = schema_int("The code line of the regression command in the script file.", allow_null=TRUE),
      wrong_number_cases = schema_arr(descr = "Sometimes, but quite rarely, there was a transcription error when the article was written such that one or multiple numbers shown in the regression table don't correspond to the actual numbers computed by the Stata command in the replication package. If you find such cases for the current regression, note them in this array. Don't note cases where numbers between the Stata output and table only differ because they are rounded to a different number of digits or formatted differently, which is very common and no problem. Usually all numbers are transcribed correctly and this array will be empty.", items=schema_obj(
        properties = list(
          cell_id = schema_str("The cell_id of the table cell showing the wrong number, e.g. 'c2_10'"),
          wrong_number_in_cell = schema_num("The wrong number shown in the cell. Ignore special formating and just write down the numeric value."),
          number_in_stata_output = schema_num("The actual number shown in the output of the Stata regression.")
        )
      )),
      problem = schema_str("Did you encounter a problem related to this mapping? If yes describe it here. Typically this field will be empty. Only write something if there is an important problem that substantially hampers this mapping task.", allow_null = TRUE)
    )),
    prod_define("map_inv_reg_run",list(
      runid = schema_int("The unique runid of the regression as stated in the table above. It is also shown in title of the corresponding output chunk of the text file containing all Stata scripts with regression outputs."),
      script_file = schema_str("The name of the Stata script file that contains the regression."),
      script_num = schema_int("The corresponding number of the script file as shown in the table of all Stata scripts in the prompt."),

      code_line = schema_int("The code line of the regression command in its Stata script. For regression commands spaning more than one line, use the first code line."),
      tabid = schema_str("If you can map the regression to a particular table shown in the list above, state here the corresponding tabid. Otherwise set null.", allow_null = TRUE),
      figid = schema_str("If the regression is used to generate a particular figure in article or appendix, please note here the figure id. For example, if a figure is called 'Figure 5' the figid would be just '5', if a figure is called 'Fugure A.1' the figid would be 'A.1'.", allow_null = TRUE),
      cell_ids = schema_str("Relevant if the run regression can be mapped to particular table. A comma separated list of all cell ids of those cells in the HTML version of the table that correspond to the specific run regression. E.g. for a table with tabid='2', this comma separated string of cell ids might look like 'c2_10,c2-12,c2-14'. Each cell id can be found as the 'id' tag of the corresponding <td> or <th> element of the HTML version of the article's tables. Only add cells that show numeric results, e.g. estimated coefficient, or number of observations, but no title cells or cells showing variable labels. Some tables in articles are structured such that some descriptive statistics, like the number of observations are shown on the bottom of a column and apply to multiple regressions shown in that column. Add the corresponding cell id for every regression, that they apply to."),
      problem = schema_str("Did you encounter a problem related to this mapping? If yes describe it here. Typically this field will be empty. Only write something if there is an important problem that substantially hampers this mapping task.", allow_null = TRUE)
    )),
    prod_define("reg_classify_static", list(
      tabid = schema_str("The table ID as stated in the list of regressions above."),
      reg_ind = schema_int("The regression index as stated in the field 'reg_ind' in the list of regressions shown above."),
      short_descr =  schema_str("A short description of what the regression analyzes based on the information in the article.", allow_null = FALSE),
      is_did_reg = schema_bool("true if the regression performs a difference-in-difference (DID) analysis."),
      is_rdd_reg = schema_bool("true if the regression performs a regression discontinuity design (RDD) analysis."),
      is_iv_reg =  schema_bool("true if it is an instrumental varibale regression"),
      is_iv_first_stage_reg = schema_bool("true if the shown regression results correspond to the first stage regression of an instrumental variable regression (in the script the command could be an iv regression with the option to show first stage results or it could be a separate OLS first stage regression)"),
      is_placebo_test = schema_bool("true if the regression performs a placebo test, or a similar permutation test."),
      is_pref_spec_in_tab = schema_bool("Often tables show multiple regression specifications and sometimes the authors state which specification is their preferred specification. Set true if this regression is the preferred specification  among the specifications shown in the table."),
      is_main_result = schema_bool("true if the regression results are described as main results of the article (compared to robustness checks or additional results)"),
      is_additional_result = schema_bool("true if the regression shows additional results that are not described as main results of the article"),
      is_robustness_check = schema_bool("true if the regression is mainly a robustness check for other results."),
      label_dep_var = schema_str("Based on the information in the article and table find a suitable label for the dependent variable in the regression."),
      labels_coef_of_interest = schema_str("Often regression tables show both coefficients of primary interest for the analysis and coefficients for control variables that are not of primary interest. Sometimes only coefficient of primary interest are shown. Please state the variable lables of the coefficients of primary interest for this regression as shown in the table. If there are multiple variables of primary interest your string shall be a comma separted list, e.g. 'age,gender'."),
      cell_id_coef_of_interest = schema_str("Please state the cell ids of all cells for this regression that show the numeric value of the  coefficient of interests or their standard error / p-value / t-value. Return a comma separated list of all those cell_ids, like 'c2_10,c2-12'."),
      analyses_heterogeneity = schema_bool("true if the  cofficient of interest of the regressions analyze heterogenous effects, e.g. if the regression provided information on how treatment effect sizes differ between subgroups."),
      error_in_prompt_or_media = schema_str("Is there some inconsistency in the prompt or the attached media files, e.g. the media don't show the tables listed in the prompt etc. This could be an indicator for some error in my pipeline. If such an inconsistency exists, briefly describe it only for the first regression. If all seems ok, set to an empty string. In later regressions always set to an empty string.")
      
      
    ))
  )
}



repbox_other_prods = function() {
  prods_define(
    prod_define(
      "privacy_breach",
      fields = list(
        dataset     = schema_str("Exact name of the dataset (file or table)."),
        variables   = schema_str("Comma‑separated list of variable names that, alone or jointly, create the privacy breach."),
        explanation = schema_str("Brief justification of why these variables constitute a breach."),
        risk_level  = schema_str("Severity of the breach: must be one of 'low', 'moderate', or 'high'.", enum=c("low","moderate","high"))
      )
    )
  )  
}



schema_html_tab = function(...) {
  x = schema_str(...)
  class(x) = union(c("schema_html_tab", "schema_html"), class(x))
  x
}

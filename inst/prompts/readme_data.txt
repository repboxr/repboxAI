You are an economic research assisstant who works very carefully.

Attached is the file {{readme_file}} from a reproduction package of an economic article. Some README files describe the data sets used in the analysis. If that is the case, please analyse the text of the README file and return a JSON array that satisfies the structure below. If some information is not provided, set the value of the corresponding field to null. If no data sets are described, just return an empty json array.

JSON RESPONSE SCHEMA:

{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "dataset_file": {
        "type": "string",
        "description": "Name of the data set."
      },
      "dataset_descr": {
        "type": "string",
        "description": "Based on the information in the README a short description of the data set in 1 to 4 sentences."
      },
      "dataset_source": {
        "type": "string",
        "description": "If the README provides any information on the data set source, please state the source here."
      },
      "is_included": {
        "type": "bool",
        "description": "TRUE if the README says that the data set is included in the reproduction package. FALSE if the README states that the data set is not included, e.g. because it is proprietary. If not info is given, set to null."
      },
      "instructions_how_to_obtain_data": {
        "type": "bool",
        "description": "Only relevant for data sets that are not included. Does the README contain instructions of how to obtain the data set?"
      },
      "is_intermediate_data": {
        "type": "bool",
        "description": "Does the README state that it is an intermediate data set, generated from other raw data sets?"
      },
      "table_names": {
        "type": "string",
        "description": "If the README describes that the data set is used to generate certain tables in the article, please list all thoise tables as a comma separated list, e.g. 'Table 2, Table 3, Table A1.' If nothing is stated leave the field empty."
      },
      "figure_names": {
        "type": "string",
        "description": "If the README describes that the data set is used to generate certain figures in the article, please list all thoise tables as a comma separated list, e.g. 'Figure 1, Figure 5'. If nothing is stated, leave the field empty."
      },
      "data_country": {
        "type": "string",
        "description": "Is there information that the data is from one or multiple countries? If yes, state the countries as comma separated string. If the data is from a larger region state it, e.g. EU or world."
      },
      "explicitly_stated_data_country": {
        "type": "bool",
        "description": "TRUE if the information about 'data_country' explicitly stated in the README file, FALSE if you guessed the information."
      },
      "data_year_start": {
        "type": "integer",
        "description": "If the README provides information on the first year of observatons in the data is from, state it."
      },
      "data_year_end": {
        "type": "integer",
        "description": "If the README provides information on the last year of observations in the data, state it."
      },
      "explicitly_stated_data_year_start": {
        "type": "bool",
        "description": "TRUE if the information about 'data_year_start' explicitly stated in the README file, FALSE if you guessed the information."
      },
      "explicitly_stated_data_year_end": {
        "type": "bool",
        "description": "TRUE if the information about 'data_year_end' explicitly stated in the README file, FALSE if you guessed the information."
      },
      "dataset_type": {
        "type": "string",
        "description": "Can one infer from the readme whether it is a 'panel', 'cross-section' or 'time series' data set? A 'panel' data set has a time dimensions and at least one cross sectional dimension. Example 1: A panel data set that has observations for multiple industries (cross-section dimension 1) in multiple countries (cross-section dimension 2) for multiple years (time dimension). Example 2: A panel data set with observations for multiple subjects (cross-section dimension 1) for multiple experimental rounds (time dimension). A cross-section data set has no explicit time dimension i.e. no multiple periods of observations for one cross section unit. A time-series data set only has a time dimension and just a single cross-section unit (e.g. a time series for a single country).",
        "enum": ["panel", "cross-section", "time series"]
      },
      "explicitly_stated_data_set_type": {
        "type": "bool",
        "description": "TRUE if the information in the README really explicitly allows to infer the data set type, FALSE if you rather guessed the information."
      },
      "num_cross_section_dimensions": {
        "type": "integer",
        "description": "For panel and cross-section data, can you infer the number of cross section dimensions of the data set from the README?"
      },
      "names_cross_section_dimensions": {
        "type": "string",
        "description": "For cross section or panel data sets can you infer suitable names for the cross section dimensions from the README? If there are multiple cross-section dimensions return a comma separated list."
      },
      "explicitly_stated_names_cross_section_dimensions": {
        "type": "bool",
        "description": "TRUE if the information in the README really explicitly allows to infer the cross section dimensions, FALSE if you rather guessed the dimesions."
      },
      "id_cross_section_dimensions": {
        "type": "string",
        "description": "For cross section or panel data sets can you infer from the README which variables are the ID variables contained in the data set for each cross section dimensions (e.g. subject id, sector id, country name, etc)? If there are multiple cross-section dimensions return a comma separated list."
      },
      "explicitly_stated_id_cross_section_dimensions": {
        "type": "bool",
        "description": "TRUE if the information in the README really explicitly allows to infer the cross section dimensions, FALSE if you rather guessed the dimenisons."
      },
      "name_time_dimension": {
        "type": "string",
        "description": "For time series or panel data sets can you infer from the README a suitable description of the time dimenions / frequency (e.g. 'year', 'year-month', 'day', 'experimental round') and write it down?"
      },
      "explicitly_stated_name_time_dimension": {
        "type": "bool",
        "description": "TRUE if the information in the README really explicitly allows to infer the type of time dimension of the data set, FALSE if you rather guessed the dimension."
      },
      "id_time_dimension": {
        "type": "string",
        "description": "For time series or panel data sets can you infer from the README which variables are ID variables for the time dimension (e.g. year, month, t, period) and write it down?"
      }
    }
  }
} 

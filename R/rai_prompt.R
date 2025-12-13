# Generate typical media and context used in rebox analyses
example = function() {
  library(repboxAI)
  library(restorepoint)
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  outfile = rai_media_all_static_do(project_dir)
  outfile = rai_media_all_tab_html(project_dir)
  rstudioapi::filesPaneNavigate(dirname(outfile))
  
  ver_dir = "~/repbox/projects_share/aeri_1_2_6/fp/prod_art/map_reg_static/g25pe-pdf-g2f/v0"
  map_df = fp_load_prod_df(ver_dir)
  
}

merge_unique_comma_str = function(str) {
  paste0(unique(unlist(stri_split_fixed(str, ",") 
  )), collapse=",")
}

rai_prompt_value_reg_list_static = function(map_df, values=list()) {
  # Transform map_df to reg_df
  map_df = rename.cols(map_df, "do_file","script_file")
  reg_df = map_df %>%
    group_by(tabid, regid) %>%
    summarize(
      cell_ids = merge_unique_comma_str(cell_ids),
      script_files = merge_unique_comma_str(script_file),
      code_lines = merge_unique_comma_str(code_line)
    ) %>%
    ungroup()
  values$reg_list_static = text_table(reg_df)
  values
}


rai_prompt_value_reg_run_list = function(reg_df, values=list()) {
  values$reg_run_list = text_table(reg_df[c("runid")])
  values
}


rai_prompt_value_tab_list = function(tab_df, values = list(), header = c("tabid", "tab_title")) {
  values$tab_list = text_table(tab_df[c("tabid", "tabtitle")], header=header)
  values
}

rai_prompt_value_script_list = function(script_df, values = list(), header = c("script_file")) {
  restore.point("rai_prompt_value_script_list")
  values$script_list = text_table(script_df[c("script_file")], header=header)
  values
}

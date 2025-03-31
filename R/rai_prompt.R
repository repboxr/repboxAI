# Generate typical media and context used in rebox analyses
example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  outfile = rai_media_all_static_do(project_dir)
  outfile = rai_media_all_tab_html(project_dir)
  rstudioapi::filesPaneNavigate(dirname(outfile))
}

rai_prompt_value_tablist = function(tab_df, values = list(), header = c("tabid", "tab_title")) {
  values$tablist = text_table(tab_df[c("tabid", "tabtitle")], header=header)
  values
}

rai_prompt_value_dolist = function(script_df, values = list(), header = c("do_file")) {
  values$dolist = text_table(script_df$file, header=header)
  values
}

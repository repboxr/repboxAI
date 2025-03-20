# Rank tab versions and pick a standard

example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  doc_type = "art"
  doc_type = "app1"
  
  rstudioapi::filesPaneNavigate(project_dir)
}

repbox_ev_tab = function(project_dir, doc_type="art") {
  fp_dir = project_dir_to_fp_dir(project_dir, doc_type)
  ev_dir = file.path(project_dir, "fp", paste0("eval_", doc_type))
  if (!dir.exists(ev_dir)) dir.create(ev_dir)
  
  fp_all_ver_dirs(fp_dir, "cell_base")
  cell_base = fp_load_all_prod_df(fp_dir, "cell_base")
  cell_base = cell_base %>% group_by(ver_id, tabid) %>%
    filter(endsWith(ver_id, "v0"))
  
  deci_df = cell_base %>% filter(has_num & is.true(has_deci))
  id_li = list(
    deci_df %>%
      arrange(row, col) %>%
      summarize(ind = "by_row_deci", value = paste0(bracket,num_str, collapse="|")),
    deci_df %>%
      arrange(col,row) %>%
      summarize(ind = "by_col_deci", value = paste0(bracket,num_str, collapse="|")),
    deci_df %>%
      arrange(num_str, bracket) %>%
      summarize(ind = "set_deci", value =  paste0(bracket,num_str, collapse="|"))
  )
  id_df = bind_rows(id_li)    

  group_df = id_df %>%
    group_by(tabid, ind, value) %>%
    summarize(
      group = list(ver_id),
      group_size = n()
    )
  
  ev_df = id_df %>%
    group_by(tabid, ind) %>%
    summarize(
      num_ver = n(),
      num_group = n_distinct(value)
    )
  
} 

left_join_all = function(li, by=NULL) {
  df = li[[1]]
  for (i in setdiff(seq_along(li),1)) {
    df = left_join(df, li[[i]]) 
  }
  df
}

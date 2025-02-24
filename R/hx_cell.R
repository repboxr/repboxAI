example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  hx_all_tab_html_to_cell_list(project_dir)
}

hx_all_tab_html_to_cell_list = function(project_dir) {
  dir = file.path(project_dir, "rai/prod_runs/tab_html")
  df_files = list.files(dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE) 
  file = df_files[1]
  for (file in df_files) {
    run_dir = dirname(file)
    if (ddp_is_up_to_date(run_dir, "cell_list")) next
    hx_tab_html_to_cell_list(run_dir = run_dir)
    #stop()
  }
}

# cell_list is a derived prod of tab_html
hx_tab_html_to_cell_list = function(hx, run_dir=hx$run_dir, prods=repbox_prods(), prod_df=hx$prod_df, version=hx$version) {
  restore.point("hx_tab_html_to_cell_list")
  prod = prods[["cell_list"]]
  
  hx = ddp_init_hx(hx, run_dir=run_dir,ddp_pid="cell_list", version = version)
  if (is.null(prod_df)) {
    df = readRDS(file.path(hx$run_dir, "prod_df.Rds"))
  } else{
    df = prod_df
  }
  i = 1
  cell_df = bind_rows(lapply(seq_len(NROW(df)), function(i) {
    cell_df = add_col_left(normalized_html_tab_to_cell_df(df$tabhtml[[i]]), tabid=df$tabid[i],otabid = df$otabid[i])
    cell_df$cellid = paste0(df$tabid[i],"_", stri_sub(cell_df$cellid,6))
    cell_df      
  }))
  prod_df = df_to_prod_df(cell_df, prod)
  hx = hx_save(hx,prod_df)
  #rstudioapi::filesPaneNavigate(hx$run_dir)
  invisible(hx)  
}


example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"

  run_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/cell_base/tab_html-n-g2flp-0-pdf-1/r0"
  run_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/cell_base/tab_html_hx_pdf/r0"
  prod_df = rai_load_prod_df(run_dir=run_dir)
  
  test_cell_base(run_dir)
  
  run_dirs = test_all_cell_base(project_dir)

}

test_all_cell_base = function(project_dir, overwrite=FALSE) {
  tests = tests = define_tests_cell_base()
  prod = get_repbox_prod("cell_base")
  run_dirs = rai_all_run_dirs(project_dir, "cell_base")
  for (run_dir in run_dirs) {
    test_cell_base(run_dir,prod=prod, tests=tests)
  }
  return(run_dirs)
}

test_cell_base = function(run_dir, prod_df=rai_load_prod_df(run_dir=run_dir), prod = get_repbox_prod("cell_base"), tests = define_tests_cell_base()) {
  restore.point("test_cell_base")
  tests = define_tests_cell_base()
  test_df = prod_run_tests(tests,prod_df=prod_df, prod=prod, return_details = TRUE)
  
  # create HTML output
  tab_df = cells_to_tabhtml(cell_df, add_flags=TRUE)
  rai_write_all_tables_html(tab_df, "table_test.html", run_dir=run_dir, title="Tests: cell_base")
  rstudioapi::filesPaneNavigate(run_dir)
}

define_tests_cell_base = function() {
  tests = prod_tests_define(
    # flags are already part of cell_base
    flag_test_funs(
      function(df, ...) return(df)
    ),
    descr = list(),
    keys = "cellid"
  )
    
}

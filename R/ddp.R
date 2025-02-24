# Directly derived products
# Example: cell_list from tab_html
#
# Each run of tab_html can be deterministically transformed into a cell_list
# We use the same version and runid for directly derived products
#
# Below are helper functions




#' Sometimes we make a deterministic transformation step, like
#' tab_html to cell_list
#' We then may want to simply keep the version and run names of 
#' the original run and just change the product. 
ddp_get_run_dir = function(run_dir, ddp_pid) {
  run_id = basename(run_dir)
  vid = basename(dirname(run_dir))
  ddp_run_dir = file.path(project_dir, "rai/prod_runs/",ddp_pid, vid, run_id)
}

ddp_is_up_to_date = function(run_dir, ddp_pid) {
  restore.point("ddp_is_up_to_date")
  ddp_run_dir = hx_run_dir_to_derived_prod(run_dir, "cell_list")
  if (!dir.exists(ddp_run_dir)) return(FALSE)
  ddp_file = file.path(ddp_run_dir,"prod_df.Rds")
  if (!file.exists(ddp_file)) return(FALSE)
  
  file = file.path(run_dir, "prod_df.Rds")
  if (isTRUE(file.mtime(ddp_file) >= file.mtime(file))) return(TRUE)
  return(FALSE)
}

#' Sometimes we make a deterministic transformation step, like
#' tab_html to cell_list
#' We then may want to simply keep the version and run names of 
#' the original run and just change the product. 
ddp_init_hx = function(hx=NULL, ddp_pid, prods = repbox_prods(), version=hx$version, run_dir=hx$run_dir) {
  restore.point("hx_to_ddp_prod")
  if (is.null(run_dir)) stop("You must provide at least run_dir (original run_dir). Or the complete original hx / rais object.")
  if (is.null(project_dir)) project_dir = str.left.of(run_dir, "rai/prod_runs/")
  if (is.null(version)) {
    version = readRDS(file.path(run_dir, "version.Rds"))
  }
  input_info = data.frame(pid=version$pid, run_dir = run_dir, found=TRUE, num_cand = 1)
  version$pid = ddp_pid
  ddp_prod = prods[ddp_pid]
  
  ddp_run_dir = ddp_get_run_dir(run_dir, ddp_pid) 
  ddp_hx = hx_init(project_dir, version, prod,input_info=input_info, run_dir=ddp_run_dir)
  ddp_hx
}

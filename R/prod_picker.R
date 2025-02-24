# Tools to pick a particular instance of a product

# Pick an input product: returns the run dir
rai_pick_input_prod = function(project_dir, pid) {
  if (NROW(pid)>1) {
    li = lapply(pid, function(pi) rai_pick_input_prod(project_dir, pi))
    return(bind_rows(li))
  }
  restore.point("rai_pick_input_prod")
  runs_dir = file.path(project_dir, "rai", "prod_runs", pid)
  files = list.files(runs_dir, glob2rx("prod_df.Rds"), full.names = TRUE, recursive = TRUE)
  num_cand = length(files)
  if (num_cand==0) {
    return(data.frame(pid=pid, run_dir = NA_character_, found=FALSE, num_cand = num_cand))
  }
  # current default: select youngest
  time = file.mtime(files)
  ind = which.max(time)
  data.frame(pid=pid, run_dir = dirname(files[ind]), found=TRUE, num_cand = num_cand)
}

input_prod_err = function(inputs, show_msg = TRUE) {
  not_found = inputs$pid[!inputs$found]
  if (length(not_found)>0) {
    msg = paste0("\nCould not valid versions of the input product(s) ", paste0(not_found, collapse=", "),"\n")
    cat(msg)
    return(TRUE)
  }
  FALSE
}

rai_load_input_prods = function(inputs, pid) {
  if (!missing(pid)) {
    inputs = inputs[inputs$pid %in% pid,]
  } else {
    pid = NULL
  }
  restore.point("rai_load_input_prods")
  prod_li = lapply(seq_len(NROW(inputs)), function(i) {
    rai_load_prod_df(inputs$run_dir[i])
  })
  names(prod_li) = inputs$pid
  prod_li
}

rai_load_prod_df = function(run_dir = rai_prod_pick_run_dir(project_dir, pid), project_dir=NULL, pid=NULL) {
  if (is.null(run_dir)) return(NULL)
  restore.point("rai_load_prod_df")
  file = file.path(run_dir, "prod_df.Rds")
  readRDS(file)
}

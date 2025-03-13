# Repbox AI process infos

rai_make_proc_info = function(prod_id, ai_opts,tpl_file=NA,json_mode=TRUE, use_schema=FALSE, raw=FALSE,  ...) {
  schema_opt = case_when(
    use_schema ~ "s",
    json_mode ~ "j",
    TRUE ~ "n"
  )
  model_short = rai_model_short(ai_opts$model)
  proc_id = paste0(prod_id, "-",schema_opt, "-", model_short,"-", round(10*ai_opts$temperature,0))
  if (raw) {
    proc_id = paste0("raw_", proc_id)
  }
  extra_args = list(...)
  restore.point("repbox_ai_version")
  if (length(extra_args)>0) {
    extra_str = do.call(paste, c(extra_args, list(sep="-")))
    proc_id = paste0(proc_id, "-", extra_str)
  }
  proc_info = c(list(proc_id=proc_id, prod_id=prod_id,model=ai_opts$model,model_short = model_short, temperature=ai_opts$temperature, tpl_file=tpl_file, tpl_base = basename(tpl_file), json_mode=json_mode, use_schema=use_schema, raw=raw), extra_args)
  proc_info = as_tibble(proc_info)
  proc_info
}


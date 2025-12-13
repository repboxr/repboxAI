# Repbox AI process infos

rai_make_proc_info = function(prod_id, ai_opts,tpl_file=NA,json_mode=TRUE, use_schema=FALSE, raw=FALSE,tpl_id=NULL,doc_file_form=NULL, proc_prefix="",proc_postfix = "",proc_id=NULL,  ...) {
  schema_opt = case_when(
    use_schema ~ "s",
    json_mode ~ "j",
    TRUE ~ "n"
  )
  schema_opt_str = ifelse(schema_opt=="s", "s","")
  temp_str = ifelse(is.true(ai_opts$temperature > 0), paste0("t",round(10*ai_opts$temperature,0)),"")
  model_short = rai_model_short(ai_opts$model)
  if (is.null(proc_id)) {
    proc_id = paste0(proc_prefix,model_short,schema_opt_str, temp_str, proc_postfix)
  }
  if (raw) {
    proc_id = paste0("raw_", proc_id)
  }
  extra_args = list(...)
  restore.point("repbox_ai_version")
  if (length(extra_args)>0) {
    extra_str = do.call(paste, c(extra_args, list(sep="-")))
    proc_id = paste0(proc_id, "-", extra_str)
  }
  proc_info = c(list(proc_id=proc_id, prod_id=prod_id,model=ai_opts$model,model_short = model_short, temperature=ai_opts$temperature, tpl_id=null_to_na(tpl_id), tpl_file=tpl_file, tpl_base = basename(tpl_file), json_mode=json_mode, use_schema=use_schema, raw=raw), doc_file_form=null_to_na(doc_file_form), extra_args)
  proc_info = as_tibble(proc_info)
  proc_info
}


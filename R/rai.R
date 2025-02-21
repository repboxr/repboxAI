get_ai_opts = function() {
  ai_opts = list(model = "gemini-2.0-flash", temperature=0, short_model = "2.0f")
}

ai_model_short = function(model) {
  case_when(
    model == "gemini-2.0-flash" ~ "g2f"
  )
}


# version characterizes the general parameters of the repbox AI call not specific
# to a particular project
# Not all general infos will be stored, e.g. not a schema object
rai_version = function(pid, ai_opts,tpl_file=NA,json_mode=TRUE, use_schema=FALSE,  ...) {
  schema_opt = case_when(
    use_schema ~ "s",
    json_mode ~ "j",
    TRUE ~ "n"
  )
  model_short = ai_model_short(ai_opts$model)
  vid = paste0(pid, "-",schema_opt, "-", model_short,"-", round(10*ai_opts$temperature,0))
  extra_args = list(...)
  restore.point("repbox_ai_version")
  if (length(extra_args)>0) {
    extra_str = do.call(paste, c(extra_args, list(sep="-")))
    vid = paste0(vid, "-", extra_str)
  }
  version = c(list(vid=vid, pid=pid,model=ai_opts$model,model_short = model_short, temperature=ai_opts$temperature, tpl_file=tpl_file, tpl_base = basename(tpl_file), json_mode=json_mode, use_schema=use_schema), extra_args)
  version = as_tibble(version)
  if (NROW(version)!=1) {
    stop("Problem in rai_version. Resulting data frame should have exactly one row. Make sure you call it correctly.")
  }
  version
}


# rai stands for repbox AI call. It contains a version object and additional information specific to the AI call
rai_init = function(project_dir, version, schema=NULL, values = NULL, media_files=NULL, tpl=NULL, tpl_file = version$tpl_file, model=version$model, temperature=version$temperature, json_mode=version$json_mode) {
  restore.point("rai_init")
  #undebug(to_json_schema)
  schema = to_json_schema(schema)
  if (is.null(tpl) & !is.null(tpl_file)) {
    tpl = merge.lines(suppressWarnings(readLines(tpl_file)))
  } else if (is.null(tpl)) {
    stop("provide a template tpl or tpl_file or haver version$tpl_file filled.")
  }

  rai = list(project_dir = project_dir, tpl=tpl, model=model, schema=schema, json_mode=version$json_mode, temperature=temperature, media_files=media_files, version=version)
}

rai_run = function(rai, values=rai$values) {
  restore.point("rai_run")
  if (is.null(rai[["prompt"]]))
    rai = rai_glue(rai, values)

  if (!startsWith(rai$model,"gemini")) {
    stop(paste0("Cannot yet run model ", model))
  }

  library(rgemini)
  rai$media = repbox_ai_media(rai$project_dir, rai$media_files)

  rai$time_stamp = Sys.time()
  rai$httr_res = try(run_gemini(rai$prompt, model=rai$model,response_schema = rai[["schema"]], json_mode=rai$json_mode,temperature = rai$temperature, media = rai$media,just_content = FALSE, httr_response = TRUE), silent=TRUE)
  restore.point("rai_run_post")

  rai$run_sec = as.numeric(Sys.time()) - as.numeric(rai$time_stamp)

  rai$df_res = try(gemini_result_to_df(rai$httr_res), silent=TRUE)
  rai$content = try(gemini_content(rai$df_res), silent=TRUE)

  rai$has_error = is(rai$content, "try-error")
  if (rai$has_error) {
    cat("\n  Error running gemini request...")
  }
  class(rai) = c("repbox_rai","list")

  return(rai)
}

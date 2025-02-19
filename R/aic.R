get_ai_opts = function() {
  ai_opts = list(model = "gemini-2.0-flash", temperature=0, short_model = "2.0f")

}

ai_model_short = function(model) {
  case_when(
    model == "gemini-2.0-flash" ~ "2f"
  )
}

ai_short_version = function(ai_opts, short=NULL) {
  paste0(ai_model_short(ai_opts$model),".", round(10*ai_opts$temperature,0))
}

# aic stands for AI call
aic_init = function(tpl, model=ai_opts$model, response_schema=NULL,json_mode=!is.null(response), temperature=ai_opts$temperature, values = NULL, media_files=NULL, ai_opts = get_ai_opts()) {
  aic = list(tpl=tpl, model=model, response_schema=response_schema, json_mode=json_mode, temperature=temperature, media_files=media_files)
}

aic_media_upload = function(aic, media_files = aic$media_files, mime_type = NULL) {
  restore.point("aic_media_upload")
  aic$media_files = media_files

  if (startsWith(aic$model,"gemini")) {
    library(rgemini)
    aic$uploaded_media = gemini_media_upload(aic$media_files, mime_type=mime_type)
    aic$upload_time = Sys.time()
  }
  aic
}


aic_run = function(aic, values=aic$values) {
  restore.point("aic_run")
  library(xglue)
  prompt = xglgue(aic$tpl, values)
  if (startsWith(aic$model,"gemini")) {
    library(rgemini)
    aic$httr_res = try(run_gemini(prompt, model=aic$model,response_schema = aic$response_schema, json_mode=aic$json_mode,temperature = aic$temperature, media = aic$uploaded_media,just_content,just_content = FALSE, httr_response = TRUE))
    aic$df_res = try(gemini_result_to_df(aic$httr_resp))
    aic$content = try(gemini_content(aic$df_res))
    return(aic)
  } else {
    stop(paste0("Cannot yet run model ", model))
  }
}

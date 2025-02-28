rai_tpl_dir = function() {
  system.file("prompts", package="repboxAI")
  "~/repbox/gemini/repboxAI/inst/prompts"
}


rai_model_short = function(model) {
  case_when(
    model == "gemini-2.0-flash" ~ "g2f",
    model == "gemini-1.5-flash-001" ~ "g15f",
    model == "gemini-2.0-flash-lite-preview-02-05" ~ "g2flp",
    model == "gemini-2.0-flash-lite" ~ "g2fl",
    model == "gemini-2.0-flash-thinking-exp" ~ "g2fte",
    TRUE ~ model    
  )
}

rai_init = function(project_dir,json_mode=first_nn(proc_info$json_mode,FALSE), schema=NULL, values = NULL, context=NULL, media_files=NULL, tpl=NULL, tpl_file = proc_info$tpl_file,   model=ai_opts$model, temperature=ai_opts$temperature, proc_info=NULL, ai_opts=get_ai_opts() ) {
  
  ai_init(project_dir, json_mode=json_mode, schema=schema, values=values, context=context, media_files=media_files, tpl=tpl, tpl_file=tpl_file, model=model, temperature=temperature, ai_opts=ai_opts)
}

# Simple wrapper to ai_context
#
# Allow to easily add typical repbox files like art_pdf
rai_context = function(project_dir, model=ai_opts$model, media_files =NULL, prompt=NULL, ttl_sec=60*5, add_art_pdf = FALSE, cache_context = isTRUE(ai_opts$cache_context), api_key = getOption("gemini_api_key"), ai_opts = get_ai_opts()) {
  restore.point("rai_context")
  if (add_art_pdf) {
    media_files = union(repbox_art_pdf_file(project_dir), media_files)
  }
  ai_context(project_dir,model = model, media_files=media_files, prompt=prompt, ttl_sec=ttl_sec, cache_context = cache_context, api_key=api_key, ai_opts=ai_opts)
}



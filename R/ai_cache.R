
repbox_ai_cache = function(project_dir) {
  cache = getOption("repbox_ai_cache")
  if (!isTRUE(cache$project_dir == project_dir)) {
    cache = new.env(parent = emptyenv())
    cache$media_cache = new.env(parent=emptyenv())
    cache$data_set_li = list()
    cache$context_cache = new.env(parent = emptyenv())
    cache$project_dir = project_dir
    options(repbox_ai_cache = cache)
  }
  cache
}

repbox_ai_media = function(project_dir, media_files, refresh_sec = 60*45, verbose=TRUE) {
  restore.point("repbox_ai_media")
  if (length(media_files)==0)
    return(NULL)
  media = lapply(media_files, function(media_file) repbox_ai_single_media(project_dir, media_file, refresh_sec, verbose=verbose))
  media
}

# Upload media or return previously uploaded media
repbox_ai_single_media = function(project_dir, media_file, refresh_sec = 60*45, verbose=TRUE) {
  restore.point("repbox_ai_sinle_media")
  mcache = repbox_ai_cache(project_dir)$media_cache
  media = mcache[[media_file]]
  outdated = TRUE
  if (!is.null(media)) {
    outdated = isTRUE(as.numeric(Sys.time())-as.numeric(media$time_upload) > refresh_sec)
  }
  if (outdated) {
    cat(paste0("\nUpload ", media_file, " to gemini\n"))
    media = rgemini::gemini_media_upload(media_file)
    media$time_upload = Sys.time()
    media$local_file = media_file
    mcache[[media_file]] = media
  }
  media
}

repbox_ai_context = function(project_dir, model=ai_opts$model, media_files =NULL, prompt=NULL, ttl_sec=60*5, add_art_pdf = FALSE, cache_context = isTRUE(ai_opts$cache_context), api_key = getOption("gemini_api_key"), ai_opts = get_ai_opts()) {
  restore.point("repbox_ai_context")
  if (add_art_pdf) {
    media_files = union(repbox_art_pdf_file(project_dir), media_files)
  }
  hash = paste0("h",digest::digest(list(project_dir=project_dir, model=model, prompt=prompt, media_files=media_files)))
  ccache = repbox_ai_cache(project_dir)$context_cache
  context = ccache[[hash]]
  if (!is.null(context) & cache_context) {
    context = gemini_update_context_cache(context, ttl_sec = ttl_sec)
    return(context)
  }
  media = repbox_ai_media(project_dir = project_dir,media_files = media_files)
  context = gemini_context(prompt=prompt,model = model,media = media, ttl_sec=ttl_sec,do_cache = cache_context, api_key = api_key)
  context$project_dir = project_dir
  ccache[[hash]] = context
  context
}

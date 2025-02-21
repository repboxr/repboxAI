
repbox_ai_cache = function(project_dir) {
  cache = getOption("repbox_ai_cache")
  if (!isTRUE(cache$project_dir == project_dir)) {
    cache = new.env(parent = emptyenv())
    cache$media_cache = new.env(parent=emptyenv())
    cache$data_set_li = list()
    cache$project_dir = project_dir
    options(repbox_ai_cache = cache)
  }
  cache
}


# repbox_ai_project_data = function(project_dir, data_set, load_fun) {
#   cache = repbox_ai_cache(project_dir)
#   data_set_li = cache$data_set_li
#   existing = names(data_set_li)
#   if (data_set %in% existing) {
#     return()
#   }
#
#   sets = names(data_sets)
#   new = setdiff(sets, names(cache$data_sets))
#   if (length(new)>0) {
#
#   }
# }

repbox_ai_media = function(project_dir, media_files, refresh_sec = 60*45) {
  restore.point("repbox_ai_media")
  if (length(media_files)==0)
    return(NULL)
  media = lapply(media_files, function(media_file) repbox_ai_single_media(project_dir, media_file, refresh_sec))
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

get_rate_limit_sleeper_object = function(gap=20, gap_if_limited=60, max_limited=2, reset=FALSE) {
  rls = getOption("rate_limit_sleeper_object")
  if (is.null(rls) | reset) {
    rls = list(gap=gap, gap_if_limited=gap_if_limited,max_limited=max_limited, num_limited=0)
  }
  rls
  
}

pru_rate_limit_sleep = function(gap=20, gap_if_limited=60,max_limited=2,  pru=fp_get_backup_last_run_pru(),rls=get_rate_limit_sleeper_object(gap=gap, gap_if_limited=gap_if_limited)) {
  rls$gap=gap
  rls$gap_if_limited = gap_if_limited
  rls$max_limited = max_limited
  if (pru_had_rate_limit(pru)) {
    delay_sec = pru_get_retry_delay(pru)
    rls$num_limited = rls$num_limited + 1
    if (rls$num_limited > rls$max_limited) {
      cat("\n", rls$num_limited, " rate limits encountered in sequence, stop execution.")
      return(list(stop=TRUE))
    }
    if (!is.null(delay_sec)) {
      cat("\nPause for suggested delay: ", delay_sec, " seconds.\n")
    } else {
      delay_sec = rls$gap_if_limited
      cat("\nPause for ", delay_sec, " seconds after rate limit.\n")
    }
  } else {
    rls$num_limited = 0
    delay_sec = rls$gap
    cat("\nPause for ", delay_sec, " seconds.\n")
  }
  rls$last_check = Sys.time()
  options(rate_limit_sleeper_object=rls)
  Sys.sleep(delay_sec)
  return(list(stop=FALSE))
  
}


pru_had_rate_limit = function(pru=fp_get_backup_last_run_pru()) {
  item = pru_get_newest_item(pru)
  isTRUE(item$fine_status=="rate_limit")
}

pru_get_retry_delay = function(pru=fp_get_backup_last_run_pru()) {
  restore.point("pru_get_retry_delay")
  item = pru_get_newest_item(pru)
  retry_delay_to_sec = function(str) {
    str = na.omit(str)
    if (length(str)==0) return(NULL)
    if (nchar(str)==0) return(NULL)
    
    str = str[1]
    restore.point("retry_delay")
    cat("\nTransform retry delay string:", str,"\n")
    if (endsWith(str,"s")) return(as.numeric(str.remove.ends(str, right=1)))
  }

  delay = NULL  
  #try({
    res = fromJSON(item$resp_json)  
    delay = retry_delay_to_sec(res$error$details$retryDelay)
    
    if (!is.null(delay)) {
      try(cat(toJSON(res$error$details$violations,auto_unbox = TRUE,pretty = TRUE)))
    }
  #})      
  delay
}



pru_get_newest_item = function(pru) {
  items = pru$items
  if (length(items)==0) return(NULL)
  time_stamps = sapply(items, function(item) item$time_stamp)
  ind = which.max(time_stamps)
  items[[ind]]
}

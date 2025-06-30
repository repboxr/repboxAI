# Create Stata log files for AI

example = function() {
  #chooseCRANmirror()
  #install.packages("dplyr")
  library(repboxDB)
  library(repboxTableTools)
  library(repboxAI)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_1_2_4"
  outfile = rai_media_run_do(project_dir)
  rstudioapi::filesPaneNavigate(dirname(outfile))
}


opts_media_run_do = function(do_max_runs=100000,line_max_runs=100,log_max_char=100000,...) {
  opts = copy_into_list()
  opts
}


rai_media_run_do = function(project_dir, parcels = list(), output_just_runid = NULL, opts=opts_media_run_do(), just_reg_log=TRUE, header=NULL) {
  restore.point("rai_media_run_do")
  parcels = repdb_load_parcels(project_dir, c("stata_source", "stata_run_cmd", "stata_run_log","stata_cmd"), parcels=parcels)
  script_df = parcels$stata_source$script_source
  
  cmd_df = parcels$stata_cmd$stata_cmd %>% left_join(script_df %>% select(file_path, script_num), by="file_path")
  log_df = parcels$stata_run_log$stata_run_log
  run_df = parcels$stata_run_cmd$stata_run_cmd
  run_df = left_join(run_df, cmd_df %>% select(artid, file_path, line, is_reg), by = c("artid", "file_path", "line"))
  
  if (just_reg_log) {
    script_nums = unique(cmd_df$script_num[cmd_df$is_reg])
    script_df = script_df[script_df$script_num %in% script_nums,]
    cmd_df = cmd_df[cmd_df$script_num %in% script_nums,]
    
    if (is.null(output_just_runid)) {
      output_just_runid = run_df$runid[run_df$is_reg]
    } else {
      output_just_runid = intersect(output_just_runid, run_df$runid[run_df$is_reg])
    }
    if (is.null(header)) {
      header="This file contains all Stata do scripts that contain at least one regression command. 

Line numbers for each script are shown.      
      
For regression commands the logged output is shown below the code line in a block like this example:
      
```output runid=25
. reg y x1 x2, robust
Linear regression                               Number of obs     =        749
                                                F(37, 711)        =       4.94

[Further output ommited in example]

```end_ouput runid=25

If the regression command was called multiple times inside a loop, multiple output blocks are shown for that code line.
            "

    }
    
  }
  
  if (!is.null(output_just_runid)) {
    log_df = log_df %>% filter(runid %in% output_just_runid)
    run_df = run_df %>% filter(runid %in% output_just_runid)
  }
  run_df = run_df  %>%
    left_join(log_df %>% select(runid, logtxt), by="runid") %>%
    left_join(script_df %>% select(file_path, script_num), by="file_path")
  
  run_df = run_df %>%
    adapt_too_big_run_df(opts=opts)
  
  ldf = script_df %>%
    mutate(
      txt = stri_split_fixed(script_df$text,"\n")
    ) %>%
    select(script_num, txt) %>%
    tidyr::unnest(txt) %>%
    group_by(script_num) %>%
    mutate(
      orgline = seq_along(txt),
      #txt = htmltools::htmlEscape(txt)
    )
  
  ldf = left_join(ldf,
    select(cmd_df, orgline_start, orgline_end, line, is_reg, cmd, script_num),
    join_by(script_num, between(orgline,orgline_start,orgline_end))
  )
  
  ldf = ldf %>%
    mutate(
      line_changes = !is.true(lag(line)==line | is.na(lag(line) & is.na(line))),
      line_group = cumsum(line_changes)
    ) %>%
    group_by(line_group) %>%
    mutate(
      orgline_start = min(orgline),
      orgline_end = max(orgline)
    )
  
  #pad_len = max(2, ceiling(log(max(ldf$orgline+1),10)))
  #stri_pad_left(ldf$orgline, pad_len)
  block_df = ldf %>%
    group_by(script_num, orgline_start, orgline_end, line) %>%
    summarize(
      code_txt =  paste0(orgline,": ",txt, collapse="\n")
    ) %>%
    ungroup()
      
  # Now we aggregate log on a line level
  loli_df = run_df %>%
    #left_join(run_df %>% select(runid, line, cmdline), by=c("runid")) %>%
    mutate(
      # correct weird log output
      logtxt = ifelse(endsWith(trimws(cmdline),"{"), "", logtxt),
      logtxt = paste0(". ", cmdline, "\n", logtxt),
      # reduce logtext that is too long
      len_logtxt = nchar(logtxt),
      logtxt = ifelse(is.true(len_logtxt > opts$log_max_char), paste0(substring(logtxt,1, opts$log_max_char), "\n... further output omitted ..."), logtxt)
    ) %>%
    left_join(block_df %>% select(line, script_num, orgline_start), by = c("line", "script_num")) %>%
    mutate(
      log_md = paste0('\n```output runid=', runid,'\n',logtxt,'\n```end_output runid=', runid,"\n")
    ) %>%
    group_by(script_num, line) %>%
    summarize(
      log_md = paste0(log_md, collapse="\n")
    )
  
  code_df = block_df %>%
    left_join(loli_df, by = c("script_num","line")) %>%
    mutate(
      log_md = na_val(log_md, ""),
      txt = paste0(code_txt, log_md)
    )
  
  
  all_df = code_df %>%
    left_join(script_df %>% select(file_path, script_num), by="script_num") %>%
    group_by(script_num) %>%
    summarize(
      script_html = paste0('\n\n#########################\nScript: ',  first(file_path), '\n#########################\n\n', paste0(txt, collapse="\n"))
    )

  text = paste0(header,paste0(all_df$script_html, collapse="\n"))
  outdir = paste0(project_dir, "/fp/prompt_files")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  outfile = file.path(outdir, "do_run.txt")
  writeUtf8(text, outfile)
  return(outfile)
}




adapt_too_big_run_df = function(run_df, opts) {
  restore.point("adapt_too_big_run_df")
  run_df = run_df %>%
    group_by(script_num, line) %>%
    mutate(line.run.count = 1:n()) %>%
    ungroup() %>%
    filter(line.run.count <= opts$line_max_runs) %>%
    group_by(script_num) %>%
    mutate(do.run.count = 1:n()) %>%
    ungroup() %>%
    filter(do.run.count <= opts$do_max_runs)
  
  run_df
}



# rai_media_run_do_old = function(project_dir, parcels = list(), output_just_runid = NULL, opts=opts_do_run_html()) {
#   restore.point("rai_media_run_do")
#   parcels = repdb_load_parcels(project_dir, c("stata_source", "stata_run_cmd", "stata_run_log","stata_cmd"), parcels=parcels)
#   script_df = parcels$stata_source$script_source
#   
#   cmd_df = parcels$stata_cmd$stata_cmd %>% left_join(script_df %>% select(file_path, script_num), by="file_path")
#   log_df = parcels$stata_run_log$stata_run_log
#   run_df = parcels$stata_run_cmd$stata_run_cmd
#   
#   if (!is.null(output_just_runid)) {
#     log_df = log_df %>% filter(runid %in% output_just_runid)
#     run_df = run_df %>% filter(runid %in% output_just_runid)
#   }
#   run_df = run_df  %>%
#     left_join(log_df %>% select(runid, logtxt), by="runid") %>%
#     left_join(script_df %>% select(file_path, script_num), by="file_path") %>%
#     adapt_too_big_run_df(opts=opts)
#   
#   ldf = script_df %>%
#     mutate(
#       txt = stri_split_fixed(script_df$text,"\n")
#     ) %>%
#     select(script_num, txt) %>%
#     tidyr::unnest(txt) %>%
#     group_by(script_num) %>%
#     mutate(
#       orgline = seq_along(txt),
#       #txt = htmltools::htmlEscape(txt)
#     )
#   
#   ldf = left_join(ldf,
#     select(cmd_df, orgline_start, orgline_end, line, is_reg, cmd, script_num),
#     join_by(script_num, between(orgline,orgline_start,orgline_end))
#   )
#   
#   ldf = ldf %>%
#     mutate(
#       line_changes = !is.true(lag(line)==line | is.na(lag(line) & is.na(line))),
#       line_group = cumsum(line_changes)
#     ) %>%
#     group_by(line_group) %>%
#     mutate(
#       orgline_start = min(orgline),
#       orgline_end = max(orgline)
#     )
#   
#   block_df = ldf %>%
#     group_by(script_num, orgline_start, orgline_end, line) %>%
#     summarize(
#       code_html = paste0('<pre class="do_code" script_num=', first(script_num),' line=',first(orgline_start),' line_end = ', first(orgline_end),'>\n',htmltools::htmlEscape(paste0(txt, collapse="\n")),'\n</pre>')
#     ) %>%
#     ungroup()
#       
#   # Now we aggregate log on a line level
#   loli_df = run_df %>%
#     #left_join(run_df %>% select(runid, line, cmdline), by=c("runid")) %>%
#     mutate(
#       # correct weird log output
#       logtxt = ifelse(endsWith(trimws(cmdline),"{"), "", logtxt),
#       # reduce logtext that is too long
#       len_logtxt = nchar(logtxt),
#       logtxt = ifelse(is.true(len_logtxt > opts$log_max_char), paste0(substring(logtxt,1, opts$log_max_char), "\n... further output omitted ..."), logtxt)
#     ) %>%
#     #left_join(cmd_df %>% select(line, script_num), by = c("line", "script_num") ) %>%
#     left_join(block_df %>% select(line, script_num, orgline_start), by = c("line", "script_num")) %>%
#     mutate(
#       log_html = paste0('\n<pre class="do_output" runid=', runid,' line="', orgline_start,'">\n',htmltools::htmlEscape(logtxt),'\n</pre>')
#     ) %>%
#     group_by(script_num, line) %>%
#     summarize(
#       log_html = paste0(log_html, collapse="\n")
#     )
#   
#   code_df = block_df %>%
#     left_join(loli_df, by = c("script_num","line")) %>%
#     mutate(
#       log_html = na_val(log_html, ""),
#       html = paste0(code_html, log_html)
#     )
#   
#   
#   all_df = code_df %>%
#     left_join(script_df %>% select(file_path, script_num), by="script_num") %>%
#     group_by(script_num) %>%
#     summarize(
#       script_html = paste0('<h2>Script <span class="script_num">', first(script_num), '</span>: <span class="script_file">', first(file_path),'</span></h2><br><div class="code_and_output">', paste0(html, collapse="\n"),"</div>")
#     )
#   
#   head_html = paste0(
# '<!DOCTYPE html>
# <html lang="en">
# <head>
#   <meta charset="UTF-8">
#   <title>Stata do files including output of successfully run code lines</title>
# <style>
# /* Base styling for all pre blocks */
# pre {
#   margin: 0px;
#   padding: 1px;
#   /*border-radius: 8px;*/
#   font-family: Consolas, Monaco, monospace;
#   /*line-height: 1.5;*/
#   overflow-x: auto;
# }
# 
# /* Stata code blocks */
# .do_code {
#   margin: 1px;
#   background-color: #f0f8ff; /* soft blue */
#   position: relative;
# }
# 
# /* Stata log output blocks */
# .do_output {
#   margin-left: 3em;
#   background-color: #f9f9f9; /* light gray */
#   position: relative;
#   color: #444;
# }
# </style>
# ')
#   html = paste0(head_html, paste0(all_df$script_html, collapse="\n"), '</body></html>')
#   outdir = paste0(project_dir, "/fp/prompt_files")
#   if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#   outfile = file.path(outdir, "do_run.html")
#   writeUtf8(html, outfile)
#   return(outfile)
# }




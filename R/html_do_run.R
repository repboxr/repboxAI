# Create Stata log files for AI

example = function() {
  library(repboxDB)
  library(repboxTableTools)
  library(repboxAI)
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  repbox_do_run_html(project_dir)
}


opts_do_run_html = function(do_max_runs=8000,line_max_runs=25,log_max_char=50000,...) {
  opts = copy_into_list()
  opts
}

repbox_do_run_html = function(project_dir, parcels = list(), opts=opts_do_run_html()) {
  restore.point("repbox_do_run_html")
  parcels = repdb_load_parcels(project_dir, c("stata_source", "stata_run_cmd", "stata_run_log","stata_cmd"), parcels=parcels)
  script_df = parcels$stata_source$script_source
  
  cmd_df = parcels$stata_cmd$stata_cmd %>% left_join(script_df %>% select(file_path, script_num), by="file_path")
  log_df = parcels$stata_run_log$stata_run_log
  run_df = parcels$stata_run_cmd$stata_run_cmd %>%
    left_join(log_df %>% select(runid, logtxt), by="runid") %>%
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
  
  ldf = ldf %>%
    left_join(select(cmd_df, orgline, line, is_reg, cmd, script_num), by=c("orgline","script_num"))
  
  cmd_df = tibble(orgline_start=c(1,5), orgline_end = c(3,5), line=c(1,5))
  ldf = tibble(orgline =1:7)
  ldf = left_join(ldf,
                  #select(cmd_df, orgline_start, orgline_end, line, is_reg, cmd, script_num),
                  cmd_df,
                  join_by(between(orgline,orgline_start,orgline_end))
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
  
  ldf = ldf %>%
    group_by(script_num, orgline_start, orgline_end, line) %>%
    summarize(
      code_html = paste0('<pre class="do_code" data-script_num="', script_num,'" data-line="',orgline_start,'" data-line_end = "', orgline_end,'">\n',htmltools::htmlEscape(paste0(txt, collapse="\n"),'\n</pre>')
                         
      )
      
      # Now we aggregate log on a line level
      loli_df = run_df %>%
        #left_join(run_df %>% select(runid, line, cmdline), by=c("runid")) %>%
        mutate(
          # correct weird log output
          logtxt = ifelse(endsWith(trimws(cmdline),"{"), "", logtxt),
          # reduce logtext that is too long
          len_logtxt = nchar(logtxt),
          logtxt = ifelse(is.true(len_logtxt > opts$log_max_char), paste0(substring(logtxt,1, opts$log_max_char), "\n... further output omitted ..."), logtxt)
        ) %>%
        left_join(cmd_df %>% select(line, orgline_start, script_num), by = c("line", "script_num") ) %>%
        mutate(
          #cmdline_html = paste0('<pre id="cmd-runid-',runid,'">', cmdline,"</pre>"),
          log_html = paste0('<pre class="do_output" data-outputid="', runid,'" data-line="', orgline_start,'">',logtxt,'</pre>')
        ) %>%
        group_by(script_num, line, orgline_start) %>%
        summarize(
          log_html = paste0(log_html, collapse="\n")
        )
      
      code_df = ldf %>%
        left_join(loli_df, by = c("script_num","line")) %>%
        mutate(
          log_html = na_val(log_html, ""),
          html = paste0(code_html, log_html)
        )
      
      all_df = code_df %>%
        group_by(script_num) %>%
        summarize(
          script_html = paste0('<h2 class="script_file">', first(file_path),'</h2><span class="script_num">script_num = ', first(script_num),'</span><br><div class="code_and_output">', paste0(html, collapse="\n"),"</div>")
        )
      
      return(all_df$script_html)
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



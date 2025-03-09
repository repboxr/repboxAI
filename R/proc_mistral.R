# Not yet fully implemented...

example = function() {
 project_dir = "~/repbox/projects_share/aejapp_1_2_4" 
 restore.point.options(display.restore.point = TRUE)
 pages = repbox_project_add_mistral_ocr(project_dir)
 
 html = paste0(pages$html, collapse = "\n")
 
 pru = proc_tab_mistral(project_dir)
 rstudioapi::filesPaneNavigate(pru$ver_dir)
 
}

proc_tab_mistral = function(project_dir, to_v0=TRUE) {
  restore.point("proc_tab_mistral")
  ocr_dir = file.path(project_dir, "mistral_ocr")
  tab_file = file.path(ocr_dir, "tab_df.Rds")
  if (!file.exists(tab_file)) {
    res = repbox_project_add_mistral_ocr(project_dir)
  }
  if (!file.exists(tab_file)) return(NULL)
  tab_df = readRDS(tab_file)

  tab_df$tabid = tabid_normalize(tab_df$tabid)
  tab_df$otabid = tabid_to_otabid(tab_df$tabid)
  
  #tab_df = tab_df[1,]  
  # prepare cell_df: split multirow / multicol latex formulas
  tab_df$cell_df = lapply(tab_df$cell_df, function(cell_df) {
    restore.point("shfjhsfk")
    cell_df$content = cell_df$inner_html
    row_df = split_html_latex_multiline(cell_df$content)
    cell_df = cell_df_split_rows_and_cols(cell_df, row_df)
    cell_df$text = cell_df$content
    cell_df = cells_add_cell_base(cell_df,add_tests = TRUE, split_multi_num = TRUE)
    cell_df
  })

  
  tab_df$tabhtml = sapply(tab_df$cell_df, cell_df_to_simple_tabhtml)
  
  prod_id = "tab_html"
  prod = get_repbox_prod(prod_id)
  fp_dir = project_dir_to_fp_dir(project_dir)
  proc_id = paste0("tab_html_mistral_ocr")
  proc_info = data.frame(prod_id=prod_id, proc_id = proc_id)
  
  pru = pru_init(fp_dir,prod_id=prod_id, proc_info=proc_info, to_v0=to_v0)
  prod_df = df_to_prod_df(tab_df, prod)
  
  pru = pru_save(pru, prod_df)
  pru_backport_save(pru, get_repbox_prod("tab_list"), prod_df)
  pru_cell = proc_tab_html_to_cell_list(pru=pru, prod_df=prod_df, also_cell_base=TRUE)
  rai_write_all_tables_html(prod_df, "tables.html",out_dir = pru$ver_dir)
  #rstudioapi::filesPaneNavigate(pru$ver_dir)
  invisible(pru)
}

repbox_project_add_mistral_ocr = function(project_dir) {
  restore.point("repbox_project_add_mistral_ocr")
  artid = basename(project_dir)
  ocr = repbox_load_mistral_ocr(artid)
  if (is.null(ocr)) return(NULL)
  ocr_dir = file.path(project_dir, "mistral_ocr")
  if (!dir.exists(ocr_dir)) dir.create(ocr_dir)
  pages = ocr$pages
  if (NROW(pages)==0) return(NULL)

  library(rmarkdown)
  library(rmistral)
  
  
  pages_dir = file.path(ocr_dir, "pages")
  if (!dir.exists(pages_dir)) dir.create(pages_dir)
  rmistral::mistral_ocr_save_md(ocr, file.path(pages_dir,"page.md"),by_page = TRUE, overwrite=FALSE,save_images = FALSE)
  
    
  saveRDS(ocr, file.path(ocr_dir, "ocr.Rds"))
  
  
  pages$html = rep(NA_character_, NROW(pages))  
  p = 1
  for (p in 1:NROW(pages)) {
    md_file = paste0(pages_dir,"/page--", p, ".md")
    html_file = paste0(pages_dir,"/page--", p, ".html")
    
    #pandoc_convert(md_file,output=html_file, options = c("--mathjax","--wrap=preserve"))
    #suppressMessages(suppressWarnings(pandoc_convert(verbose=FALSE, md_file,output=html_file, options = c("--wrap=preserve"))))
    my_pandoc(md_file,html_file, args = c("--wrap=preserve","--mathjax"))
    
    pages$html[[p]] = paste0(readLines(html_file), collapse="\n")    
  }
  pages$page = 1:NROW(pages)
  page_df = pages[,c("page","markdown","html")]
  stri_count_fixed(page_df$html, "<table")
  html = paste0(page_df$html, collapse = "\n")
  saveRDS(page_df, file.path(ocr_dir, "page_df.Rds"))
  tab_df = mistral_html_extract_tables(html)
  saveRDS(tab_df, file.path(ocr_dir, "tab_df.Rds"))

  md_file = paste0(ocr_dir,"/art.md")
  rmistral::mistral_ocr_save_md(ocr, md_file,by_page = FALSE, overwrite=FALSE,save_images = TRUE)
  html_file = paste0(ocr_dir,"/art.html")
  my_pandoc(md_file,html_file,c("--wrap=preserve", "--mathjax", "--standalone"))
  #pandoc_convert(md_file,output=html_file, options = c("--wrap=preserve", "--mathjax", "--standalone"))
  
  tab_html = cells_to_tabhtml(bind_rows(tab_df$cell_df))
  rai_write_all_tables_html(tab_html,file.path(ocr_dir, "tabs.html"))
  writeUtf8(tab_html, file.path(ocr_dir, "tabs.html"))
  #rstudioapi::filesPaneNavigate(ocr_dir)
  
  return(invisible(list(ocr=ocr, page_df=page_df, tab_df=tab_df)))
}

repbox_load_mistral_ocr = function(artid) {
  file = file.path("~/ocr/mistral_ocr_art", paste0(artid, ".Rds"))
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}


mistral_html_extract_tables = function(html) {
  restore.point("mistral_html_extract_tables")
  txt = html
  library(repboxArt)
  
  # Find table positions
  pattern <- "(?s)(<table\\b[^>]*>.*?</table>)"
  tab_pos = stringi::stri_locate_all_regex(txt, pattern,
  omit_no_match = TRUE, opts_regex = list(case_insensitive = TRUE)
)[[1]]
  
  pattern <- "(^|[\n]|<p>)[ \t]*((Table)|(TABLE))"
  # Find all matches with their positions
  tit_pos = stringi::stri_locate_all_regex(txt, pattern, omit_no_match = TRUE)[[1]]
  
  # Candidates for table titles and table fragments
  pos_df = bind_rows(
    as.data.frame(tab_pos) %>% mutate(type="tab"),
    as.data.frame(tit_pos) %>% mutate(type="tit")
  ) %>%
    arrange(start) %>%
    filter(type == "tab" | type=="tit" & lead(type)=="tab") %>%
    # mutate(dist_to_next = lead(start)-end) %>%
    # title or panel titles should not be more than 1000 characters
    # filter(dist_to_next <= 1000) %>% 
    mutate(
      merge_above = lag(type)=="tab", 
      title_start = case_when(
        type == "tab" & lag(type) == "tab" ~ lag(end)+1L,
        type == "tab" & lag(type) == "tit" ~ lag(start),
        TRUE ~ NA_integer_
      ),
      tabtitle = stri_sub(txt, title_start, start-1) %>%
        stri_replace_all_fixed("<p>","") %>%
        stri_replace_all_fixed("</p>",""),
      title_dist = start-title_start
    )
  
  tab_df = pos_df %>%
    filter(type=="tab") %>%
    mutate(raw_tabhtml = stri_sub(txt, start, end) %>%
             stri_replace_all_fixed("\u2003", "")
    ) %>% 
    mutate(tabhtml = sapply(raw_tabhtml,html_table_add_cellnum_row_col)) %>%
    mutate(parent_row = cumsum(1L-merge_above)) %>%
    mutate(cell_df = lapply(tabhtml, normalized_html_tab_to_cell_df)) %>%
    group_by(parent_row) %>%
    mutate(
      num_panels = n(),
      panel_pos = 1:n()
    ) %>%
    ungroup()    

  # Merge tables
  merge_df = tab_df %>%
    filter(num_panels > 1)
  
  if (NROW(merge_df)>0) {
    merge_df = merge_df %>%
      group_by(parent_row) %>%
      summarize(
        cell_df = list(cell_df_join(cell_df, tabtitle))
      )
    for (i in 1:NROW(merge_df)) {
      tab_df$cell_df[[merge_df$parent_row[i] ]] = merge_df$cell_df[[i]]
    }
  }
  tab_df = tab_df %>% filter(panel_pos==1) %>%
    ungroup() %>%
    mutate(
      tabname = tabtitle_to_tabname(tabtitle),
      tabid = tabname_to_tabid(tabname)
    )
  for (i in 1:NROW(tab_df$cell_df)) {
    tab_df$cell_df[[i]]$tabid = tab_df$tabid[[i]]
  }
  #tab_df$tabhtml = sapply(tab_df$cell_df, cell_df_to_simple_tabhtml)
  tab_df
}


cell_df_join = function(cell_li, panel_titles = NULL) {
  if (length(cell_li)==1) return(cell_li[[1]])
  restore.point("cell_df_join")
  cell_df = cell_li[[1]]
  
  # Add panel titles as cell
  panel_inds = setdiff(seq_along(cell_li),1)
  if (!is.null(panel_titles)) {
    max_col = max(sapply(cell_li, function(cell_df) max(cell_df$col)))
    for (i in panel_inds) {
      text = panel_titles[i]
      cell_df = bind_rows(
        tibble(cellid="cell-0", row=0, col=1, colspan=max_col, rowspan=1,inner_html=text, text=text),
        cell_li[[i]]
      )
      cell_df$row = cell_df$row +1
      cell_li[[i]] = cell_df
    }   
  }
  cell_df = cell_li[[1]]
  cell_df$panel_num = 1
  max_row = max(cell_df$row)
  for (i in panel_inds) {
    new_cell_df = cell_li[[i]]
    new_cell_df$panel_num = i
    new_cell_df$row = new_cell_df$row + max(cell_df$row)
    cell_df = bind_rows(cell_df, new_cell_df)
  }
  cell_df$cellid = paste0("cell-",1:NROW(cell_df))  
  cell_df
  
}

my_pandoc = function(input, output, args, stdout = FALSE, stderr=FALSE) {
  restore.point("my_pandoc")
  all_args = c(input, "-o", output, args)
  system2("pandoc", args = all_args, stdout = stdout, stderr = stderr)
}

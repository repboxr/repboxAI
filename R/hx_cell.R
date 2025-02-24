example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  hx_all_tab_html_to_cell_list(project_dir, overwrite=TRUE)
  hx_all_cell_list_to_cell_base(project_dir, overwrite=TRUE)
}

hx_all_tab_html_to_cell_list = function(project_dir, overwrite=FALSE) {
  ddp_derive_all_instances(project_dir, from_pid = "tab_html", to_pid = "cell_list",convert_fun = hx_tab_html_to_cell_list, overwrite=overwrite)
}


# cell_list is a derived prod of tab_html
hx_tab_html_to_cell_list = function(hx=NULL, run_dir=hx$run_dir, prods=repbox_prods(), prod_df=hx$prod_df, version=hx$version) {
  restore.point("hx_tab_html_to_cell_list")
  prod = prods[["cell_list"]]
  df = hx_load_prod_df(run_dir=run_dir, prod_df=prod_df)

  hx = ddp_init_hx(hx, run_dir=run_dir,ddp_pid="cell_list", version = version)
  i = 1
  cell_df = bind_rows(lapply(seq_len(NROW(df)), function(i) {
    restore.point("shkfjhksfdo")
    cell_df = normalized_html_tab_to_cell_df(df$tabhtml[[i]]) %>% rename(text=content)
    cell_df = add_col_left(cell_df, tabid=df$tabid[i],otabid = df$otabid[i])
    cell_df$cellid = paste0(df$tabid[i],"_", stri_sub(cell_df$cellid,6))
    cell_df      
  }))
  prod_df = df_to_prod_df(cell_df, prod)
  hx = hx_save(hx,prod_df)
  #rstudioapi::filesPaneNavigate(hx$run_dir)
  invisible(hx)  
}


hx_all_cell_list_to_cell_base = function(project_dir, overwrite=FALSE) {
  restore.point("hx_all_cell_list_to_cell_base")
  ddp_derive_all_instances(project_dir, from_pid = "cell_list", to_pid = "cell_base",convert_fun = hx_cell_list_to_cell_base, overwrite=overwrite)
}


hx_cell_list_to_cell_base = function(hx=NULL, run_dir=hx$run_dir, prods=repbox_prods(), prod_df=hx$prod_df, version=hx$version) {
  restore.point("hx_cell_list_to_cell_base")
  cell_df = hx_load_prod_df(run_dir=run_dir, prod_df=prod_df)
  prod = get_repbox_prod("cell_base", prods)
  hx = ddp_init_hx(hx, run_dir=run_dir,ddp_pid="cell_base", version = version)
  prod_df = cell_list_to_cell_base_prod(cell_df, prod=prod)
  hx = hx_save(hx,prod_df)
  #rstudioapi::filesPaneNavigate(hx$run_dir)
  invisible(hx)  
}

cell_list_to_cell_base_prod = function(cell_list, prod=repbox_prods()[["cell_base"]]) {
  restore.point("cell_df_to_cell_base_prof")
  prod$vars
  
  # We only extract number if it is at the beginning
  # of in a bracket
  fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  fp_start = paste0("^", fp)
  cell_df = cell_df %>% mutate(
    nchar = nchar(text),
    nchar_letters = stri_count_regex(text, "[a-zA-Z]"),
    # A num column should not start with any other text strings
    num_text_temp = text %>%
      stri_replace_all_regex("[\\(\\)\\[\\]\\{\\}, \\*]","") %>%
      stri_replace_all_fixed("−","-"),
    num_str = num_text_temp %>%
      stri_extract_first_regex(fp_start),
    other_num_str_temp = num_text_temp %>%
      stri_extract_last_regex(fp_start),
    other_num_str = ifelse(is.true(num_str == other_num_str_temp), NA_character_, other_num_str_temp),
    num = suppressWarnings(as.numeric(num_str)),
    has_num = !is.na(num),
    has_deci = stri_detect_fixed(num_str, "."),
    num_deci = ifelse(!is.na(num),nchar(str.right.of(num_str,".", not.found=rep("", n()))), NA_integer_)
  )
  
  cell_df = cell_df %>% mutate(
    has_paren = stri_detect_fixed(text,"("),
    has_bracket = stri_detect_fixed(text,"["),
    has_curley = stri_detect_fixed(text,"{"),
    sig_star_str = find_stars_str(text),
    has_sig_star = sig_star_str != "",
    bracket = case_when(
      has_paren ~ "()",
      has_bracket ~ "[]",
      has_curley ~ "{}",
      TRUE ~ ""
    )
  )
  if (length( setdiff(prod$vars, colnames(cell_df)) ) > 0) {
    stop("Not all variables in cell_base generated.")
  }
  df_to_prod_df(cell_df,  prod)
  
}

find_stars_str = function (big_str) {
  restore.point("find_stars_str")
  if (length(big_str) == 0) 
    return(NULL)
  digit_pos = stringi::stri_locate_last_regex(big_str, "[0-9]")[, 
                                                                1]
  digit_pos[is.na(digit_pos)] = 0
  right_str = substring(big_str, digit_pos + 1)
  right_str = gsub(" ", "", right_str)
  as.vector(stringi::stri_match_first_regex(right_str, "[*]+")) %>% 
    na.val("")
}

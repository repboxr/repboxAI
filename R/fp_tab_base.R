# Parse tab_base

example = function() {
  def = def_tab_base(prods)
}



def_tab_base = function(prods, ai_opts = get_ai_opts(), art_source = c("pdf","txt","page_img","page_txt", tpl_num=1)[1]) {
  restore.point("def_tab_base")
  pid = "tab_base"
  prod = prods[["pid"]]
  version = paste0(prod$pid, "|",art_source,"|", ai_short_version(ai_opts))

  tpl_file = file.path(repbox_ai_tpl_dir(), paste0(pid, "-", art_source, "-", tpl_num, ".txt"))
  tpl = merge.lines(readLines(tpl_file))
  vars = find_tpl_placeholders(tpl)

  xglue::xglue.pre(tpl)
  xglue::parse.xglue.blocks(tpl)

  list(
    version = version,
    prod = prod,
    art_source = art_source,
    ai_opts = ai_opts
  )


}

proc_tab_base = function() {

}

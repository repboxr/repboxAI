rai_tpl_dir = function() {
  system.file("prompts", package="repboxAI")
  "~/repbox/gemini/repboxAI/inst/prompts"
}


rai_model_short = function(model) {
  ai_model_short(model)
}

rai_init = function(project_dir,json_mode=first_nn(proc_info$json_mode,FALSE), schema=NULL, values = NULL, context=NULL, media_files=NULL, tpl=NULL, tpl_file = proc_info$tpl_file,   model=ai_opts$model, temperature=ai_opts$temperature, proc_info=NULL, ai_opts=get_ai_opts() ) {
  
  ai_init(project_dir, json_mode=json_mode, schema=schema, values=values, context=context, media_files=media_files, tpl=tpl, tpl_file=tpl_file, model=model, temperature=temperature, ai_opts=ai_opts)
}

# Simple wrapper to ai_context
#
# Allow to easily add typical repbox files like art_pdf
rai_context = function(project_dir, model=ai_opts$model, media_files = NULL, prompt=NULL, ttl_sec=60*5, doc_type_pdf = NULL, add_all_pdf = FALSE, cache_context = isTRUE(ai_opts$cache_context), api_key = getOption("gemini_api_key"), ai_opts = get_ai_opts()) {
  restore.point("rai_context")
  pdf_files = NULL
  if (add_all_pdf) {
    pdf_files = repbox_all_pdf_file(project_dir)
  } else if (!is.null(doc_type_pdf)) {
    pdf_files = repbox_pdf_file(project_dir, doc_type = doc_type_pdf)
  }
  media_files = sort(union(pdf_files, media_files))
  ai_context(project_dir,model = model, media_files=media_files, prompt=prompt, ttl_sec=ttl_sec, cache_context = cache_context, api_key=api_key, ai_opts=ai_opts)
}

rai_run = function(rai,values=rai$values, verbose=FALSE) {
  restore.point("rai_run")
  rai = ai_run(rai, values, verbose)
  rai
}

rai_doc_dir_to_project_dir = function(doc_dir) {
  dirname(dirname(dirname(dirname(doc_dir))))
}

rai_fp_dir_to_project_dir = function(fp_dir) {
  dirname(dirname(fp_dir))
}

rai_fp_dir_to_doc_type = function(fp_dir) {
  str.right.of(basename(fp_dir),"prod_")
}


rai_has_input = function(doc_dir, ...) {
  inputs = list(...)
  restore.point("rai_has_input")
  fp_dir = doc_dir_to_fp_dir(doc_dir)
  doc_form = rdoc_form(doc_dir)
  for (inp in inputs) {
    if (isTRUE(inp=="pdf")) {
      if (doc_form != "pdf") return(FALSE)
    } else if (is.character(inp)) {
      inp = list(prod_id = inp)
    }
    if (is.list(inp)) {
      ok = fp_has_prod(fp_dir, inp$prod_id, inp$proc_id)
      if (!ok) {
        cat("\nRequired input ", inp$prod_id, " not found for ", doc_dir,"\n")
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

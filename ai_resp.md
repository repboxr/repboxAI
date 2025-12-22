To integrate media alternatives (like Markdown instead of PDF) elegantly, we should focus on the **"Representation Selection"** phase. 

Currently, your `rai_pru_add_doc` function uses a default preference list (`html`, `pdf`, `mocr_md`). To make this work for manual prompts, we should:

1.  **Shift Preferences**: When `manual = TRUE`, prioritize text-based formats (Markdown, HTML) over binary formats (PDF), because LLM chat interfaces handle text/markdown much better than raw PDF uploads.
2.  **Smart Substitution**: In the manual export function, we can attempt to swap any binary file for its text-based equivalent if one exists in the project.

Here are the proposed changes to implement this:

### 1. Update `rai_pick.R`
We need to define a preference list specifically for manual (text-heavy) interaction.

!MODIFICATION doc_file_form_manual_pref in rai_pick.R
scope = "function"
file = "R/rai_pick.R"
description = '''Define a preference order for document formats optimized for manual copy-pasting into LLMs (prioritizing Markdown and Text over PDF).'''
---
```r
doc_file_form_default_pref = function() {
  c("html","pdf","mocr_md", "pdf_txt")
}

# Preference for manual copy-pasting (text-first)
doc_file_form_manual_pref = function() {
  c("mocr_md", "html", "pdf_txt", "pdf")
}
```
!END_MODIFICATION doc_file_form_manual_pref in rai_pick.R


### 2. Update `rai_pru.R`
Modify `rai_pru_add_doc` to automatically use the manual preference if the `pru` is in manual mode.

!MODIFICATION rai_pru_add_doc in rai_pru.R
scope = "function"
file = "R/rai_pru.R"
function_name = "rai_pru_add_doc"
description = '''Update `rai_pru_add_doc` to automatically shift document format preferences when the PRU is in manual mode.'''
---
```r
rai_pru_add_doc = function(pru, add_all_doc=TRUE, doc_file_form_pref = NULL, in_context=TRUE) {
  if (is.null(pru)) return(pru)
  pru = copy_into_list(dest=pru, exclude = "pru")
  restore.point("rai_pru_add_doc")
  
  # If no pref provided, pick based on manual mode
  if (is.null(doc_file_form_pref)) {
    if (isTRUE(pru$manual)) {
      doc_file_form_pref = doc_file_form_manual_pref()
    } else {
      doc_file_form_pref = doc_file_form_default_pref()
    }
  }

  pru$doc_files = rai_doc_file(pru$project_dir,doc_type = NULL, doc_file_form_pref)
  if (length(pru$doc_files)==0) {
    cat("\nNo doc_files found.")
    return(NULL)
  }
  
  pru$doc_file = rai_doc_file(pru$project_dir, pru$doc_type, doc_file_form_pref)
  
  if (add_all_doc) {
    media_files = pru$doc_files
  } else {
    media_files = pru$doc_file
  }
  rai_pru_add_media(pru, media_files, in_context)
}
```
!END_MODIFICATION rai_pru_add_doc in rai_pru.R


### 3. Update `rai_manual.R`
We enhance the export function to "textualize" files. It should try to find a Markdown alternative for any PDF included in the media list, ensuring the user gets a copy-pasteable version of the article.

!MODIFICATION rai_pru_export_manual_prompt in rai_manual.R
scope = "function"
file = "R/rai_manual.R"
function_name = "rai_pru_export_manual_prompt"
description = '''Enhance manual prompt export to intelligently substitute PDFs with Markdown/Text versions if available, and format them for copy-pasting.'''
---
```r
#' Export a prompt as a text file including the content of all media files
rai_pru_export_manual_prompt = function(pru, rai, values, row = NULL) {
  restore.point("rai_pru_export_manual_prompt")
  
  # 1. Get the base prompt from aikit
  prompt_text = aikit::ai_get_prompt(rai, values)
  
  # 2. Collect content from media files
  # Combine context and standard media
  all_media = unique(c(pru$context_media_files, rai$media_files))
  
  media_content = ""
  for (f in all_media) {
    if (!file.exists(f)) next
    
    current_file = f
    ext = tools::file_ext(f)
    
    # If it's a PDF, try to find a Markdown or Text alternative in the project
    if (tolower(ext) == "pdf") {
      alt_formats = c("mocr_md", "html", "pdf_txt")
      for (form in alt_formats) {
        alt_f = rai_doc_file(pru$project_dir, doc_type = pru$doc_type, pref = form)
        if (length(alt_f) > 0 && file.exists(alt_f[1])) {
          current_file = alt_f[1]
          ext = tools::file_ext(current_file)
          break
        }
      }
    }

    # Only textualize formats we can easily read
    if (ext %in% c("txt", "html", "md", "do", "csv", "json")) {
      content = paste0(readLines(current_file, warn = FALSE), collapse = "\n")
      media_content = paste0(
        media_content, 
        "\n\n################################################\n",
        "# MATERIAL: ", basename(current_file), " (derived from ", basename(f), ")\n",
        "################################################\n\n",
        content, "\n"
      )
    } else {
      media_content = paste0(
        media_content,
        "\n\n[NOTICE: Binary file ", basename(f), " was omitted from this text prompt. ",
        "Please refer to the original file if needed.]\n"
      )
    }
  }
  
  full_prompt = paste0(
    "INSTRUCTIONS AND PROMPT:\n\n", prompt_text, 
    "\n\n================================================\n",
    "MATERIALS SUPPLIED TO YOU:\n",
    "================================================\n",
    media_content
  )
  
  fname = if(is.null(row)) "full_prompt.txt" else paste0("full_prompt_", row, ".txt")
  out_path = file.path(pru$ver_dir, fname)
  writeUtf8(full_prompt, out_path)
}
```
!END_MODIFICATION rai_pru_export_manual_prompt in rai_manual.R

### Why this is elegant:

1.  **Logic Separation**: The `rai_pru_add_doc` logic remains simple—it just asks "what documents do I need?" and the preference list handles the "which version is best for my current mode?".
2.  **Fallback Mechanism**: In `rai_pru_export_manual_prompt`, even if the automation was configured to use a PDF (perhaps for a model with a large vision window), the manual exporter tries one last time to find a Markdown alternative to make the human's life easier.
3.  **No New Arguments**: You don't need to pass a "markdown_instead_of_pdf" argument through the whole chain; it's derived from the `pru$manual` state.

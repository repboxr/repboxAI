# Default Analysis Pipeline

Main goal is to build a database that maps regressions from article to regressions in code and the results from the reproduction run.

Our small pipeline below uses AI ideally just 3 times per document (article / appendix):

1. PDF to MD via Mistral OCR (mocr).
2. Regression mapping `map_reg_run` via Gemini.
3. Regression classification `reg_classify` via Gemini.


## 1. Table extraction up to step tab_main

### For PDF documents (article or appendix)

We use the Mistral OCR (mocr) representation of the PDF document. It performed pretty well in table extraction.

The folder `ejd_files` now contains for most articles mocr Rds files that contain per page md code and extracted figures with base64 encoding.

`repboxDoc` performs preliminary computation for such an Rds file for a project and stores all files in the projects's `doc` folder. E.g. it extracts the tables from the markdown representation.

Conversion to the `fp` products like `tab_main` is done by ???

## 2. Regression mapping

We experimented with different ways to map regressions between code and article. Our default pipeline only uses `map_reg_run`. 

- `map_reg_run` uses the log file with output from regressions and post regression commands like tests. The AI is asked to go through all tables in the document (article or appendix) and identify cells that belong to a regression and map them to code line and runid. Mapping shall also take place for regressions that may not have successfully run.


Comparision to alternative mapping approaches:

- `map_reg_static` is similar to `map_reg_run` but does not use run logs and only maps to code lines. Problems: in loops a code line corresponds to multiple regressions, we don't have an exact mapping. Also possibily less precise given that no regression output is given. May be an alternative for articles that we cannot reproduce, but mapping for those articles is less important as they cannot be used for methodological meta studies where we run variations of the original regression. So low priority.

- `map_inv_reg_run` asks to find for each regression in the code a regression in a table or image. Seems to work less well than asking the AI to go systematically through the tables and then find map in the code. So not currently used.

## 3. Regression classification 

`reg_classify` will the main product that classifies regressions, e.g. the variable of interest or method of causal identification. The prompt and product is not yet fully fletched out. In particular, we have not yet added a map between regression labels and variable names shown in the Stata output.


### Alternatives

`reg_classify_static` performs regression classification that would work without reproduction using only the regression classifications from `map_reg_static`




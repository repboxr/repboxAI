example = function() {
  # dummy data
  set.seed(42)
  df = data.frame(
    respondent_id = sprintf("ID%03d", 1:100),
    name          = sample(c("Alice", "Bob", NA, "Diane"), 100, TRUE),
    income        = rnorm(100, 50000, 12000),
    birth_date    = as.Date("1980-01-01") + sample(0:15000, 100, TRUE),
    stringsAsFactors = FALSE
  )
  attr(df$income, "label") = "Annual income in USD"
  
  cat(data_set_to_prompt(df, "demo_data"))
  data_set_quick_overview(df, "mydata")
}


#' Compact, prompt‑ready data‑set overview (no parentheses)
#'
#' @param dat         data.frame / tibble
#' @param data_name   Header label
#' @param top_n       How many examples or levels to show (default 3)
#' @param trunc_chars Truncate long strings / levels to this many chars (default 10)
#' @return            Single character string
#'
data_set_to_prompt = function(dat, data_name,
                                top_n = 3, trunc_chars = 10) {

  if (!requireNamespace("stringi", quietly = TRUE))
    stop("Install the 'stringi' package.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Install the 'dplyr' package.")

  library(stringi)
  library(dplyr)
  library(purrr)

  # ------------------------------------------------------------------
  # helpers
  # ------------------------------------------------------------------
  get_type = function(x) {
    cls <- class(x)[1]
    if (cls %in% c("numeric", "double"))  "num"
    else if (cls == "integer")            "int"
    else if (inherits(x, "Date"))         "date"
    else if (inherits(x, "POSIXt"))       "datetime"
    else if (is.factor(x))                "factor"
    else                                  "chr"
  }

  trunc_str = function(s, k = trunc_chars) {
    s <- as.character(s)
    too_long <- nchar(s, type = "chars") > k
    s[too_long] <- paste0(substr(s[too_long], 1, k), "...")
    s
  }

  summarise_var = function(x, v_type) {
    if (v_type %in% c("num", "int")) {
      s <- summary(x)
      sprintf("stats=min=%s med=%s max=%s",
              format(s["Min."], trim = TRUE),
              format(s["Median"], trim = TRUE),
              format(s["Max."], trim = TRUE))
    } else if (v_type == "chr") {
      uvals <- unique(x[!is.na(x)])
      if (length(uvals) == 0) return("")
      samp  <- trunc_str(sample(uvals, min(top_n, length(uvals))))
      paste0("sample=", stri_join(samp, collapse = ", "))
    } else if (v_type == "factor") {
      tbl <- sort(table(x, useNA = "no"), decreasing = TRUE)
      if (length(tbl) == 0) return("")
      top <- head(tbl, top_n)
      names(top) <- trunc_str(names(top))
      paste0("main_levels=", stri_join(names(top), "(", as.integer(top), ")",
                                       collapse = ", "))
    } else if (v_type %in% c("date", "datetime")) {
      if (all(is.na(x))) return("")
      rng <- range(x, na.rm = TRUE)
      paste0("range=", format(rng[1]), " ... ", format(rng[2]))
    } else ""
  }

  # ------------------------------------------------------------------
  # pre‑compute column widths for tidy alignment
  # ------------------------------------------------------------------
  var_names_trunc <- trunc_str(names(dat), 30)            # never wider than 30
  var_width  <- max(nchar(var_names_trunc))               # pad to longest
  type_width <- 8                                         # "datetime" fits
  int_width  <- nchar(as.character(nrow(dat)))            # for non‑NA/unique

  # ------------------------------------------------------------------
  # build variable lines
  # ------------------------------------------------------------------
  lines <- map_chr(
    seq_along(dat),
    function(i) {
      var      <- var_names_trunc[i]
      x        <- dat[[i]]
      v_type   <- get_type(x)
      n_non_na <- sum(!is.na(x))
      n_unique <- length(unique(x[!is.na(x)]))
      miss_pct <- round(mean(is.na(x)) * 100, 1)
      summary  <- summarise_var(x, v_type)

      sprintf(
        "%-*s  type=%-*s  non-NA=%*d  unique=%*d  miss%%=%5.1f  %s",
        var_width,  var,
        type_width, v_type,
        int_width,  n_non_na,
        int_width,  n_unique,
        miss_pct,
        summary
      )
    }
  )

  # ------------------------------------------------------------------
  # variable labels
  # ------------------------------------------------------------------
  label_lines <- map_chr(
    names(dat),
    function(v) {
      lbl <- attr(dat[[v]], "label", exact = TRUE)
      if (is.null(lbl) || lbl == "") "" else sprintf("%s: %s", v, lbl)
    }
  )
  label_lines <- label_lines[label_lines != ""]

  # ------------------------------------------------------------------
  # header + assembly
  # ------------------------------------------------------------------
  header <- sprintf(
    "%s  |  n=%d  vars=%d  size=%.2f MB",
    data_name, nrow(dat), ncol(dat),
    round(as.numeric(object.size(dat)) / 1024^2, 2)
  )
  out <- c(header, strrep("-", nchar(header)), lines)
  if (length(label_lines) > 0)
    out <- c(out, "", "Variable labels:", label_lines)

  stringi::stri_join(out, collapse = "\n")
}


#' Build a privacy‑aware description of a data frame for LLM prompts
#'
#' The output is ready to paste under “Materials supplied to you” in your
#' privacy‑audit prompt.  It now *starts* with a one‑sentence disclaimer:
#'     “Sample values below are masked or truncated …”
#'
#' @param dat        A data.frame or tibble
#' @param data_name  Character; human‑readable name of the data set
#' @param max_sample Max number of example values per variable (default 3)
#' @return           A single character string
#'
#' Dependencies: stringi  (install.packages("stringi"))
#'
#' Build a privacy‑aware description of a data frame for LLM prompts
#'
#' *No global date range is reported.*  
#' Sample values are masked or truncated **only in this summary**
#' to avoid leaking PII; the underlying data remain unchanged.
#'
#' Masking rule  : show first 2 and last 1 characters → "Al***r"  
#' Truncation    : show first 3 characters, then "..." → "Ale..."  
#'
#' @param dat        data.frame or tibble to summarise
#' @param data_name  Character; human‑readable dataset name
#' @param max_sample Integer; examples per variable (default 3)
#' @param max_total  Integer; string length at which to truncate (default 12)
#' @return           Single character string, ready for LLM prompts
#'
#' Requires package stringi
#'
data_set_to_prompt_privacy = function(dat, data_name,
                                    max_sample = 3, max_total = 12) {

  if (!requireNamespace("stringi", quietly = TRUE))
    stop("Please install the 'stringi' package.")
  library(stringi)

  ## ------------------------------------------------------------------------
  ## Helper functions
  ## ------------------------------------------------------------------------

  get_type = function(x) {
    cls = class(x)[1]
    if (cls %in% c("numeric", "integer")) "num"
    else if (inherits(x, "Date"))         "date"
    else if (inherits(x, "POSIXt"))       "datetime"
    else if (is.factor(x))                "factor"
    else                                  "chr"
  }

  # Mask (*** between first 2 & last 1) or truncate (first 3 + ...)
  mask_string = function(s, max_total = 12) {
    s = as.character(s)
    if (is.na(s)) return("NA")
    n = nchar(s, type = "chars")

    if (n <= 3) {
      s  # keep as is (very short)
    } else if (n <= max_total) {
      paste0(substr(s, 1, 2), "***", substr(s, n, n))
    } else {
      paste0(substr(s, 1, 3), "...")
    }
  }

  # Build a single variable line
  build_var_line = function(var_name) {
    x            = dat[[var_name]]
    n_non_na     = sum(!is.na(x))
    n_unique     = length(unique(x[!is.na(x)]))
    na_pct       = if (nrow(dat) == 0) 0 else (1 - n_non_na / nrow(dat)) * 100
    var_type     = get_type(x)

    # Random sample of distinct non‑NA values
    if (n_non_na == 0) {
      sample_vals = "NA"
    } else {
      samp = sample(unique(x[!is.na(x)]), min(max_sample, n_unique))
      samp = switch(
        var_type,
        chr      = vapply(samp, mask_string, character(1), max_total = max_total),
        factor   = vapply(as.character(samp), mask_string, character(1),
                          max_total = max_total),
        num      = format(round(as.numeric(samp), 3), trim = TRUE),
        date     = format(as.Date(samp)),
        datetime = format(as.POSIXct(samp, tz = "UTC")),
        samp
      )
      sample_vals = stri_join(samp, collapse = ", ")
    }

    # Date range inside variable description (if applicable)
    range_str = ""
    if (var_type %in% c("date", "datetime") && n_non_na > 0) {
      rng = range(x, na.rm = TRUE)
      range_str = stri_paste("; range: ",
                             format(rng[1], "%Y-%m-%d"), " ... ",
                             format(rng[2], "%Y-%m-%d"))
    }

    stri_paste(
      var_name, " (", n_unique, "/", n_non_na,
      " unique, ", var_type, ", NA%=", sprintf("%.1f", na_pct), "): ",
      sample_vals, range_str
    )
  }

  ## ------------------------------------------------------------------------
  ## Header
  ## ------------------------------------------------------------------------
  n_obs   = nrow(dat)
  n_vars  = ncol(dat)
  size_mb = round(as.numeric(object.size(dat)) / 1024^2, 2)

  header = stri_paste(
    data_name, " (", n_obs, " obs, ", n_vars,
    " vars, ", size_mb, " MB)"
  )

  disclaimer = paste(
    "NOTE: Sample values below are randomly chosen and may be",
    "masked (*** pattern) or truncated (...) solely for this",
    "summary; the original dataset is unchanged."
  )

  ## ------------------------------------------------------------------------
  ## Variable lines and variable‑label appendix
  ## ------------------------------------------------------------------------
  var_lines = vapply(names(dat), build_var_line, character(1), USE.NAMES = FALSE)

  label_lines = vapply(
    names(dat),
    function(v) {
      lbl = attr(dat[[v]], "label", exact = TRUE)
      if (is.null(lbl) || lbl == "") "" else stri_paste(v, ": ", lbl)
    },
    character(1),
    USE.NAMES = FALSE
  )
  label_lines = label_lines[label_lines != ""]

  ## ------------------------------------------------------------------------
  ## Combine and return
  ## ------------------------------------------------------------------------
  descr_parts = c(header, disclaimer, var_lines)
  if (length(label_lines) > 0)
    descr_parts = c(descr_parts, "", "Variable labels:", label_lines)

  stringi::stri_join(descr_parts, collapse = "\n")
}

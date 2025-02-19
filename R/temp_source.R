# To better develop FuzzyProduction and repboxAI simultaneously
# we just source FuzzyProduction each time we re-load repboxAI

source_dir = function(dir, verbose=TRUE) {
  files = list.files(dir, pattern=glob2rx("*.R"),full.names = TRUE)
  for (f in files) {
    if (verbose) cat("\nsource ", f)
    source(f)
  }
}
source_dir("~/repbox/gemini/FuzzyProduction/R")

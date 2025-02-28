# To better develop FuzzyProduction and repboxAI simultaneously
# we just source FuzzyProduction each time we re-load repboxAI

source_dir = function(dir, verbose=TRUE) {
  files = list.files(dir, pattern=glob2rx("*.R"),full.names = TRUE)
  for (f in files) {
    if (verbose) cat("\nsource ", f)
    source(f)
  }
}
cat("\nSource FuzzyProduction and repboxTableTools")
source_dir("~/repbox/gemini/FuzzyProduction/R")
source_dir("~/repbox/gemini/repboxTableTools/R")
source_dir("~/repbox/gemini/aikit/R")

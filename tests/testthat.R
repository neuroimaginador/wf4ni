suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(wf4ni))

write.table(test_check("wf4ni"), "test_results.csv")

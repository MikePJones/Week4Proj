library(testthat)
library(Week4Proj)

test_that("make_file", expect_equal(make_filename(2013), "accident_2013.csv.bz2"))

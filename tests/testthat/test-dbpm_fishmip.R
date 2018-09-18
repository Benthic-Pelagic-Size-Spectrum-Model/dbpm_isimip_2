context("dbpm fishmip model run code ")

test_that("model date boundaries are as expected",{
  setwd("..")
  source("helpers.R", local = TRUE)
  
  expect_that(start_of_spinup, equals(1))
  expect_that(end_of_spinup, equals(300*12))
  expect_that(start_of_history, equals(300*12 + 1))
  expect_that(end_of_history, equals(300*12 + 55*12))
  expect_that(start_of_projections, equals(300*12 + 55*12 + 1))
  expect_that(end_of_projections, equals(5412))
  
  expect_that(start_of_spinup_weeks, equals(1))
  expect_that(end_of_spinup_weeks, equals(300*48))
  expect_that(start_of_history_weeks, equals(300*48 + 1))
  expect_that(end_of_history_weeks, equals(300*48 + 55*48))
  expect_that(start_of_projections_weeks, equals(300*48 + 55*48 + 1))
  expect_that(end_of_projections_weeks, equals(21645))  
  
})


# test_that("commandline calls work as expected", {
#   setwd("..")
#   expect_equal(blah <- system("Rscript runmodel_calls.R -s 1 -i /rd/gem/private/fishmip_inputs/ -o ~/", ignore.stdout = TRUE, ignore.stderr = TRUE), 1)
# })


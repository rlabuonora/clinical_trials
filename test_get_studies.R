source('./R/get_studies.R')
source('./global.R')

library(testthat)

test_that("get_studies filters studies by map bounds correctly", {
  
  studies <- get_studies()
  expect_equal(length(studies), 2)
  expect_true(studies$count > 500e3)
})

test_that("get_studies filters studies by map bounds correctly", {
  
  studies <- get_studies(condition="lung cancer")
  expect_equal(length(studies), 2)
  expect_true(studies$count > 10e3)
  
})

test_that("get_studies returns correct fields", {
  
  studies_data <- get_studies()$data
  expect_equal(colnames(studies_data), c("title",
                                         "org",
                                         "status",
                                         "phase",
                                         "endDate",
                                         "lon",
                                         "lat"))
})

test_that("get_studies filters by status", {
  
  studies_data <- get_studies(status="COMPLETED")$data
  expect_true(all(studies_data$status=="COMPLETED"))
})


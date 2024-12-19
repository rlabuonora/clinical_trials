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

test_that("geo filter", {
  lat <- 39.0035707
  lon <- -77.1013313
  radius <- 50
  studies <- get_studies(lat=lat, lon=lon,  radius=50)
  expect_true(studies$count > 27e3)
  
})

test_that("geo filter wit large radius", {
  lat <- 39.0035707
  lon <- -77.1013313
  radius <- 5000
  studies <- get_studies(lat=lat, lon=lon,  radius=radius)
  expect_true(studies$count > 300e3)
  
})

test_that("geo filter wit large radius", {
  lat <- 0
  lon <- 0.17578
  radius <- 5628.616
  studies <- get_studies(lat=lat, lon=lon,  radius=radius)
  expect_true(studies$count > 300e3)
  
})




test_that("map bounds to lon lat raduis", {
  # Example usage
  map_bounds <- list(
    north = 39.5,
    south = 39.0,
    east = -76.0,
    west = -77.0
  )
  
  center_and_radius <- get_center_and_radius(map_bounds)
  
  expect_equal(center_and_radius$latitude, 39.25, tolerance = .01)
  expect_equal(center_and_radius$longitude, -76.5, tolerance = .01)
  expect_equal(center_and_radius$radius, 31.80469, tolerance = .01)
  

})


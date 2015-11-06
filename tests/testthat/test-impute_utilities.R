
library(testthat)
library(imputation)

context("weighted mean") 

test_that("weighted mean works", {
  set.seed(359)
  a <- rnorm(10)
  b <- runif(10)
  
  a2 <- rnorm(1000)
  b2 <- runif(1000)
  
  ### first check errors
#   expect_equal(weighted_mean(a, runif(9)), -1)
#   expect_equal(weighted_mean(a, runif(11)), -1)
#   expect_equal(weighted_mean(rnorm(9), b), -1)
#   expect_equal(weighted_mean(rnorm(11), b), -1)
  
  ### next check results
  expect_equal(weighted_mean(a,b), weighted.mean(a,b))
  expect_equal(weighted_mean(a2,b2), weighted.mean(a2,b2))
  
})


context("sort indices")

test_that("sort_indices == order", {
  set.seed(359)
  a <- rnorm(10)
  b <- rnorm(1000)
  
  # expected to equal (order(x) - 1) -- using C++ indices [0, n-1] vs R: [1, n]
  expect_equal(sort_indices(a), order(a) - 1)
  expect_equal(sort_indices(b), order(b) - 1)
  
})


## ECHO is on.

library(matrixcalc)
context("Check local linear regression function")
source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

test_that("llr output has correct length", {
    expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})

test_that("make_weight_matrix returns diagonal matrix", {
    expect_true(is.diagonal.matrix(make_weight_matrix(z, x, omega = 1)))
})

test_that("weight matrix all positive", {
  expect_true(all(make_weight_matrix(x, z, omega = 1) >= 0))
})

test_that("simple case of make_weight_matrix", {
  expect_equal(make_weight_matrix(z = 0, x = c(1, 0.5), omega = 1), matrix(c(0, 0, 0, 343/512), nrow = 2, ncol = 2, byrow = FALSE))
})

test_that("first column of predictor matrix all 1's", {
  expect_true(all(make_predictor_matrix(x)[, 1] == 1))
})

test_that("correct nx2 dimensions of predictor matrix", {
  expect_true(nrow(make_predictor_matrix(x)) == length(x))
  expect_true(ncol(make_predictor_matrix(x)) == 2)
})

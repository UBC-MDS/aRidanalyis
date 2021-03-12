library(testthat)
library(tidyverse)
library(glmnet)

# creating toy data
set.seed(2021)
X <- matrix(rnorm(40 * 3), 40, 3)
xnew <- matrix(rnorm(10*3), 10, 3)
y <- sample(c(0,1), 40, replace = TRUE)
y0 <- rep(1, 40)
y3 <- sample(c(0,1,2), 40, replace = TRUE)
y2 <- sample(c(0,1,"3"), 40, replace = TRUE)

# Test for incorrect regularization inputs.
testthat::test_that('Incorrect regularization input generates error', {
  expect_error(arid_logreg(X, y, regularization = "l1"))
  expect_error(arid_logreg(X, y, regularization = "l2"))
  expect_error(arid_logreg(X, y, regularization = "lasso"))
})

# Test for X input
testthat::test_that('Empty or non-matrix structure as X generates error', {
  expect_error(arid_logreg(data.frame(), y))
  expect_error(arid_logreg(list(1,2,3,4), y))
  expect_error(arid_logreg(TRUE, y))
})

# Test for y response input
testthat::test_that('Empty or non-numeric vectors as y generates error', {
  expect_error(arid_logreg(X, y2))
  expect_error(arid_logreg(X, as.character(y)))
  expect_error(arid_logreg(X, as.logical(y)))
  expect_error(arid_logreg(X, c()))
})

# Test for X and y dimensions
testthat::test_that('Generate error when dimensions of X and y do not match', {
  expect_error(arid_logreg(X, y[1:20], regularization = NULL))
  expect_error(arid_logreg(X[1:20,], y, regularization = NULL))
  expect_error(arid_logreg(X[1:35,], y[1:25], regularization = "L1"))
})

# Test for incorrect model outputs.
testthat::test_that('Output should be of class arid_logreg', {
  expect_identical(class(arid_logreg(X, y)), "arid_logreg")
  expect_identical(mode(arid_logreg(X, y)), "list")
})

# Test for length of model outputs
testthat::test_that('Output should be of the correct length', {
  expect_equal(length((arid_logreg(X, y))$intercept_), 1)
  expect_equal(length((arid_logreg(X, y))$coef_), 3)
  expect_equal(length((arid_logreg(X, y))$score()), 1)
  expect_equal(length((arid_logreg(X, y))$predict(xnew)), 10)
})

# Test for score
testthat::test_that('Test correct results of score function', {
  expect_equal((arid_logreg(X, y))$score(),0.0136, tolerance=5e-3)
})

# Test for intercept_
testthat::test_that('Test correct results of score function', {
  expect_equal((arid_logreg(X, y))$intercept_,0.3714, tolerance=5e-3)
})

# Test for coef_
testthat::test_that('Test correct results of score function', {
  expect_equal(((arid_logreg(X, y))$coef_)[1],0.2438, tolerance=5e-3)
  expect_equal(((arid_logreg(X, y))$coef_)[2],-0.0186, tolerance=5e-3)
  expect_equal(((arid_logreg(X, y))$coef_)[3],0.1248, tolerance=5e-3)
})

# Test for coef_
testthat::test_that('Test correct results of score function', {
  expect_identical(((arid_logreg(X, y))$predict(xnew))[1], 0)
  expect_identical(((arid_logreg(X, y))$predict(xnew))[3], 0)
  expect_identical(((arid_logreg(X, y))$predict(xnew))[5], 1)
  expect_identical(((arid_logreg(X, y))$predict(xnew))[7], 1)
  expect_identical(((arid_logreg(X, y))$predict(xnew))[9], 1)
})
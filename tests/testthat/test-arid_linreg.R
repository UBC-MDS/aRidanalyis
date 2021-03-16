linear_df <- dplyr::tibble(x1 = c(1,0,0),
                           x2 = c(0, 1.0, 0),
                           x3 = c(0,0,1),
                           x4 = c('a','a','b'),
                           y = c(1,3,-1.0))
X <- linear_df %>%
  dplyr::select(-y) %>%
  dplyr::select_if(is.numeric) %>%
  data.matrix()
y <- linear_df %>%
  dplyr::select(y) %>%
  as.matrix()

# Test for arid_linreg() invalid constructor inputs
testthat::test_that('Incorrect linear regression model constructor inputs', {
  expect_error(arid_linreg(5))
  expect_error(arid_linreg("L3"))
  expect_error(arid_linreg(NULL, "lambda"))
  expect_error(arid_linreg(NULL, c(1,2)))
})

# Test for arid_linreg::fit() invalid inputs
testthat::test_that('Incorrect linear regression model fit inputs', {
  expect_error(arid_linreg()$fit(NULL, y))
  expect_error(arid_linreg()$fit(X, NULL))
  expect_error(arid_linreg()$fit(X, as.matrix(c(1),c(0))))
  expect_warning(arid_linreg()$fit(dplyr::select(linear_df, x1:x3), y))
  expect_warning(arid_linreg()$fit(X, dplyr::select(linear_df, y)))
  expect_error(arid_linreg()$fit(X, c('a','b','c')))
  expect_error(arid_linreg()$fit(X, c(0,1)))
})

# Test for arid_linreg::predict() invalid inputs
testthat::test_that('Incorrect linear regression model predict inputs', {
  expect_error(arid_linreg()$fit(X, y)$predict(NULL))
  expect_error(arid_linreg()$predict(X))
  expect_error(arid_linreg()$fit(X, y)$predict(t(as.matrix(X[1:2]))))
})

# Test for arid_linreg::score() invalid inputs
testthat::test_that('Incorrect linear regression model score inputs', {
  expect_error(arid_linreg()$score())
})

# Test range of arid_linreg valid constructor models
testthat::test_that('Range of valid regularization and lambda inputs', {
  expect_identical(class(arid_linreg()$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg()$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L1")$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L1", 0)$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L2")$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L2", 1)$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L1L2")$fit(X, y)), "arid_linreg")
  expect_identical(class(arid_linreg("L1L2", 0.5)$fit(X, y)), "arid_linreg")
})

testthat::test_that('Test correct results of model fitting', {
  expect_true(length(arid_linreg()$fit(as.matrix(dplyr::select(linear_df, x1:x2)), y)$coef_) == 2)
  expect_true(length(arid_linreg("L1L2")$fit(X, y)$coef_) == 3)
  expect_equal(arid_linreg()$fit(X, y)$intercept_, 1, tolerance = 5e-3)
  expect_true(all(arid_linreg('L1')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
  expect_false(all(arid_linreg('L2')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
  expect_false(all(arid_linreg('L1L2')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
})

testthat::test_that('Test correct results of predict function', {
  expect_true(all(round(arid_linreg()$fit(X, y)$predict(X),1) == matrix(c(1,3,-1), nrow = 3, ncol = 1)))
  expect_true(all(arid_linreg('L1')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
  expect_false(all(arid_linreg('L2')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
  expect_false(all(arid_linreg('L1L2')$fit(X, y)$predict(X) == matrix(1, nrow = 3, ncol = 1)))
})

testthat::test_that('Test correct results of score function', {
  expect_equal(arid_linreg()$fit(X, y)$score(), 1, tolerance = 5e-3)
  expect_equal(arid_linreg('L2')$fit(matrix(1:12, nrow = 3, ncol = 4), y)$score(), 0.173, tolerance = 1e-2)
})

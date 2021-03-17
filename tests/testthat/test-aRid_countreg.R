
# Test for incorrect model type inputs.
testthat::test_that('Incorrect model type specification should generate error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(X,y, model="aditive"))
  expect_error(arid_countreg(X,y, model="interact", verbose=1))
  expect_error(arid_countreg(X,y, model= 0 , verbose=0))
  expect_error(arid_countreg(X,y, model=c("additive", "interactive")))

})

testthat::test_that('Empty or non-dataframe structures as X generate error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(data.frame(),y, model="additive"))
  expect_error(arid_countreg(list(1,2,3,4),y))
  expect_error(arid_countreg(TRUE,y, model="interactive", verbose=1))
})

testthat::test_that('Empty or non-integer vectors as y  generate error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(X,y +0.2, model="additive"))
  expect_error(arid_countreg(X,as.character(y), model="interactive"))
  expect_error(arid_countreg(X,as.logical(y), verbose=1))
  expect_error(arid_countreg(X,c(), fit_intercept=0 , verbose=0))
})

testthat::test_that('Generate error when dimensions of X and y do not match', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(X,y[1:3], model="additive"))
  expect_error(arid_countreg(X[1:4,],y, model="interactive"))
  expect_error(arid_countreg(X[1:3,],y[1:2], verbose=1))
})

testthat::test_that('Non-logical inputs shoulg generate an error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(X,y, verbose = "fail" ))
  expect_error(arid_countreg(X,y, fit_intercept="incorrect"))
  expect_error(arid_countreg(X,y, verbose=NULL))
  expect_error(arid_countreg(X,y, fit_intercept=11.33))
})

testthat::test_that('Incorrect values for significance generate error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_error(arid_countreg(X,y,alpha= 0L ))
  expect_error(arid_countreg(X,y, alpha= 21.33))
  expect_error(arid_countreg(X,y,alpha= FALSE))
  expect_error(arid_countreg(X,y,alpha=-0,05))
})

# Test for incorrect model outputs.
testthat::test_that('Output should be of class arid_countreg', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_identical(class(arid_countreg(X,y, model="additive")), "arid_countreg")
  expect_identical(mode(arid_countreg(X,y, model="additive")), "list")
})
testthat::test_that('Output should be of the correct length', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_equal(length(arid_countreg(X,y, model="additive")$p_values_), 2)
  expect_equal(length(arid_countreg(X,y, model="interactive")$coef_), 3)
  expect_identical(arid_countreg(X,y, model = "additive",
                                 fit_intercept = FALSE)$intercept_,
                                 NULL)

})
testthat::test_that('Incorrect model types and classes should give error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_identical(class(arid_countreg(X,y, model="additive")$count_model_)[1], "glm")
  expect_identical(mode(arid_countreg(X,y, model="interactive")$count_model_),"list")
})

testthat::test_that('Incorrect integration of significance throws error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_equal(arid_countreg(X,y, model="additive", alpha=0.1)$alpha_, 0.1)
  expect_equal(arid_countreg(X,y, model="additive")$alpha_, 0.05)

})

testthat::test_that('Incorrect integration of family throws error', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_identical(arid_countreg(X,y, model="additive", alpha=0.1)$family_,
                   "poisson")


})

testthat::test_that('Incorrect integration of model type throws error ', {
  X <- tibble::tribble(
    ~badh,    ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  expect_identical(arid_countreg(X,y, model="additive", alpha=0.1,
                                 verbose=TRUE)$type_,
                   "additive")


})

testthat::test_that('Test correct results of predict function', {
  X <- tibble::tribble(
    ~badh,     ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L)
  new_X <- tibble::tibble(badh = c("bad"), age=c(59))
  a <- arid_countreg(X,y, model = "additive")
  expect_equal(a$predict_count(a$count_model_, new_X)[[1]],22.19, tolerance=5e-3)


})

testthat::test_that('Test correct results of score function', {
  X <- tibble::tribble(
    ~badh,     ~age,
    "good",      58,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
    "bad",       57,
    "good",      33,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
    "good",      33,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
    "bad",       57,
    "good",      33,
    "good",      44,
    "bad",       54,
    "bad",       57,
    "good",      33,
  )
  y <- c(30L,16L,20L,20L,15L, 30L,16L,20L,
         50L,15L, 30L,16L,14L,60L,15L,
         20L,25L, 19L,56L,20L,30L,15L,
         30L,16L,20L,19L)
  b <- arid_countreg(X,y, model = "additive", verbose=TRUE)
  expect_equal(nrow(b$score(b$count_model_)),2)
}
)


#' Given a matrix X of explanatory variables, a response y numeric variable,
#' this function fits either a 'binomial' or 'multinomial' logistic regression model
#' and returns a class object similar to sci-kit learn's object
#'
#'@param X the explanatory variables matrix
#'@param y the response variable numeric vector
#'@param regularization what level of regularization to use in the model (optional)
#'@param lambda the regularization strength parameter to use (optional)
#'
#'@returns a class object after fitting a 'binomial' or 'multinomial' logistic regression model
#'
#'@examples
#'model <- arid_logreg(X, y, regularization="L2")
#'model$coef_
#'model$intercept_
#'model$score()
#'model$predict(newx)
arid_logreg <- function(X, y, regularization=NULL, lambda=NULL){
    
    # initializing environment
    thisEnv <- environment()
    assign("regularization_", regularization, thisEnv)
    assign("lambda_", lambda, thisEnv)
    
    # testing some inputs
    if (class(y) != 'numeric'){
        stop("response must be numeric (i.e. class(y)=='numeric')")
    }
    if (!is.matrix(X) | is_empty(X)){
        stop("The input X must be a non-empty matrix")
    }
    if (length(regularization) > 0 ){
        if (!(regularization %in% c("L1", "L2", "L1L2", NULL))) {
            stop("The regularization parameter should be either 'L1', 'L2', 'L1L2' or NULL")
        }
    }
    if (length(unique(y)) < 2){
        stop('response must contain at least two unique values')
    }
    
    # assigning the family based on response input (y)
    if (length(unique(y)) == 2){
        family <- "binomial"
    }
    else {
        family <- "multinomial"
    }
    
  # function to get the coefficients  
  get_coefs <- function(X, y, model, lambda) {
    coef_ <- NULL
    if (is.null(lambda)) {
      lambda <- glmnet::cv.glmnet(X, y)$lambda.min
      coefs <- glmnet::coef.glmnet(model, s = lambda)
    }
    else {
      coefs <- glmnet::coef.glmnet(model, s = lambda)
    }
    assign("lambda_", lambda, thisEnv)
    return(coefs)
  }
  
  # function to assign intercept and coefficients
  set_coefs <- function(coefs, lambda) {
    assign("intercept_", coefs[1], thisEnv)
    assign("coef_", coefs[c(-1)], thisEnv)
  }
  
  #fit function depending on regularization, lamda and family
  fit <- function(X, y) {
    model <- NULL
    regularization <- regularization_
    lambda <- lambda_
    if(is.null(regularization)) {
      lambda <- 0
      model <- glmnet::glmnet(X, y, family = family, alpha = 1, lambda = lambda)
    }
    else if(regularization == c("L1")) {
      model <- glmnet::glmnet(X, y, alpha = 1, family = family)
    }
    else if(regularization == c("L2")) {
      model <- glmnet::glmnet(X, y, alpha = 0, family = family)
    }
    else if(regularization == c("L1L2")) {
      model <- glmnet::glmnet(X, y, alpha = 0.5, family = family)
    }
  
    coefs <- get_coefs(X, y, model, lambda)
    set_coefs(coefs) 
        
    return(model)
  }
  
  # predict function for binomial
  predict <- function(newx) {
      if(family == 'binomial'){
         prob <- glmnet::predict.glmnet(model_, s = lambda_, newx = newx, type="response")
         pred <- ifelse(prob > 0.5,1,0)
         return(pred) 
      }
      else {
         return("Only predictions for binomial logistic regression are available")
      }
  }

  # score function
  score <- function() {
    model_$dev.ratio
  }
  
  # calling fit function and assigning model to env
  model <- fit(X, y)
  assign("model_", model, thisEnv)
 
  # setting up the model class attributes
  arid_logreg <- list(
    thisEnv = thisEnv,
    fit = fit,
    predict = predict,
    score = score,
    intercept_ = intercept_,
    coef_ = coef_
  )
  
  # returning the class model  
  class(arid_logreg) <- "arid_logreg"
  return(arid_logreg)
}
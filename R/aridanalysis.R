#' Function to create summary statistics and basic EDA plots. Given a data frame,
#' this function outputs general exploratory analysis plots as well as basic
#' statistics summarizing trends in the features of the input data.
#'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param response_type string indiating if response is 'categorical' or 'continuous'
#'@param features a list of explanatory variable column names
#'
#'@returns a dataframe with a list of features and their coefficients
#'@retusns a ggplot object containing the EDA
#'
#'@examples
#'arid_eda(house_prices, 'price', 'continuous, c('rooms', 'age','garage'))
arid_eda <- function(df, response, response_type, features=c())


#' Function that performs a linear regression on continuous response data.
#' This function will fit a linear regression model on the input dataframe
#' using the response supplied and provide skLearn interface functionality
#'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param features a list of explanatory variable column names
#'@param regularization what level of regularization to use in the model
#'@param alpha the regularization weight parameter to use
#'
#'@returns a linear regression model wrapped in an sklearn style class
#'
#'@examples
#'arid_linreg(df, income)
arid_linreg <- function(df, response, features=c(), regularization=NULL, alpha = c(1))

#' Given a matrix X of explanatory variables, a response y numeric variable,
#' this function fits a logistic regression model, either "binomial" or "multinomial"
#'
#'@param X the explanatory variables matrix
#'@param y the response variable numeric vector
#'@param regularization what level of regularization to use in the model (optional)
#'@param lambda the regularization strength parameter to use (optional)
#'
#'@returns either a "binomial" or "multinomial" logistic regression model wrapped in an sklearn style class
#'
#'@examples
#'arid_logreg(X, y, regularization="L2")
arid_logreg <- function(X, y, regularization=NULL, lambda=NULL){
    
    thisEnv <- environment()
    assign("regularization_", regularization, thisEnv)
    assign("lambda_", lambda, thisEnv)
    
    if (class(y) != 'numeric'){
        stop("response must be numeric (i.e. class(y)=='numeric')")
    }
    
    if (length(unique(y)) < 2){
        stop('response must contain at least two unique values')
    }
    
    if (regularization %in% c("L1", "L2", "L1L2", "NULL") == FALSE){
        stop("regularization parameter must be 'L1', 'L2', 'L1L2', or 'NULL'")
    }
    
    if (length(unique(y)) == 2){
        family <- "binomial"
    }
    else {
        family <- "multinomial"
    }
    
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
    
  set_coefs <- function(coefs, lambda) {
    assign("intercept_", coefs[1], thisEnv)
    assign("coef_", coefs[c(-1)], thisEnv)
  }
    
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
    
  predict <- function(newx) {
    return(glmnet::predict.glmnet(model_, s = lambda_, newx = newx)[1])
  }
    
  score <- function() {
    model_$dev.ratio
  }
  
  model <- fit(X, y)
  assign("model_", model, thisEnv)
 
  arid_logreg <- list(
    thisEnv = thisEnv,
    fit = fit,
    predict = predict,
    score = score,
    intercept_ = intercept_,
    coef_ = coef_
  )
    
  class(arid_logreg) <- "arid_logreg"
  return(arid_logreg)
}

#' A function that performs linear regression on counting data when the response is
#' restricted to be positive and natural. This function will perform count regression
#' to the specified columns of a data frame and return a substantial inferential analysis.
#'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param features a list of explanatory variable column names
#'@param model A string indicating the model type. Either "additive" or "interactive"
#'@param alpha significance level for analysis
#'
#'@returns a wrapped statistical linear model with new functionalities.
#'@returns a string family was used in the generalized linear regression model based on an overdispersion and fitting analysis
#'
#'@examples
#'aridanalysis.arid_countreg(df, income, features = [feat1, feat5] ,"additive")
arid_countreg <- function(data_frame, response, features=c(), model="additive", polynomial=False, alpha=0.05)

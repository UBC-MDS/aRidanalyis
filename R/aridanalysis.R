#' Function to create summary statistics and basic EDA plots. Given a data frame,
#' this function outputs general exploratory analysis plots as well as basic
#' statistics summarizing trends in the features of the input data.
#'
#'@param df (data frame): the input data frame to analyze
#'@param response (character): the column name of the response variable
#'@param response_type (character): string indicating if response is 'categorical' or 'continuous' (default: 'numeric')
#'@param features (list<string>): a list of explanatory variable column names (default: c())
#'
#'@returns data frame: a data frame with a list of features and their coefficients
#'@returns plot: a ggplot object containing the EDA
#'
#'@export
#'
#'@examples
#'arid_eda(house_prices, 'price', 'continuous, c('rooms', 'age','garage'))
arid_eda <- function(df, response, response_type = 'numeric', features = c()){

  if (all(features %in% colnames(df)) == FALSE){
    stop('one or more features are not present in data frame')
  }
  if (response %in% features){
    stop('response must not be explicit from feature list')
  }
  if (response %in% colnames(df) == FALSE){
    stop('response is not contained in data frame')
  }
  if (response_type == 'numeric'){
    if (dplyr::select(df, response) %>% dplyr::pull() %>% is.numeric() == FALSE){
      stop('Response is not numeric')
    }
  } else if (response_type == 'categorical'){
    if (dplyr::select(df, response) %>% dplyr::pull() %>% is.numeric() == TRUE){
      stop('Response is not categorical')
    }
  }

  if (length(features) == 0){
    filtered_df <- df %>% tidyr::drop_na()
    cols <- dplyr::select(df, where(is.integer), where(is.double)) %>% colnames()

  } else {
    filtered_df <- df %>% dplyr::select(one_of(features), all_of(response), where(is.numeric)) %>% tidyr::drop_na()
    cols <- features
  }

  myplots <- list()  # new empty list

  if (response_type == 'numeric'){
    for (i in 1:length(cols)) {
      p1 <- ggplot2::ggplot(data=filtered_df, ggplot2::aes_string(x=cols[i])) +
        ggplot2::geom_histogram(fill="lightgreen", stat='count', binwidth = 20) +
        ggplot2::xlab(colnames(cols)[i]) +
        ggplot2::ggtitle(cols[i])
      myplots[[i]] <- p1
      myplots[[i]] <- p1
    }

  } else if(response_type == 'categorical'){
    for (i in 1:length(cols)) {
      p1 <- ggplot2::ggplot(filtered_df, ggplot2::aes_string(x = cols[i], fill = response)) +
        ggplot2::geom_density(alpha = 0.6)
      myplots[[i]] <- p1  # add each plot into plot list
    }
  }

  corr_df <- filtered_df %>%  dplyr::select(where(is.numeric))

  myplots[[length(cols)+1]] <- GGally::ggcorr(corr_df, label=TRUE) + ggplot2::ggtitle('Correlation Matrix')
  myplots
}


#' Function that builds an arid_linreg class model object that provides sklearn
#' linear regression interface functionality and attributes. The arid_linreg
#' function instantiates a linear regression model type based on the input
#' specifications and provides methods to fit/predict/score the results and
#' retrieve the calculated sklearn coefficients.
#'
#'
#'@param regularization (character): A string defining NULL,'L1','L2', or 'L1L2'
#'       linear coefficient regularization (default: NULL)
#'@param lambda (double):the numeric regularization strength value
#'
#'@returns arid_linreg: an arid_linreg class linear regression model object
#'
#'@export
#'
#'@examples
#'model <- arid_linreg()
#'model$fit(matrix(1:12, nrow = 3, ncol = 3), as.matrix(c(1,2,0)))
#'model$predict(t(as.matrix(c(1,2,2))))
#'model$score()
arid_linreg <- function(regularization=NULL, lambda=NULL) {

  # Validate initialization inputs
  if (!is.null(regularization)) {
    if (!is.null(regularization) & !is.character(regularization)) {
      stop('ERROR: regularization input must be a character vector')
    }
    if (!is.null(regularization) & !(regularization %in% c(NULL,
                                                           "L1",
                                                           "L2",
                                                           "L1L2"))) {
      stop('ERROR: Invalid regularization input value')
    }
  }

  if (!is.null(lambda)) {
    if (!is.numeric(lambda)) {
      stop('ERROR: lambda input must be a numeric vector')
    }
    if (length(lambda) > 1) {
      stop('ERROR: lambda input must single value')
    }
  }

  # Create an environment to allow class-wide variables
  thisEnv <- environment()
  assign("regularization_", regularization, thisEnv)
  assign("lambda_", lambda, thisEnv)
  assign("model_", NULL, thisEnv)
  assign("intercept_", NULL, thisEnv)
  assign("coef_", NULL, thisEnv)

  # Private method to get the coefficients from the fit model
  .get_coefs <- function(X, y, model, lambda) {
    coef_ <- NULL
    # If lambda is not specified, return lowest error lambda
    if (is.null(lambda)) {
      lambda <- glmnet::cv.glmnet(X, y, grouped=FALSE)$lambda.min
      coefs <- glmnet::coef.glmnet(model, s = lambda)
    }
    else {
      coefs <- glmnet::coef.glmnet(model, s = lambda)
    }

    # Store the lowest error lambda in instance
    assign("lambda_", lambda, thisEnv)
    return(coefs)
  }

  # arid_linreg::fit method to fit input sample regression model
  fit <- function(X, y) {
    # Validate fit inputs
    if (is.null(X) | length(X) < 1) {
      stop('ERROR: Invalid input X feature values to fit')
    }
    if (is.null(y) | length(y) < 1) {
      stop('ERROR: Invalid input y response value to fit')
    }
    if (is.list(X)) {
      if (length(dplyr::select_if(X, is.numeric) != length(X))) {
        warning('WARNING: Dropping non-numeric input features in X')
        X <- X %>%
          dplyr::select_if(is.numeric)
      }
      X <- data.matrix(X)
    }
    if (is.list(y)) {
      warning('WARNING: Input y to fit is a list, converting to matrix')
      y <- as.matrix(y)
    }
    if (!is.numeric(y)) {
      stop('ERROR: Response y is not numeric')
    }
    if (nrow(X) != nrow(y)) {
      print('WHAT IS going on here?')
      stop('ERROR: Input samples X and responses y not the same length')
    }

    # Fit the model family according to specifications
    model <- NULL
    if(is.null(regularization_)) {
      lambda_ <- 0
      model <- glmnet::glmnet(X,
                              y,
                              family = 'gaussian',
                              alpha = 1,
                              lambda = lambda_)
    }
    else if(regularization == c("L1")) {
      model <- glmnet::glmnet(X,
                              y,
                              alpha = 1,
                              family = 'gaussian',
                              lambda = lambda_)
    }
    else if(regularization == c("L2")) {
      model <- glmnet::glmnet(X,
                              y,
                              alpha = 0,
                              family = 'gaussian',
                              lambda = lambda_)
    }
    else if(regularization == c("L1L2")) {
      model <- glmnet::glmnet(X,
                              y,
                              alpha = 0.5,
                              family = 'gaussian',
                              lambda = lambda_)
    }

    # Get the coefficients from the fit model
    coefs <- .get_coefs(X, y, model, lambda_)

    # Store class object
    arid_linreg$intercept_ <- coefs[1]
    arid_linreg$coef_ <- coefs[c(-1)]
    assign("coef_", coefs[c(-1)], thisEnv)
    assign("model_", model, thisEnv)

    # Return updated arid_linreg model
    return(arid_linreg)
  }

  # arid_linreg::predict method on new samples
  predict <- function(newx) {
    if (is.null(newx) | length(newx) < 1) {
      stop('ERROR: Invalid input new X sample values to predict')
    }
    if (is.null(model_)) {
      stop('ERROR: Must fit model before predicting')
    }
    if (length(coef_) != ncol(newx)) {
      stop('ERROR: Incorrect number of features in newx samples')
    }

    # Return the predicted values of the input samples
    return(glmnet::predict.glmnet(model_, s = lambda_, newx = newx))
  }

  # arid_linreg::score method to return rsquared score of training samples
  score <- function() {
    if (is.null(model_)) {
      stop('ERROR: Must fit model before scoring')
    }
    max(model_$dev.ratio)
  }

  # Define the elements of the arid_linreg model class
  arid_linreg <- list(
    thisEnv = thisEnv,
    fit = fit,
    predict = predict,
    score = score,
    intercept_ = intercept_,
    coef_ = coef_
  )

  # Return the arid_linreg model class type
  class(arid_linreg) <- "arid_linreg"
  return(arid_linreg)
}


#' Given a matrix X of explanatory variables, a response y numeric variable,
#' this function fits either a 'binomial' or 'multinomial' logistic regression model
#' and returns a class object similar to sci-kit learn's object
#'
#'@param X (data frame): the explanatory variables matrix
#'@param y (numeric): the response variable numeric vector
#'@param regularization (character): what level of regularization to use in the model (default NULL)
#'@param lambda (double): the regularization strength parameter to use (default: NULL)
#'
#'@returns arid_logreg: a class object after fitting a 'binomial' or 'multinomial' logistic regression model
#'
#'@export
#'@examples
#'X <- matrix(rnorm(40 * 3), 40, 3)
#'y <- sample(c(0,1), 40, replace = TRUE)
#'arid_logreg(X, y)
arid_logreg <- function(X, y, regularization=NULL, lambda=NULL){

  # initializing environment
  thisEnv <- environment()

  # setting environment function
  set_properties <- function(regularization, lambda) {
    assign("regularization_", regularization, thisEnv)
    assign("lambda_", lambda, thisEnv)
  }

  set_properties(regularization, lambda)

  # testing some inputs
  if (class(y) != 'numeric'){
    stop("response must be numeric (i.e. class(y)=='numeric')")
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


#' Function to create class object similar to sci-kit learn's object
#' structure for inferential purposes. Given a data frame, the response,
#' and certain specifications return a class object with a fit, predict, and
#' score functions as well as attributes obtained from the statistical analysis
#'
#'@param X (data_frame): the input data frame with the explanatory variables to fit the model.
#'@param y (integer): an integer vector with the response to be fitted (only natural numbers).
#'@param alpha (double): a double vector of length 1 indicating the significance level (default: 0.05)
#'@param fit_intercept (logical): if the model should include the intercept (TRUE or FALSE). (default: FALSE)
#'@param verbose (logical): if results should include a written explanation (TRUE or FALSE). (default: FALSE)
#'@param model (character): type of model to be fitted, either "additive" or "interactive". (default: "additive")
#'@param family (character): distributional family to be used in generalized linear model. (default: "poisson")
#'
#'@returns a class object with three methods and statistical attributes
#'@export
#'@examples
#'X <- as.data.frame(matrix(rnorm(40 * 3), 40, 3))
#'y <- sample(c(1:60), 40, replace = TRUE)
#'arid_countreg(X,y,0.1)
arid_countreg <- function(X, y, alpha=0.05, fit_intercept=TRUE, verbose=FALSE, model="additive", family="poisson")
{


  Env <- environment()

  set_properties <- function(alpha, fit_intercept, verbose, model) {
    assign("fit_intercept_",fit_intercept, Env)
    assign("verbose_",verbose, Env)
    assign("alpha_",alpha, Env)
    assign("type_",model, Env)
    assign("family_", "poisson", Env)
  }

  set_properties(alpha, fit_intercept, verbose, model)

  if (!is.data.frame(X) | rlang::is_empty(X)) {
    stop("The input X must be a non-empty data frame")
  }
  if (!is.integer(y) | length(y) == 0) {
    stop("The response y must be an non-empty vector with whole numbers")
  }
  if (!(model %in% c("additive", "interactive"))) {
    stop("The model specification should be either additive or interactive")
  }
  if (length(model) != 1) {
    stop("The model specification should be a character vector of length 1")
  }
  if (!(verbose %in% c(TRUE, FALSE))) {
    stop("verbose should be either TRUE or FALSE")
  }
  if (!(fit_intercept %in% c(TRUE, FALSE))) {
    stop("fit_intercept should be either TRUE or FALSE")
  }
  if (nrow(X) != length(y)) {
    stop("Dimensions of X and y should match")
  }
  if (alpha>1 | alpha<0 |!as.double(alpha) | length(alpha) != 1) {
    stop("Significance should be a single value between 0 and 1")
  }


  set_attributes <- function(count_model) {
    if (fit_intercept == TRUE) {
      assign("intercept_",exp(broom::tidy(count_model)$estimate[1]), Env)
      assign("coef_", exp(broom::tidy(count_model)$estimate[-1]), Env)
      assign("p_values_",broom::tidy(count_model)$p.value[-1], Env)

    } else {
      assign("intercept_",NULL, Env)
      assign("coef_", exp(broom::tidy(count_model)$estimate), Env)
      assign("p_values_",broom::tidy(count_model)$p.value, Env)
    }
  }


  fit <- function(X, y) {

    model_df <- X
    model_df$response <- y
    if (model == "additive") {
      columns <- stringr::str_c(colnames(X), collapse = " + ")
      formula <- stringr::str_c(c("response",columns), collapse = " ~ ")

    } else {
      columns <- stringr::str_c(colnames(X), collapse = " * ")
      formula <- stringr::str_c(c("response",columns), collapse = " ~ ")

    }
    if (fit_intercept == FALSE) {
      formula <- formula <- stringr::str_c(c(formula, "1"), collapse = " - ")
    }
    count_model <- glm(formula, data=model_df, family = family)

    if (family == "poisson") {
      if (AER::dispersiontest(count_model)[[2]] < alpha) {
        assign("family_", "negative binomial", Env)
        count_model <- MASS::glm.nb(formula, data=model_df)
        if (verbose == TRUE) {
          print(
            "The Poisson model has overdispersion and it is underestimating the variance of the model, hence the negative binomial model will be used"
          )
          print(" ")
        }
      }
    }

    if(fit_intercept ==TRUE) {
      initial_value=2
    } else {
      initial_value=1
    }

    if (verbose == TRUE) {
      for (i in seq(initial_value, nrow(tidy(count_model)))) {
        if (broom::tidy(count_model)$p.value[i] < alpha){
          print(" ")
          print(paste("The variable", tidy(count_model)$term[i], "has a statistically significant association over the response"))
        }

      }
    }

    # Set new attributes:
    set_attributes(count_model)
    return(count_model)
  }

  count_model <- fit(X, y)
  assign("count_model_", count_model, Env)

  predict_count <- function(model,new_X){
    return(exp(predict(model, new_X)))
  }

  score <- function(model) {
    return(tibble(In_Sample_Metric = c("AIC", "Deviance"),
                  Value = c(model$aic, model$deviance)))
  }
  arid_countreg <- list(
    Env = Env,
    fit = fit,
    predict_count = predict_count,
    score = score,
    intercept_ = intercept_,
    coef_ = coef_,
    p_values_ = p_values_,
    count_model_ = count_model_,
    alpha_ = alpha_,
    fit_intercept_ = fit_intercept_,
    type_ = type_,
    family_ = family_

  )

  class(arid_countreg) <- "arid_countreg"
  return(arid_countreg)

}

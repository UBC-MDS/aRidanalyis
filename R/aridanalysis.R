#' Function to create summary statistics and basic EDA plots. Given a data frame,
#' this function outputs general exploratory analysis plots as well as basic
#' statistics summarizing trends in the features of the input data.
#' #'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param response_type string indiating if response is 'categorical' or 'continuous'
#'@param features a list of explanatory variable column names
#'
#'@returns a dataframe with a list of features and their coefficients
#'@returns a ggplot object containing the EDA
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
            p1 <- eval(substitute(
                ggplot2::ggplot(data=filtered_df, ggplot2::aes_string(x=cols[i])) +
                  ggplot2::geom_histogram(fill="lightgreen", stat='count') +
                  ggplot2::xlab(colnames(cols)[i]) +
                  ggplot2::ggtitle(cols[i])
            ,list(i = i)))
            myplots[[i]] <- p1
        }

    } else if(response_type == 'categorical'){
        for (i in 1:length(cols)) {
            p1 <- ggplot2::ggplot(filtered_df, ggplot2::aes_string(x = cols[i], fill = response)) +
                ggplot2::geom_density(alpha = 0.6)
            myplots[[i]] <- p1  # add each plot into plot list
        }
    }

    myplots[[length(cols)+1]] <- GGally::ggcorr(filtered_df, label=TRUE) + ggplot2::ggtitle('Correlation Matrix')
    myplots
}

#' Function that builds an aRid_linreg class model object that provides sklearn
#' linear regression interface functionality and attributes. The aRid_linreg
#' function instantiates a linear regression model type based on the input
#' specifications and provides methods to fit/predict/score the results and
#' retrieve sklearn attributes.

#'
#'@param regularization A string defining NULL,'L1','L2', or 'L1L2'
#'       linear coefficient regularization
#'@param lambda the numeric regularization strength value
#'
#'@returns an aRid_linreg class linear regression model object
#'
#'@export
#'
#'@examples
#'aRid_linreg()
aRid_linreg <- function(regularization=NULL, lambda=NULL) {

  # Validate initialization inputs
  if (!is.null(regularization)) {
    if (!is.null(regularization) & !is.character(regularization)) {
      stop('ERROR: regularization input must be a character vector')
    }
    if (!is.null(regularization) & !(regularization %in% c(NULL, "L1", "L2", "L1L2"))) {
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

  # aRid_linreg::fit method to fit input sample regression model
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
      print(length(X))
      stop('ERROR: Input features X and response y not the same length')
    }

    # Fit the model family according to specifications
    model <- NULL
    if(is.null(regularization_)) {
      lambda_ <- 0
      model <- glmnet::glmnet(X, y, family = 'gaussian', alpha = 1, lambda = lambda_)
    }
    else if(regularization == c("L1")) {
      model <- glmnet::glmnet(X, y, alpha = 1, family = 'gaussian', lambda = lambda_)
    }
    else if(regularization == c("L2")) {
      model <- glmnet::glmnet(X, y, alpha = 0, family = 'gaussian', lambda = lambda_)
    }
    else if(regularization == c("L1L2")) {
      model <- glmnet::glmnet(X, y, alpha = 0.5, family = 'gaussian', lambda = lambda_)
    }

    # Get the coefficients from the fit model
    coefs <- .get_coefs(X, y, model, lambda_)

    # Store class object
    aRid_linreg$intercept_ <- coefs[1]
    aRid_linreg$coef_ <- coefs[c(-1)]
    assign("coef_", coefs[c(-1)], thisEnv)
    assign("model_", model, thisEnv)

    # Return updated arid_linreg model
    return(aRid_linreg)
  }

  # aRid_linreg::predict method on new samples
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

  # aRid_linreg::score method to return rsquared score of training samples
  score <- function() {
    if (is.null(model_)) {
      stop('ERROR: Must fit model before scoring')
    }
    max(model_$dev.ratio)
  }

  # Define the elements of the aRid_linreg model class
  aRid_linreg <- list(
    thisEnv = thisEnv,
    fit = fit,
    predict = predict,
    score = score,
    intercept_ = intercept_,
    coef_ = coef_
  )

  # Return the aRid_linreg model class type
  class(aRid_linreg) <- "aRid_linreg"
  return(aRid_linreg)
}

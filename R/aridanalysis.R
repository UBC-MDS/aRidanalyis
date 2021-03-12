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

#' Given a data frame, a response variable and explanatory variables (features),
#' this function fits a logistic regression
#'
#'@param df the input dataframe to analyze
#'@param response the column name of the response variable
#'@param features a list of explanatory variable column names
#'@param type a string indicating classification type. Either "binomial" or "multinomial"
#'
#'@returns a logistic regression model wrapped in an sklearn style class
#'
#'@examples
#'arid_logreg(df, 'target', ['feat1', 'feat2', 'feat3'], type="multinomial")
arid_logreg <- function(data_frame, response, features=c(), type="binomial")





#' Function to create class object similar to sci-kit learn's object
#' structure for inferential purposes. Given a data frame, the response,
#' and certain specifications return a class object with a fit, predict and
#' score functions as well as attributes obtained from the statistical analysis.
#'
#'@param X the input data frame with the explanatory variables to fit the model.
#'@param y an integer vector with the response to be fitted (only natural numbers).
#'@param alpha  a double vector of length 1 indicating the significance level for the
#'       hypothesis testing.
#'@param fit_intercept if the model should include the intercept (TRUE or FALSE).
#'@param verbose if results should include a written explanation (TRUE or FALSE).
#'@param model type of model to be fitted, either "additive" or "interactive".
#'@param family distributional family to be used in generalized linear model.
#'
#'
#'@returns a class object with three methods and statistical attributes
#'
#'
#'@examples
#'result <- aRid_countreg(house_spec_df, number_rooms, alpha=0.1, model="additive", verbose=TRUE)
#'result$score(result$count_model)
#'result$predict_count(result$count_model, new_specs)
#'result$p_values_
#'result$coef_
#'result$intercept_
#'result$type_
aRid_countreg <- function(X, y, alpha=0.05, fit_intercept=TRUE, verbose=FALSE, model="additive", family="poisson")
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

  if (!is.data.frame(X) | is_empty(X)) {
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
      columns <- str_c(colnames(X), collapse = " + ")
      formula <- str_c(c("response",columns), collapse = " ~ ")

    } else {
      columns <- str_c(colnames(X), collapse = " * ")
      formula <- str_c(c("response",columns), collapse = " ~ ")

    }
    if (fit_intercept == FALSE) {
      formula <- formula <- str_c(c(formula, "1"), collapse = " - ")
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
  aRid_countreg <- list(
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

  class(aRid_countreg) <- "aRid_countreg"
  return(aRid_countreg)

}

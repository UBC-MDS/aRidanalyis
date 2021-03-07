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
#'@reutrns a ggplot object containing the EDA
#'
#'@examples
#'arid_eda(house_prices, 'price', 'continuous, c('rooms', 'age','garage')
arid_eda <- function(data_frame, response, response_type, features=c())


#' Function that performs a linear regression on continuous response data.
#' This function will fit a linear regression model on the input dataframe
#' using the response supplied and all or optionally specified features given.
#'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param features a list of explanatory variable column names
#'@param estimator the function used to fit the linear regression model
#'@param regularization what level of regularization to use in the model
#'
#'@returns a dataframe with a list of features and their coefficients
#'
#'@examples
#'arid_linreg(df, income)
arid_linreg <- function(data_frame, response, features=c(), estimator=None, regularization=NULL)

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


#' A function that performs linear regression on counting data when the response is
#' restricted to be positive and natural. This function will perform count regression
#' to the specified columns    of a data frame and return a substantial inferential analysis.
#'
#'@param data_frame the input dataframe to analyze
#'@param response the column name of the response variable
#'@param features a list of explanatory variable column names
#'@param type a string indicating classification type. Either "binomial", "ordinal" or "multinomial"
#'@param model A string indicating the model type. Either "additive" or "interactive"
#'@param alpha significance level for analysis
#'
#'@returns dataframe with 4 columns: 'features', 'p-value', 'significant', 'interpretation'
#'@returns a string family was used in the generalized linear regression model based on an overdispersion and fitting analysis
#'
#'@examples
#'aridanalysis.arid_countreg(df, income, features = [feat1, feat5] ,"additive")
arid_countreg <- function(data_frame, response, features=c(), model="additive", polynomial=False, alpha=0.05)

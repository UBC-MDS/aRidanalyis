library(testthat)
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
library(GGally)
library(grid)
source("http://peterhaschke.com/Code/multiplot.R")

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
        if (select(df, response) %>% pull() %>% is.numeric() == FALSE){
            stop('Response is not numeric')
        }
    } else if (response_type == 'categorical'){
        if (select(df, response) %>% pull() %>% is.numeric() == TRUE){
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
                ggplot2::ggplot(data=filtered_df, aes_string(x=cols[i])) + 
                  ggplot2::geom_histogram(fill="lightgreen", stat='count') +
                  ggplot2::xlab(colnames(cols)[i]) + 
                  ggplot2::ggtitle(cols[i])
            ,list(i = i)))
            myplots[[i]] <- p1  
        }
        
    } else if(response_type == 'categorical'){
        for (i in 1:length(cols)) {
            p1 <- ggplot2::ggplot(filtered_df, aes_string(x = cols[i], fill = response)) + 
                ggplot2::geom_density(alpha = 0.6)
            myplots[[i]] <- p1  # add each plot into plot list
        }
    }
    
    #multiplot(plotlist = myplots,  cols = 1)
    myplots[[length(cols)+1]] <- GGally::ggcorr(filtered_df, label=TRUE) + ggtitle('Correlation Matrix') 
    myplots
}



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

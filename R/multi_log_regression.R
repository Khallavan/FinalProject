#' @name multi_log_regression
#' @title Multinominal Logistic Regression
#' @param df : data frame
#' Training a Multinominal Logistic Regression model
#' @return model.multinom
#' @example
#' multi_log_regression(data_frame)
multi_log_regression <- function (df) {
    # todo
    if(!require(tidyverse))
        install.packages("tidyverse")
    library(tidyverse)

    if(!require(caret))
        install.packages("caret")
    library(caret)

    if(!require(nnet))
        install.packages("nnet")
    library(nnet)
    library(dplyr)
    library(MASS)
    df.idx <- createDataPartition(df$env, p = 0.8, list = FALSE)
    df.train <- df[df.idx,]
    df.test <- df[-df.idx,]
    control.multinom <- trainControl(method = 'cv', number = 10)
    tune.grid <- expand.grid(decay = seq(0, 1, by = 0.1))
    model.multinom <- multinom (
        env ~ temp_C + press + freqBlue + freqRed,
        data = df.train,
        dacay = 0.1,
        iter = 500
    )
    return(model.multinom)
}

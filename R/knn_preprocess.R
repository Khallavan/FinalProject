#' @name knn_preprocess
#' @title Knn Model
#' @param df : data frame
#' Training a knn model
#' @return model.knn
#' @example
#' knn_preprocess(data_frame)
knn_preprocess <- function(df) {
    # todo
    if(!require(tidyverse))
        install.packages("tidyverse")
    library(tidyverse)

    if(!require(caret))
        install.packages("caret")
    library(caret)
    df.idx <- createDataPartition(df$env, p = 0.8, list = FALSE)
    df.train <- df[df.idx,]
    df.test <- df[-df.idx,]

    df.idx <- createDataPartition(df$env, p = 0.8, list = FALSE)
    df.train <- df[df.idx,]
    df.test <- df[-df.idx,]

    ctrl <- trainControl(method='cv', number=5)

    model.knn <- train(
        enviroment ~ .,
        data=df.knn.train,
        method='knn',
        preProcess=c('center', 'scale'),
        trControl = ctrl
        )

    preditct(model.knn, newdata=df.knn.test)
    return(model.knn)
}

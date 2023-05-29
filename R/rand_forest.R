#' @name rand_forest
#' @title Random Forest Model
#' @param df : data frame
#' Training a random Forest model
#' @return model.rf
#' @example
#' rand_forest(data_frame)
rand_forest <- function(df) {
    if(!require(caret))
        install.packages("caret")
    library(caret)
    library(randomForest)
    df$env = factor(df$env)
    rf <-df[complete.cases(df),]
    df.idx.rf<-createDataPartition(df$env,p=0.7,list = F)

    model.rf <- randomForest(
        x = df[df.idx.rf, -which(names(df) == "env")],
        y = df[df.idx.rf, "env"],
        ntree = 500,
        keep.forest = TRUE
    )
    return(model.rf)
}

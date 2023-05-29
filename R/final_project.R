#' @name final_project
#' @title final project
#' this function generates the models from knn,
#' MLR and random forest models, and saves the models
#' on rds files in models folder.

final_project<- function() {
    df <- load_datasets('dataset.csv')
    df$env <- as.factor(df$env)
    df$select._color <- as.factor(df$select._color)
    df$temp_C <- as.numeric(as.character(df$temp_C))
    df$press <- as.numeric(as.character(df$press))
    library(caret)
    dummies <- dummyVars(~ select._color, data = df, na.action=na.pass)
    dummy_data <- predict(dummies, newdata = df)
    dummy_data[is.na(dummy_data)] <- 0
    df <- cbind(df, dummy_data)
    df$select._color <- NULL
    df.idx <- createDataPartition(df$env, p = 0.8, list = FALSE)
    df.train <- df[df.idx,]
    df.test <- df[-df.idx,]

    # Training Models
    model.knn <- knn_preprocess(df)
    model.multinom <- multi_log_regression(df)
    model.rf <- rand_forest(df)

    # Predict Models.
    df.newdata <- load_datasets('newdata.csv')
    predictor.knn <- predict(knn_model, df.newdata)
    predictor.knn

    predictor.mlg <- predict(mlg_model, df.newdata)
    predictor.mlg

    predictor.rf <- predict(rforest_model, df.newdata)
    predictor.rf
    # Save Models
    saveRDS(model.knn, paste0(parentFolder,"/models/knnModel.rds"))
    saveRDS(model.multinom, paste0(parentFolder,"/models/multinom.rds"))
    saveRDS(model.rf, paste0(parentFolder,"/models/rf_model.rds"))
}

load_libraries <- function() {
    library(ggplot2)
    library(tidyverse)
    library(stargazer)
    library(lubridate)
    library(patchwork)
    library(zoo)
    library(modelsummary)
    library(leaps)
    library(lmtest)
    library(fixest)
    library(fixest)
    library(modelsummary)
}

train_test <- function(df){
    # code from cw7
    set.seed(99) #set the random number generator seed.

    n = floor(0.8 * nrow(df)) #find the number corresponding to 80% of the data
    index = sample(seq_len(nrow(df)), size = n) #randomly sample indicies to be included in the training set

    train = df[index, ] #set the training set to be the randomly sampled rows of the dataframe
    test = df[-index, ] #set the testing set to be the remaining rows

    return(list(train = train, test = test))
}

eval <- function(specification, test, model){
    yhat = predict(model, newdata = test, type = "response")
    mspe = mean((test$ridership - yhat)^2) 
    aic  = AIC(model)
    bic  = BIC(model)

    data.frame(name = specification,
               mspe = mspe, 
               aic  = aic,
               bic  = bic)
}
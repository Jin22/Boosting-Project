# UW ID: 20673533
# Name: Han, Jihoon
# email: 

# fill in the above ID and name, but leave other existing comments untouched
# Only libraries and BoostingModel function allowed in this file
# No setwd, load, or read.csv commands allowed here
# add library here using the library function
# all libraries need to be used in your code in the library::function format
### Your libraries start here
library(caret)
library(dplyr)
library(vtreat)
library(xgboost)
### Your libraries end here

# dtrain: data.frame for the training set
# dtest: data.frame for the test set
# should return a data.frame for prediction
BoostingModel <- function(dtrain, dtest){
# Write all your code below, including preprocessing and functions.
# Only the final model should be fitted, no model selection steps allowed
# All plotting, diagnositic and model building/selection steps should be in your Rmd file.
### Your code starts here
    # preprocessing
    preprocessing <- function(dat){
        plan_of_treat <- vtreat::designTreatmentsZ(
            dframe = dat, 
            varlist = colnames(dat),
            codeRestriction = c("clean", "isBAD", "lev"),
            verbose = FALSE)
        
        score_frame <- plan_of_treat$scoreFrame %>% 
            select(varName, origName, code)
        
        dat_treated <- vtreat::prepare(plan_of_treat, dat)
        return(dat_treated)
        
    }
    #col_names <- names(select(dtrain,-price))
    dtrain <- preprocessing(dtrain)
    dtrain$price <- log(dtrain$price)
    dtest <- preprocessing(dtest)
    
    # print(summary(dtrain))
    # print(dim(dtrain))
    # print(summary(dtest))
    # print(dim(dtest))
    
    # some parameters for the caret::train
    train_control <- caret::trainControl(
        method = "none",
        verboseIter = FALSE, # no training log
        allowParallel = TRUE # FALSE for reproducible results 
    )
    
    (tuned_grid <- expand.grid(
        nrounds = 10000,
        eta = 0.005,
        max_depth = 6,
        gamma = 0,
        colsample_bytree = 0.4,
        min_child_weight = 1,
        subsample = 0.75
    ))
    
    #col_names <- names(select(dtrain,-price))
    #print(col_names)
    
    # The column names for dtrain and dtest using vtreat
    # does not match, thus, use interset to use the common
    # column names
    train_col_names <- names(dtrain)
    test_col_names <- names(dtest)
    col_names <- intersect(train_col_names, test_col_names)
    # print(train_col_names)
    # print(length(train_col_names))
    # print(test_col_names)
    # print(length(test_col_names))
    # print(col_names)
    # print(length(col_names))
    x_input <- as.matrix(select(dtrain, col_names))
    model <- caret::train(
        x = x_input,
        y = dtrain$price,
        trControl = train_control,
        tuneGrid = tuned_grid,
        method = "xgbTree",
        verbose = TRUE
    )
    pred <- predict(model, newdata=dtest)
    pred <- exp(pred)
    res <- data.frame(Id=dtest$Id, price=pred)
    return(res)
	
### Your code ends here
} # end BoostingModel

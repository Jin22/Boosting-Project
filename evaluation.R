# first set working directory to the data location, for example, setwd("/Users/DonaldDuck/Stat444/"), then read in the data by
# remove all objects in R environment to have a clean start
rm(list=ls())
load("project.Rdata")
load("final.Rdata")
load("solution.Rdata")
# project.Rdata contains dtrain and dtest

# load user-defined SmoothingModel function, use UW_ID 20654321 as an example
source("20673533.R")
# call the function
res <- BoostingModel(dtrain, dtest)

# The grading team will compare the result to the solution to compute RMLSE
# for example, assume sol is the data.frame containing the true price in the same order as in res
load("final.Rdata")
sqrt(mean((log(res$price)-log(dat$price[4856:8090]))^2))
# for obvious reasons, the solution provided here is not the real one and is only for the illustration purpose

# you can save the result for submission to Kaggle, for example
write.csv(res, file="mysolution.csv", row.names=FALSE)

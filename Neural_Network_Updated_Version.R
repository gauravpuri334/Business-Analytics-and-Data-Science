if (!require('nnet')) install.packages("nnet");
library("nnet")
if (!require('pROC')) install.packages("pROC");
library("pROC") 
if (!require('caret')) install.packages("caret");
library("caret")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
setwd("/Users/sarjodas/Downloads/BADS")
source("helperFunctions.R")
known <- data.frame(get.data.dataset())
########################################################################################################
known$return_customer = factor(known$return_customer)
known$account_creation_date= factor(known$account_creation_date)
known$form_of_address= as.character(known$form_of_address)
known$email_domain= as.character(known$email_domain)
known$payment= as.character(known$payment)
known$advertising_code= as.character(known$advertising_code)
known$postcode_delivery= as.character(known$postcode_delivery)
known$account_creation_date = as.Date(known$account_creation_date, format = "%Y/%m/%d")
known$order_date = as.Date(known$order_date, format = "%Y/%m/%d")
known$deliverydate_actual = as.Date(known$deliverydate_actual, format = "%Y/%m/%d")
known$deliverydate_estimated =  as.Date(known$deliverydate_estimated, format = "%Y/%m/%d")
######################################################################################################

set.seed(7)
training_index <- createDataPartition(known$return_customer, p=0.60, list=FALSE)
training <- known[training_index,]
validation <- known[-training_index,]
feature.names=names(training)
feature.names2=names(validation)

for (f in feature.names) {
  if (class(training[[f]])=="factor") {
    levels <- unique(c(training[[f]]))
    training[[f]] <- factor(training[[f]],
                            labels=make.names(levels))
  }
}

for (f in feature.names2) {
  if (class(validation[[f]])=="factor") {
    levels <- unique(c(validation[[f]]))
    validation[[f]] <- factor(validation[[f]],
                              labels=make.names(levels))
  }
}

model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the ouput training object
)
# 
# # Define a search grid of values to test 
nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,15,2))

#Train neural network nn with 5-fold cv
nn <- train(return_customer~account_creation_date + remitted_items + deliverydate_actual + deliverydate_estimated + order_date + schoolbook_count + coupon + goods_value + item_count + weight + cost_shipping, data = training,
            method = "nnet", maxit = 450, trace = FALSE, # options for nnet function
            tuneGrid = nn.parms, # parameters to be tested
            metric = "ROC", 
            trControl = model.control)
# Analyze the cross-validation results
print(nn)
plot(nn)
summary(nn)
if(!require("devtools")) install.packages("devtools"); library("devtools")
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
# 
if(!require("reshape")) install.packages("reshape"); library("reshape")
yhat.nn   <- predict(nn, newdata = validation, type = "prob")[,2]
# # Obtain ROC curve and calculate AUC 
nn.roc <-roc(validation$return_customer, yhat.nn)
auc(nn.roc)
plot.roc(nn.roc)

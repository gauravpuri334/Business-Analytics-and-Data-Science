#get.loan.dataset <-function() {
  # Assume the name of the data set is Loan_Data.csv
  #data <- read.csv("Loan_Data.csv", sep = ";")
  # Convert categorical variables into factors
  #data$PHON <- factor(data$PHON, labels=c("no", "yes"))
  #data$BAD <- factor(data$BAD, labels=c("GOOD", "BAD"))
  # replace missing values
  #data$YOB[data$YOB == 99] <- NA
  #data$YOB_missing <- factor(ifelse(is.na(data$YOB), 1, 0))
  #data$YOB[is.na(data$YOB)] <- as.numeric(names(table(data$YOB))[table(data$YOB) == max(table(data$YOB))])
  #return(data)
#}

if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package
install.packages("caret")
library(Hmisc)
library(pROC)
library(caret)
nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

source("helperfunctions.R", local = TRUE)
whole.data<-get.data.dataset()
idx.train <- createDataPartition(y = whole.data$return_customer, p = 0.8, list = FALSE)
train <- whole.data[idx.train, ]
test <- whole.data[-idx.train, ]

train$return_customer = factor(train$return_customer)
train$account_creation_date= factor(train$account_creation_date)
train$form_of_address= as.character(train$form_of_address)
train$email_domain= as.character(train$email_domain)
train$payment= as.character(train$payment)
train$advertising_code= as.character(train$advertising_code)
train$postcode_delivery= as.character(train$postcode_delivery)

test$return_customer = factor(test$return_customer)
test$account_creation_date= factor(test$account_creation_date)
test$form_of_address= as.character(test$form_of_address)
test$email_domain= as.character(test$email_domain)
test$payment= as.character(test$payment)
test$advertising_code= as.character(test$advertising_code)
test$postcode_delivery= as.character(test$postcode_delivery)

train$order_date = as.Date(train$order_date, format = "%Y/%m/%d")
train$deliverydate_actual = as.Date(train$deliverydate_actual, format = "%Y/%m/%d")
train$deliverydate_estimated = as.Date(train$deliverydate_estimated, format = "%Y/%m/%d")


feature.names=names(train)
for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]],
                            labels=make.names(levels))
  }
}
feature.names2=names(test)
for (f in feature.names2) {
  if (class(test[[f]])=="factor") {
    levels <- unique(c(test[[f]]))
    test[[f]] <- factor(test[[f]],
                         labels=make.names(levels))
  }
}


library(nnet)
library(pROC)
library(caret)
library(randomForest)
library(xgboost)
model.control <- trainControl(
  method = "cv", number = 10, classProbs = TRUE,
  summaryFunction = twoClassSummary, allowParallel = TRUE,
  returnData = TRUE
)


xgb.prams <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2,4),
                         eta = c(0.01, 0.05, 0.1, 0.15),
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)



xgb <- train(return_customer~ account_creation_date, data = train, method = "xgbTree",
             tuneGrid = xgb.prams,
             metric = "ROC", trControl = model.control)

xgb.pred <- predict(xgb, newdata = test, type = "class") [,2]
auc(test$return_customer, xgb.pred)

View(xgb.pred)
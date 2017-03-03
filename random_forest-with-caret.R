#library
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc") # load the package
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # load the package
if(!require("e1071")) install.packages("e1071"); library("e1071") # load the package
if(!require("randomForest")) install.packages("randomForest"); library("randomForest") # load the package
if(!require("caret")) install.packages("caret"); library("caret") # load the package
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package

if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # load the package

#parllellism
nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


#load cleaned known data
source("helperfunctions.R", local = TRUE)

whole.data <- data.frame(get.data.dataset())

whole.data$return_customer = factor(whole.data$return_customer)

whole.data$account_creation_date= as.Date(whole.data$account_creation_date, format= "%Y/%m/%d")
whole.data$deliverydate_actual = as.Date(whole.data$deliverydate_actual, format= "%Y/%m/%d")
whole.data$deliverydate_estimated =as.Date(whole.data$deliverydate_estimated, format= "%Y/%m/%d")
whole.data$order_date =as.Date(whole.data$order_date, format= "%Y/%m/%d")

whole.data$form_of_address= as.character(whole.data$form_of_address)
whole.data$email_domain= as.character(whole.data$email_domain)
whole.data$payment= as.character(whole.data$payment)
whole.data$advertising_code= as.character(whole.data$advertising_code)
whole.data$postcode_delivery= as.character(whole.data$postcode_delivery)

feature.names=names(whole.data)
for (f in feature.names) {
  if (class(whole.data[[f]])=="factor") {
    levels <- unique(c(whole.data[[f]]))
    whole.data[[f]] <- factor(whole.data[[f]],
                              labels=make.names(levels))
    
  }
}

idx.train <- createDataPartition(y = whole.data$return_customer, p = 0.6, list = FALSE)
train <- whole.data[idx.train, ]
test <-  whole.data[-idx.train, ]

# train$return_customer = factor(train$return_customer)---------------------------------------
# 
# train$account_creation_date= as.Date(train$account_creation_date, format= "%Y/%m/%d")
# train$deliverydate_actual = as.Date(train$deliverydate_actual, format= "%Y/%m/%d")
# train$deliverydate_estimated =as.Date(train$deliverydate_estimated, format= "%Y/%m/%d")
# train$order_date =as.Date(train$order_date, format= "%Y/%m/%d")
# 
# train$form_of_address= as.character(train$form_of_address)
# train$email_domain= as.character(train$email_domain)
# train$payment= as.character(train$payment)
# train$advertising_code= as.character(train$advertising_code)
# train$postcode_delivery= as.character(train$postcode_delivery)
# 
# test$return_customer = factor(test$return_customer)
# 
# test$account_creation_date= as.Date(test$account_creation_date, format= "%Y/%m/%d")
# test$deliverydate_actual = as.Date(test$deliverydate_actual, format= "%Y/%m/%d")
# test$deliverydate_estimated =as.Date(test$deliverydate_estimated, format= "%Y/%m/%d")
# test$order_date =as.Date(test$order_date, format= "%Y/%m/%d")
# 
# test$form_of_address= as.character(test$form_of_address)
# test$email_domain= as.character(test$email_domain)
# test$payment= as.character(test$payment)
# test$advertising_code= as.character(test$advertising_code)
# test$postcode_delivery= as.character(test$postcode_delivery)-----

# feature.names=names(train)---------------------------------------------------
# for (f in feature.names) {
#   if (class(train[[f]])=="factor") {
#     levels <- unique(c(train[[f]]))
#     train[[f]] <- factor(train[[f]],
#                          labels=make.names(levels))
# 
#   }
# }
# 
# 
# feature.names2=names(test)
# for (f in feature.names2) {
#   if (class(test[[f]])=="factor") {
#     levels <- unique(c(test[[f]]))
#     test[[f]] <- factor(test[[f]],
#                         labels=make.names(levels))
#   }
# }----------------------------------------------


#train the model
set.seed(123)
model.control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE, # Return class probabilities
  summaryFunction = twoClassSummary, # twoClassSummary returns AUC
  allowParallel = TRUE # Enable parallelization if available
)



rf.parms <- expand.grid(mtry = 1:10)
rf.caret <- train(return_customer ~ deliverydate_actual + deliverydate_estimated + order_date +account_creation_date + 
                  schoolbook_count + coupon + goods_value + item_count + weight + cost_shipping + remitted_items , data = train,  
                  method = "rf", ntree = 500, tuneGrid = rf.parms, 
                  metric = "ROC", trControl = model.control)


train$predicted.response <- predict(rf.caret, train)
auc(train$return_customer, as.numeric(train$predicted.response))

#cross validate the model
test$predicted.response <- predict(rf.caret, newdata = test, type = "prob")[,2]

#compare the AUC curve
auc(train$return_customer, as.numeric(train$predicted.response))
auc(test$return_customer, as.numeric(test$predicted.response))



#load dataset
source("helperfunctions.R", local = TRUE)
class.data <- data.frame(get.read.dataset())
class.data$account_creation_date = as.numeric(class.data$account_creation_date)
class.data$predicted.response <- predict(rf.caret, class.data)



stopCluster(cl)

#library
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc") # load the package
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # load the package
if(!require("e1071")) install.packages("e1071"); library("e1071") # load the package
if(!require("randomForest")) install.packages("randomForest"); library("randomForest") # load the package
if(!require("caret")) install.packages("caret"); library("caret") # load the package
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package
if(!require("xgboost")) install.packages("xgboost"); library("xgboost") # load the package
if(!require("caretEnsemble")) install.packages("caretEnsemble"); library("caretEnsemble") # load the package




#parllellism
nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


#load cleaned known data
source("helperfunctions.R", local = TRUE)
whole.data <- data.frame(get.data.dataset())

whole.data$account_creation_date = as.Date(whole.data$account_creation_date, format = "%Y/%m/%d")
whole.data$deliverydate_actual = as.Date(whole.data$deliverydate_actual, format = "%Y/%m/%d")
whole.data$deliverydate_estimated = as.Date(whole.data$deliverydate_estimated, format = "%Y/%m/%d")
whole.data$order_date = as.Date(whole.data$order_date, format = "%Y/%m/%d")
whole.data$return_customer = factor(whole.data$return_customer)

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

idx.train <- createDataPartition(y = whole.data$return_customer, p = 0.8, list = FALSE)
train <- whole.data[idx.train, ]
test <- whole.data[-idx.train, ]



ctrl  <- trainControl(method = "cv", number = 10, 
                      classProbs = TRUE,  savePredictions = "final", 
                      summaryFunction = twoClassSummary, allowParallel = TRUE, returnData = TRUE)

nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,30,2))
xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2,4),
                         eta = c(0.01, 0.05, 0.1, 0.15),
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)
rf.parms <- expand.grid(mtry = 1:10)


modelList <- list(caretModelSpec(method = "nnet", trace = "FALSE", tuneGrid = nn.parms, metric = "ROC"),
                  caretModelSpec(method = "rf", ntree = 800, tuneGrid = rf.parms, metric = "ROC"),
                  caretModelSpec(method = "xgbTree", tuneGrid = xgb.parms, metric = "ROC"))


models <- caretList(return_customer ~ account_creation_date  + deliverydate_estimated + 
                    deliverydate_actual + cost_shipping + weight + coupon + schoolbook_count 
                    + goods_value + order_date + remitted_items + item_count,
                    data = train, trControl = ctrl, tuneList = modelList, 
                    continue_on_fail = FALSE)



ens.stack <- caretStack(models, method='glm')
ens.stack$ens_model$finalModel

ens.stack.pred <- predict(ens.stack, newdata = test, type = "prob")
ens.predictions <- data.frame(STACKING = ens.stack.pred)


r.stack <- roc(test$return_customer, ens.stack.pred)
plot(r.stack)

View(modelList)
View(models)

stopCluster(cl)

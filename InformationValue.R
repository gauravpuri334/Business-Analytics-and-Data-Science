install.packages("InformationValue")
library(InformationValue)
library(mlbench)
library(caret)
source("helperfunctions.R", local = TRUE)
whole.data <- data.frame(get.data.dataset())
summary(whole.data$form_of_address)
idx.train <- createDataPartition(y = whole.data$return_customer, p = 0.6, list = FALSE)
train <- whole.data[idx.train, ]
test <-  whole.data[-idx.train, ]

train$return_customer = factor(train$return_customer)
factor_vars<- c("order_date", "form_of_address", "email_domain", "account_creation_date", "payment", "postcode_delivery", "advertising_code", "deliverydate_estimated", "deliverydate_actual") 
fac <- train[,names(train) %in% factor_vars]
train <- train[,!names(train) %in% categorical_features]
sapply(train, class)

all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), 
                     STRENGTH=character(length(factor_vars)), stringsAsFactors = F)
for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=train[, factor_var], Y=train$return_customer)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=train[, factor_var], Y=train$return_customer), "howgood")
}
all_iv <- all_iv[order(-all_iv$IV), ]

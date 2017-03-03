library(Hmisc)
get.data.dataset <-function() {
  known<- read.csv("assignment_BADS_WS1617_known.csv", header = TRUE, sep = ",")
  
  #for cleaning account creation date on the basis of order date  
  known$account_creation_date <- as.character(known$account_creation_date) #change class from factor to charcter
  known$order_date <- as.character(known$order_date) #same as above
  known$account_creation_date[is.na(known$account_creation_date)] <-  (known$order_date[is.na(known$account_creation_date)]) #replacing NA's with order date values
  known$account_creation_date <- as.factor(known$account_creation_date) #convert back to factor
  known$order_date <- as.factor(known$order_date) #same as above
  
  
  #for cleaning delivery date actual on the basis of order date
  known$order_date <- as.Date(known$order_date) #change class from factor to Date
  known$deliverydate_actual <- as.Date(known$deliverydate_actual) #same as above
  known$deliverydate_actual <- with(known, as.Date(ifelse(is.na(deliverydate_actual), order_date + days(4), deliverydate_actual), origin = "1970-01-01"))
  known$date_diff<-difftime(known$deliverydate_actual, known$order_date, units = "days")
  known$date_diff <- as.numeric(known$date_diff) #change class to numeric
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile <- as.numeric(summary(whole.data$date_diff)[2])
  upper.quartile <- as.numeric(summary(whole.data$date_diff)[5])
  IQR <- upper.quartile - lower.quartile
  # Calculate upper bound value
  upper.bound <- ceiling(upper.quartile + 1.5*IQR)
  known$date_diff[known$date_diff>upper.bound] <-NA # numeric values more than 15 are replaced with NA
  known$deliverydate_actual <- with(known, as.Date(ifelse(is.na(date_diff), order_date + days(upper.bound), deliverydate_actual), origin = "1970-01-01")) #replace outliers with order date + 2
  
  #for cleaning delivery date estimated on the basis of order date
  known$deliverydate_estimated <- as.Date(known$deliverydate_estimated) #same as above
  known$clean_date_diff<-difftime(known$deliverydate_estimated, classdata$order_date, units = "days")#to remove outliers from delivery date estimated
  known$clean_date_diff <- as.numeric(known$clean_date_diff) #change class to numeric
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile.est <- as.numeric(summary(known$clean_date_diff)[2])
  upper.quartile.est <- as.numeric(summary(known$clean_date_diff)[5])
  IQR.est <- upper.quartile.est - lower.quartile.est
  # Calculate upper bound value
  upper.bound.est <- ceiling(upper.quartile + 1.5*IQR.est)
  known$clean_date_diff[known$clean_date_diff>upper.bound.est] <-NA
  known$clean_date_diff[known$clean_date_diff<0] <-NA
  known$deliverydate_estimated <- with(known, as.Date(ifelse(is.na(clean_date_diff), order_date + days(upper.bound.est), deliverydate_estimated), origin = "1970-01-01"))
  
  
  
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile.weight <- as.numeric(summary(known$weight)[2])
  upper.quartile.weight <- as.numeric(summary(known$weight)[5])
  IQR.weight <- upper.quartile.weight - lower.quartile.weight
  # Calculate upper bound value
  upper.bound.weight <- ceiling(upper.quartile + 1.5*IQR.weight)
  known$weight[known$weight>upper.bound.weight] <-upper.bound.weight
  
  
  known[is.na(known[,23]), 23] <- mean(known[,23], na.rm = TRUE)  #correction of weight
  known[is.na(known[,16]), 16] <- mean(known[,16], na.rm = TRUE)   #item_count
  
  return(known)
  
}
  

get.read.dataset <-function() {
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # load the package
  classdata<- read.csv("assignment_BADS_WS1617_class.csv", header = TRUE, sep = ",")
  
  
  

  #for cleaning account creation date on the basis of order date  
  classdata$account_creation_date <- as.character(classdata$account_creation_date) #change class from factor to charcter
  classdata$order_date <- as.character(classdata$order_date) #same as above
  classdata$account_creation_date[is.na(classdata$account_creation_date)] <-  (classdata$order_date[is.na(classdata$account_creation_date)]) #replacing NA's with order date values
  classdata$account_creation_date <- as.factor(classdata$account_creation_date) #convert back to factor
  classdata$order_date <- as.factor(classdata$order_date) #same as above
  
  
  #cleaning delivery date actual on the basis of order date
  classdata$order_date <- as.Date(classdata$order_date) #change class from factor to Date
  classdata$deliverydate_actual <- as.Date(classdata$deliverydate_actual) #same as above
  classdata$deliverydate_actual <- with(classdata, as.Date(ifelse(is.na(deliverydate_actual), order_date + days(4), deliverydate_actual), origin = "1970-01-01"))
  classdata$date_diff<-difftime(classdata$deliverydate_actual, classdata$order_date, units = "days")
  classdata$date_diff <- as.numeric(classdata$date_diff) #change class to numeric
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile <- as.numeric(summary(classdata$date_diff)[2])
  upper.quartile <- as.numeric(summary(classdata$date_diff)[5])
  IQR <- upper.quartile - lower.quartile
  # Calculate upper bound value
  upper.bound <- ceiling(upper.quartile + 1.5*IQR)
  classdata$date_diff[classdata$date_diff>upper.bound] <-NA # numeric values more than 15 are replaced with NA
  classdata$deliverydate_actual <- with(classdata, as.Date(ifelse(is.na(date_diff), order_date + days(upper.bound), deliverydate_actual), origin = "1970-01-01"))#replace outliers with order date + 2
  
  
  #cleaning delivery date estimated on the basis of order date
  classdata$deliverydate_estimated <- as.Date(classdata$deliverydate_estimated) #same as above
  classdata$clean_date_diff<-difftime(classdata$deliverydate_estimated, classdata$order_date, units = "days")#to remove outliers from delivery date estimated
  classdata$clean_date_diff <- as.numeric(classdata$clean_date_diff) #change class to numeric
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile.est <- as.numeric(summary(classdata$clean_date_diff)[2])
  upper.quartile.est <- as.numeric(summary(classdata$clean_date_diff)[5])
  IQR.est <- upper.quartile.est - lower.quartile.est
  # Calculate upper bound value
  upper.bound.est <- ceiling(upper.quartile + 1.5*IQR.est)
  classdata$clean_date_diff[classdata$clean_date_diff>upper.bound.est] <-NA
  classdata$clean_date_diff[classdata$clean_date_diff<0] <-NA
  classdata$deliverydate_estimated <- with(classdata, as.Date(ifelse(is.na(clean_date_diff), order_date + days(upper.bound.est), deliverydate_estimated), origin = "1970-01-01"))
  
  
  #for cleaing numerical values
  # Find the quartile values and the inter-quantile-range IQR
  lower.quartile.weight <- as.numeric(summary(classdata$weight)[2])
  upper.quartile.weight <- as.numeric(summary(classdata$weight)[5])
  IQR.weight <- upper.quartile.weight - lower.quartile.weight
  # Calculate upper bound value
  upper.bound.weight <- ceiling(upper.quartile + 1.5*IQR.weight)
  classdata$weight[classdata$weight>upper.bound.weight] <-upper.bound.weight
  
  classdata[is.na(classdata[,23]), 23] <- mean(classdata[,23], na.rm = TRUE)  #correction of weight
  classdata[is.na(classdata[,16]), 16] <- mean(classdata[,16], na.rm = TRUE)   #item_count
  
  return(classdata)
}


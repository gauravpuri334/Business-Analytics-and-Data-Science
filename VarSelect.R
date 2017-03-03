
if (!require('leaps')) install.packages("leaps");
library("leaps")
if (!require('MASS')) install.packages("MASS");
library("MASS")
leaps=regsubsets(known$return_customer ~account_creation_date + remitted_items + newsletter + other_count +referrer + payment + schoolbook_count + order_date + coupon + goods_value + delivery + item_count + weight + cost_shipping, data = known, nvmax = 5, really.big = T)
summary(reg1)
regsubsets.formula(known$return_customer ~ account_creation_date + remitted_items + newsletter + other_count +referrer + payment + schoolbook_count + order_date + coupon + goods_value + delivery + item_count + weight + cost_shipping, data = known, nvmax = 4)

source("helperFunction.R")
known <- data.frame(get.data.dataset())

null=lm(known$return_customer~1, data=known)
null
full=lm(known$return_customer~newsletter + cost_shipping + remitted_items + 
          delivery + item_count + audiobook_download_count + model + 
          used_items + account_creation_date + coupon + paperback_count + 
          ebook_count + other_count + imported_count + referrer +
          audiobook_count + goods_value + order_date + deliverydate_estimated + advertising_code + deliverydate_actual
        + points_redeemed + weight, data=known)
full
step=step(null, scope=list(lower=null, upper=full), direction="forward")



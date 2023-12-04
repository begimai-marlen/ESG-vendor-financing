setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

#load files
load(file = "sscf_merged_25.RData")

# load library
library(BBmisc)
library(lubridate)
library(graphics)
library(plyr)
library(dplyr)
library(zoo)
library(tibble)
library(data.table)
library(plm)
library(stats)
library(lmtest)
library(utils)
library(rio)
library(xtable)
library(openxlsx) 
library(naniar)
library(tidyr)
library(MASS)

# functions 
scale.fn <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
manual.scale.fn <- function(x, alpha = 1,  na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / alpha
# Creating additional variables
# binary variable for suppliers who adopted the program
sscf_merged_25$adoption_time <- ifelse(sscf_merged_25$SID == "CG" & sscf_merged_25$VI_Issue_Date >= "2020-05-21"|
                              sscf_merged_25$SID == "AG" & sscf_merged_25$VI_Issue_Date >= "2020-04-01"|
                              sscf_merged_25$SID == "DF" & sscf_merged_25$VI_Issue_Date >= "2020-05-13"|
                              sscf_merged_25$SID == "FH" & sscf_merged_25$VI_Issue_Date >= "2018-10-01"|
                              sscf_merged_25$SID == "AE" & sscf_merged_25$VI_Issue_Date >= "2020-03-02"|
                              sscf_merged_25$SID == "BJ" & sscf_merged_25$VI_Issue_Date >= "2019-11-15"|
                              sscf_merged_25$SID == "BZ" & sscf_merged_25$VI_Issue_Date >= "2020-04-07"|
                              sscf_merged_25$SID == "DM" & sscf_merged_25$VI_Issue_Date >= "2020-09-16"|
                              sscf_merged_25$SID == "BS" & sscf_merged_25$VI_Issue_Date >= "2020-06-03"|
                              sscf_merged_25$SID == "DD" & sscf_merged_25$VI_Issue_Date >= "2020-10-05"| # have the time with safe assessment
                              sscf_merged_25$SID == "GM" & sscf_merged_25$VI_Issue_Date >= "2020-02-28"|
                              sscf_merged_25$SID == "AM" & sscf_merged_25$VI_Issue_Date >= "2020-03-11"|
                              sscf_merged_25$SID == "FR" & sscf_merged_25$VI_Issue_Date >= "2019-09-10"|
                              sscf_merged_25$SID == "EB" & sscf_merged_25$VI_Issue_Date >= "2020-05-26"|
                              sscf_merged_25$SID == "GE" & sscf_merged_25$VI_Issue_Date >= "2020-08-07"|
                              sscf_merged_25$SID == "GX" & sscf_merged_25$VI_Issue_Date >= "2020-03-26"|
                              sscf_merged_25$SID == "GK" & sscf_merged_25$VI_Issue_Date >= "2018-12-17"|
                              sscf_merged_25$SID == "BQ" & sscf_merged_25$VI_Issue_Date >= "2019-06-16"|
                              sscf_merged_25$SID == "CQ" & sscf_merged_25$VI_Issue_Date >= "2019-09-10", 1,0)

sscf_merged_25$has_adopted <- sscf_merged_25$adoption_time  
sscf_merged_25$adopter <- ifelse(sscf_merged_25$SID == "CG" |
                                 sscf_merged_25$SID == "AG" |
                                 sscf_merged_25$SID == "DF" |
                                 sscf_merged_25$SID == "FH" |
                                 sscf_merged_25$SID == "AE" |
                                 sscf_merged_25$SID == "BJ" |
                                 sscf_merged_25$SID == "BZ" |
                                 sscf_merged_25$SID == "DM" |
                                 sscf_merged_25$SID == "BS" |
                                 sscf_merged_25$SID == "DD" |
                                 sscf_merged_25$SID == "GM" |
                                 sscf_merged_25$SID == "AM" |
                                 sscf_merged_25$SID == "FR" |
                                 sscf_merged_25$SID == "EB" |
                                 sscf_merged_25$SID == "GE" |
                                 sscf_merged_25$SID == "GX" |
                                 sscf_merged_25$SID == "GK" |
                                 sscf_merged_25$SID == "BQ" |
                                 sscf_merged_25$SID == "CQ", 1, 0)

# continuous variable of time 
sscf_merged_25$model.time <- as.numeric(as.Date(sscf_merged_25$VI_Issue_Date))
sscf_merged_25$model.time.year <- manual.scale.fn(sscf_merged_25$model.time,alpha=365)

## SAFE variable analysis
summary(model_fit <- polr(as.factor(Grade) ~ Business_Unit  + Product_Division 
                          + has_adopted  + adopter
                          + model.time.year,
                          data = sscf_merged_25, Hess = TRUE))

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE) * 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table <- as.data.frame(summary_table)

write.xlsx(summary_table, file = paste("summary_tables.xlsx"), row.names = TRUE)

# random analysis
summary(order_fill_rate <- plm(Order_Fill_Rate ~ Business_Unit + Product_Division 
                               + model.time.year + has_adopted + adopter, 
                               data = sscf_merged_25, model="random",
                               index=c("SID")))

summary(price_index <- plm(price.index ~ Business_Unit + Product_Division 
                           + model.time.year + has_adopted + adopter, 
                           data = sscf_merged_25,model="random", 
                           index=c("SID")))

#sscf_merged_25$On_Time_Days_Conf.year <- manual.scale.fn(sscf_merged_25$On_Time_Days_Conf,alpha = 365)
summary(on_time_delivery <- plm(On_Time_Days_Conf ~ Business_Unit + Product_Division 
                                +  model.time.year + has_adopted + adopter, 
                               data = sscf_merged_25,model="random",
                               index=c("SID")))

output.coef.price_index <- as.data.frame(summary(price_index)$coef)
output.coef.order_fill_rate <- as.data.frame(summary(order_fill_rate)$coef)
output.coef.on_time_delivery <- as.data.frame(summary(on_time_delivery)$coef)

Results <- list("price_index" = output.coef.price_index,  
                "order_fill_rate" = output.coef.order_fill_rate, 
                "on_time_delivery" = output.coef.on_time_delivery)

write.xlsx(Results, file = paste("Results.xlsx"), row.names = TRUE)

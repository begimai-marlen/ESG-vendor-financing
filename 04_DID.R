
setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

# load files
load(file = "sscf.RData")

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

# cut 6 suppliers who adopted before 2018
sscf <-  sscf %>% subset(SID %in% c("CG", "AG", "DF", "FH", "AE",
                                    "BJ", "BZ", "DM", "BS", "DW",
                                    "DD", "GM", "AM", "FR", "CQ",
                                    "EB", "GE", "GX", "GK", "BQ", 
                                    "BB", "CK", "CR", "DL", "FX"))


# create a dummy variable to identify the group exposed to the treatment - create a dummy for 20 suppliers
sscf$treated <- ifelse(sscf$SID == "CG" |
                         sscf$SID == "AG" |
                         sscf$SID == "DF" |
                         sscf$SID == "FH" |
                         sscf$SID == "AE" |
                         sscf$SID == "BJ" |
                         sscf$SID == "BZ" |
                         sscf$SID == "DM" |
                         sscf$SID == "BS" |
                         sscf$SID == "DW" |
                         sscf$SID == "DD" |
                         sscf$SID == "GM" |
                         sscf$SID == "AM" |
                         sscf$SID == "FR" |
                         sscf$SID == "EB" |
                         sscf$SID == "GE" |
                         sscf$SID == "GX" |
                         sscf$SID == "GK" |
                         sscf$SID == "BQ" |
                         sscf$SID == "CQ", 1, 0)

### create dummy for adoption time
# create a dummy when adoption started 
sscf$adoption_time <- ifelse(sscf$SID == "CG" & sscf$VI_Issue_Date >= "2020-05-21"|
                             sscf$SID == "AG" & sscf$VI_Issue_Date >= "2020-04-01"|
                             sscf$SID == "DF" & sscf$VI_Issue_Date >= "2020-05-13"|
                             sscf$SID == "FH" & sscf$VI_Issue_Date >= "2018-10-01"|
                             sscf$SID == "AE" & sscf$VI_Issue_Date >= "2020-03-02"|
                             sscf$SID == "BJ" & sscf$VI_Issue_Date >= "2019-11-15"|
                             sscf$SID == "BZ" & sscf$VI_Issue_Date >= "2020-04-07"|
                             sscf$SID == "DM" & sscf$VI_Issue_Date >= "2020-09-16"|
                             sscf$SID == "BS" & sscf$VI_Issue_Date >= "2020-06-03"|
                             sscf$SID == "DW" & sscf$VI_Issue_Date >= "2020-02-28"|
                             sscf$SID == "DD" & sscf$VI_Issue_Date >= "2020-02-28"|
                             sscf$SID == "GM" & sscf$VI_Issue_Date >= "2020-02-28"|
                             sscf$SID == "AM" & sscf$VI_Issue_Date >= "2020-03-11"|
                             sscf$SID == "FR" & sscf$VI_Issue_Date >= "2019-09-10"|
                             sscf$SID == "EB" & sscf$VI_Issue_Date >= "2020-05-26"|
                             sscf$SID == "GE" & sscf$VI_Issue_Date >= "2020-08-07"|
                             sscf$SID == "GX" & sscf$VI_Issue_Date >= "2020-03-26"|
                             sscf$SID == "GK" & sscf$VI_Issue_Date >= "2018-12-17"|
                             sscf$SID == "BQ" & sscf$VI_Issue_Date >= "2019-06-16"|
                             sscf$SID == "CQ" & sscf$VI_Issue_Date >= "2019-09-10", 1,0)

                                         
# Create Z values 

#make adoption time as a date 

#sscf$adoption_date <- ifelse(sscf$adoption_time == "1" = sscf$VI_Issue_Date)

#sscf$Z <- ifelse(sscf$adoption_time >= sscf$VI_Issue_Date, 1, 0)

#plyr::count(sscf$Z)

### DID analysis 
# create an interaction between time and treated - sscf$DID interaction
sscf$has_adopted <- sscf$adoption_time  
sscf$model.time <- as.numeric(as.Date(sscf$VI_Issue_Date))
sscf$model.time <- sscf$model.time - min(sscf$model.time)

summary(price_index <- plm(price.index ~ PO_VI_Unit + PO_VI_Merch_Value + Supplier_in_program + PO_VI_Price + 
                     InvoiceFinanced +
                     treated+ model.time + has_adopted + 
                     has_adopted:model.time, 
                     data = sscf,model="random",
                     index=c("SID")))

summary(order_fill_rate <- plm(Order_Fill_Rate ~ PO_VI_Unit + PO_VI_Merch_Value + Supplier_in_program + 
                     PO_VI_Price + InvoiceFinanced +
                     treated+ model.time + has_adopted + 
                     has_adopted:model.time, 
                     data = sscf,model="random",
                     index=c("SID")))


summary(on_time_delivery <- plm(On_Time_Days_Conf ~ PO_VI_Unit + PO_VI_Merch_Value + 
                     Supplier_in_program + 
                     PO_VI_Price + InvoiceFinanced +
                     treated+ model.time + has_adopted + 
                     has_adopted:model.time, 
                     data = sscf,model="random",
                     index=c("SID")))



#coeftest(re.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0"))

output.coef.price_index <- as.data.frame(summary(price_index)$coef)
output.r.squared.price_index <- as.data.frame(summary(price_index)$r.squared)

output.coef.order_fill_rate <- as.data.frame(summary(order_fill_rate)$coef)
output.r.squared.order_fill_rate <- as.data.frame(summary(order_fill_rate)$r.squared)

output.coef.on_time_delivery <- as.data.frame(summary(on_time_delivery)$coef)
output.r.squared.on_time_delivery <- as.data.frame(summary(on_time_delivery)$r.squared)

Results <- list("price_index" = output.coef.price_index, "Rsq.price_index" = output.r.squared.price_index, 
                "order_fill_rate" = output.coef.order_fill_rate, "Rsq order_fill_rate" = output.r.squared.order_fill_rate,
                "on_time_delivery" = output.coef.on_time_delivery, "Rsq on_time_delivery" = output.r.squared.on_time_delivery)


#i=read.csv(file="i.csv")[2]+1

write.xlsx(Results, file = paste("Results.xlsx"), row.names = TRUE)

#write.csv(i,file="i.csv")
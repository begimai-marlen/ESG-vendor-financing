setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

# load files
load(file = "sscf_merged.RData")
load(file = "sscf_merged_25.RData")
load(file = "sscf_suppliers.RData")
load(file = "sscf_in_no_adoption.RData")
load(file = "no_sscf.RData")
load(file = "sscf_suppliers_before2018.RData")

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

# transaction level data 
min(sscf_merged$PO_VI_Total_Value)
quantile(sscf_merged$PO_VI_Total_Value, 0.25)
median(sscf_merged$PO_VI_Total_Value)
mean(sscf_merged$PO_VI_Total_Value)
quantile(sscf_merged$PO_VI_Total_Value, 0.75)
max(sscf_merged$PO_VI_Total_Value)

# transaction for suppliers who adopted the program before 2018 
min(sscf_suppliers_before2018$PO_VI_Total_Value)
quantile(sscf_suppliers_before2018$PO_VI_Total_Value, 0.25)
median(sscf_suppliers_before2018$PO_VI_Total_Value)
mean(sscf_suppliers_before2018$PO_VI_Total_Value)
quantile(sscf_suppliers_before2018$PO_VI_Total_Value, 0.75)
max(sscf_suppliers_before2018$PO_VI_Total_Value)

# transaction for suppliers who adopted after 10.2018 
min(sscf_suppliers$PO_VI_Total_Value)
quantile(sscf_suppliers$PO_VI_Total_Value, 0.25)
median(sscf_suppliers$PO_VI_Total_Value)
mean(sscf_suppliers$PO_VI_Total_Value)
quantile(sscf_suppliers$PO_VI_Total_Value, 0.75)
max(sscf_suppliers$PO_VI_Total_Value)

# transaction for suppliers in the program, but not adopters
min(sscf_in_no_adoption$PO_VI_Total_Value)
quantile(sscf_in_no_adoption$PO_VI_Total_Value, 0.25)
median(sscf_in_no_adoption$PO_VI_Total_Value)
mean(sscf_in_no_adoption$PO_VI_Total_Value)
quantile(sscf_in_no_adoption$PO_VI_Total_Value, 0.75)
max(sscf_in_no_adoption$PO_VI_Total_Value)

# transaction for suppliers NOT in the program 
min(no_sscf$PO_VI_Total_Value)
quantile(no_sscf$PO_VI_Total_Value, 0.25)
median(no_sscf$PO_VI_Total_Value)
mean(no_sscf$PO_VI_Total_Value)
quantile(no_sscf$PO_VI_Total_Value, 0.75)
max(no_sscf$PO_VI_Total_Value)

# Overviews
# supplier level
temp <- plyr::count(sscf_merged_25$SID)
temp$percentage <- (temp$freq/sum(temp$freq))*100
temp$Spend <- sscf_merged_25 %>%  group_by(SID) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=temp, file="SID.csv")

df <- plyr::count(sscf$S_COUNTRY_)
df$percentage <- (df$freq/sum(df$freq))*100
df$Spend <- sscf %>%  group_by(S_COUNTRY_) %>% summarise(Spend = sum(PO_VI_Total_Value))

write.csv(x=df, file="S_Country.csv")

business.unit <- plyr::count(sscf_merged_25$Business_Unit)
business.unit$percentage <- (business.unit$freq/sum(business.unit$freq))*100
business.unit$Spend <- sscf_merged_25 %>%  group_by(Business_Unit) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=business.unit, file="business.unit.csv")

product_division <- plyr::count(sscf_merged_25$Product_Division)
product_division$percentage <- (product_division$freq/sum(product_division$freq))*100
product_division$Spend <- sscf_merged_25 %>%  group_by(Product_Division) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=product_division, file="product_division.csv")

suppliers_in_program <- plyr::count(sscf$Supplier_in_program)
df1 <- plyr::count(sscf[sscf$InvoiceFinanced == TRUE,]$SID)
df <- plyr::count(sscf[sscf$Supplier_in_program == TRUE,]$SID)
df<- sscf[sscf$first_scf_trans == TRUE,] %>% select(SID, VI_Issue_Date)

plant_level <- plyr::count(sscf_merged_25$FactoryNo)
plant_level$percentage <- (plant_level$freq/sum(plant_level$freq))*100
plant_level$Spend <- sscf_merged_25 %>%  group_by(FactoryNo) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=plant_level, file="plant_level.csv")

## Order Fill Rate
# Order Fill Rate level data 
min(sscf_merged_25$Order_Fill_Rate)
quantile(sscf_merged_25$Order_Fill_Rate, 0.25)
median(sscf_merged_25$Order_Fill_Rate)
mean(sscf_merged_25$Order_Fill_Rate)
quantile(sscf_merged_25$Order_Fill_Rate, 0.75)
max(sscf_merged_25$Order_Fill_Rate)

# Order Fill Rate for suppliers who adopted after 10.2018 
min(sscf_suppliers$Order_Fill_Rate)
quantile(sscf_suppliers$Order_Fill_Rate, 0.25)
median(sscf_suppliers$Order_Fill_Rate)
mean(sscf_suppliers$Order_Fill_Rate)
quantile(sscf_suppliers$Order_Fill_Rate, 0.75)
max(sscf_suppliers$Order_Fill_Rate)

# Order Fill Rate for suppliers NOT in the program 
min(no_sscf$Order_Fill_Rate)
quantile(no_sscf$Order_Fill_Rate, 0.25)
median(no_sscf$Order_Fill_Rate)
mean(no_sscf$Order_Fill_Rate)
quantile(no_sscf$Order_Fill_Rate, 0.75)
max(no_sscf$Order_Fill_Rate)

# Order Fill Rate for sscf_in_no_adoption
min(sscf_in_no_adoption$Order_Fill_Rate)
quantile(sscf_in_no_adoption$Order_Fill_Rate, 0.25)
median(sscf_in_no_adoption$Order_Fill_Rate)
mean(sscf_in_no_adoption$Order_Fill_Rate)
quantile(sscf_in_no_adoption$Order_Fill_Rate, 0.75)
max(sscf_in_no_adoption$Order_Fill_Rate)

## Price Index
# Price Index level data 
min(sscf_merged_25$price.index)
quantile(sscf_merged_25$price.index, 0.25)
median(sscf_merged_25$price.index)
mean(sscf_merged_25$price.index)
quantile(sscf_merged_25$price.index, 0.75)
max(sscf_merged_25$price.index)

# Price Index  for suppliers who adopted after 10.2018 
min(sscf_suppliers$price.index)
quantile(sscf_suppliers$price.index, 0.25)
median(sscf_suppliers$price.index)
mean(sscf_suppliers$price.index)
quantile(sscf_suppliers$price.index, 0.75)
max(sscf_suppliers$price.index)

# Price Index for sscf_in_no_adoption
min(sscf_in_no_adoption$price.index)
quantile(sscf_in_no_adoption$price.index, 0.25)
median(sscf_in_no_adoption$price.index)
mean(sscf_in_no_adoption$price.index)
quantile(sscf_in_no_adoption$price.index, 0.75)
max(sscf_in_no_adoption$price.index)

# Price Index for suppliers NOT in the program 
min(no_sscf$price.index)
quantile(no_sscf$price.index, 0.25)
median(no_sscf$price.index)
mean(no_sscf$price.index)
quantile(no_sscf$price.index, 0.75)
max(no_sscf$price.index)


## On Time Delivery
# On Time Delivery level data 
min(sscf_merged_25$On_Time_Days_Conf)
quantile(sscf_merged_25$On_Time_Days_Conf, 0.25)
median(sscf_merged_25$On_Time_Days_Conf)
mean(sscf_merged_25$On_Time_Days_Conf)
quantile(sscf_merged_25$On_Time_Days_Conf, 0.75)
max(sscf_merged_25$On_Time_Days_Conf)

# On Time Delivery for suppliers who adopted after 10.2018 
min(sscf_suppliers$On_Time_Days_Conf)
quantile(sscf_suppliers$On_Time_Days_Conf, 0.25)
median(sscf_suppliers$On_Time_Days_Conf)
mean(sscf_suppliers$On_Time_Days_Conf)
quantile(sscf_suppliers$On_Time_Days_Conf, 0.75)
max(sscf_suppliers$On_Time_Days_Conf)

# On Time Deliveryfor sscf_in_no_adoption
min(sscf_in_no_adoption$On_Time_Days_Conf)
quantile(sscf_in_no_adoption$On_Time_Days_Conf, 0.25)
median(sscf_in_no_adoption$On_Time_Days_Conf)
mean(sscf_in_no_adoption$On_Time_Days_Conf)
quantile(sscf_in_no_adoption$On_Time_Days_Conf, 0.75)
max(sscf_in_no_adoption$On_Time_Days_Conf)

# On Time Delivery for suppliers NOT in the program 
min(no_sscf$On_Time_Days_Conf)
quantile(no_sscf$On_Time_Days_Conf, 0.25)
median(no_sscf$On_Time_Days_Conf)
mean(no_sscf$On_Time_Days_Conf)
quantile(no_sscf$On_Time_Days_Conf, 0.75)
max(no_sscf$On_Time_Days_Conf)


# Analysis of 1 supplier with SAFE

plyr::count(sscf[sscf$SID == "EB",]$FactoryNo)
plyr::count(sscf[sscf$SID == "EB",]$SAFE)

plyr::count(sscf$SAFE)

safe <- plyr::count(sscf_merged_25$Grade)
safe$percentage <- (safe$freq/sum(safe$freq))*100
safe$Spend <- sscf_merged_25 %>%  group_by(Grade) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=safe, file="safe.csv")

plyr::count(sscf[sscf$SAFE == "A",]$SID)
plyr::count(sscf[sscf$SAFE == "B-",]$SID)
plyr::count(sscf[sscf$SAFE == "B+",]$SID)

plyr::count(sscf[sscf$SAFE == "A",]$FactoryNo)
plyr::count(sscf[sscf$SAFE == "B-",]$FactoryNo)
plyr::count(sscf[sscf$SAFE == "B+",]$FactoryNo)

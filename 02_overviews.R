setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

load(file = "sscf_merged_25.RData")

# packages
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

#for(i in 1:length(unique(sscf$SID)))
 # print(length(plyr::count(unique(sscf[sscf$SID ==unique(sscf$SID)[i] ,]$FactoryNo))$x))

### supplier level 
supplier_level <- ddply(sscf_merged_25, "SID", summarise, 
                        Spend = sum(PO_VI_Total_Value),
                        sum_unitsordered = sum(PO_Unit),
                        sum_unitsdelivered = sum(PO_VI_Unit),
                        mean_order_fill_rate = mean(Order_Fill_Rate),
                        mean_dollar_fill = mean(Dollar_Fill, na.rm = T))

unique_values <- sscf %>% count(SID, S_COUNTRY_) %>% rename(c("Trans_Quant"="n")) 
supplier_level <- merge(unique_values, supplier_level)
counts <- ddply(sscf, .(sscf$SID, sscf$FactoryNo), nrow)
unique(sscf[sscf$SID == "GK",]$FactoryNo)

# On Time variable
df <- data.frame(c(GK = var(sscf[sscf$SID == "GK",]$On_Time_Days, na.rm =T),
                   GL = var(sscf[sscf$SID == "GL",]$On_Time_Days, na.rm =T),
                   CK = var(sscf[sscf$SID == "CK",]$On_Time_Days, na.rm =T),
                   GM = var(sscf[sscf$SID == "GM",]$On_Time_Days, na.rm =T),
                   DL = var(sscf[sscf$SID == "DL",]$On_Time_Days, na.rm =T),
                   CW = var(sscf[sscf$SID == "CW",]$On_Time_Days, na.rm =T),
                   AG = var(sscf[sscf$SID == "AG",]$On_Time_Days, na.rm =T),
                   EO = var(sscf[sscf$SID == "EO",]$On_Time_Days, na.rm =T),
                   CR = var(sscf[sscf$SID == "CR",]$On_Time_Days, na.rm =T),
                   BQ = var(sscf[sscf$SID == "BQ",]$On_Time_Days, na.rm =T),
                   CQ = var(sscf[sscf$SID == "CQ",]$On_Time_Days, na.rm =T),
                   FR = var(sscf[sscf$SID == "FR",]$On_Time_Days, na.rm =T),
                   AM = var(sscf[sscf$SID == "AM",]$On_Time_Days, na.rm =T),
                   GX = var(sscf[sscf$SID == "GX",]$On_Time_Days, na.rm =T),
                   CG = var(sscf[sscf$SID == "CG",]$On_Time_Days, na.rm =T),
                   DW = var(sscf[sscf$SID == "DW",]$On_Time_Days, na.rm =T),
                   GE = var(sscf[sscf$SID == "GE",]$On_Time_Days, na.rm =T),
                   BB = var(sscf[sscf$SID == "BB",]$On_Time_Days, na.rm =T),
                   FX = var(sscf[sscf$SID == "FX",]$On_Time_Days, na.rm =T),
                   EN = var(sscf[sscf$SID == "EN",]$On_Time_Days, na.rm =T),
                   BS = var(sscf[sscf$SID == "BS",]$On_Time_Days, na.rm =T),
                   AK = var(sscf[sscf$SID == "AK",]$On_Time_Days, na.rm =T),
                   DF = var(sscf[sscf$SID == "DF",]$On_Time_Days, na.rm =T),
                   FK = var(sscf[sscf$SID == "FK",]$On_Time_Days, na.rm =T),
                   EB = var(sscf[sscf$SID == "EB",]$On_Time_Days, na.rm =T),
                   FH = var(sscf[sscf$SID == "FH",]$On_Time_Days, na.rm =T),
                   BJ = var(sscf[sscf$SID == "BJ",]$On_Time_Days, na.rm =T),
                   DD = var(sscf[sscf$SID == "DD",]$On_Time_Days, na.rm =T),
                   BZ = var(sscf[sscf$SID == "BZ",]$On_Time_Days, na.rm =T),
                   DM = var(sscf[sscf$SID == "DM",]$On_Time_Days, na.rm =T),
                   DY = var(sscf[sscf$SID == "DY",]$On_Time_Days, na.rm =T),
                   AE = var(sscf[sscf$SID == "AE",]$On_Time_Days, na.rm =T)))

df$SID <- (c("GK", "GL", "CK", "GM","DL", "CW","AG", "EO","CR","BQ","CQ", "FR","AM",
             "GX","CG","DW","GE", "BB","FX","EN", "BS","AK","DF", "FK","EB","FH","BJ", "DD","BZ","DM","DY", "AE"))

supplier_level <- merge(x = supplier_level, y= df, by = "SID", all = TRUE)

supplier_level <- rename(supplier_level, Var_OnTime_Delivery = 9)

save(supplier_level, file = "supplier_level.RData")  

### plant level - factory
plant_level <- ddply(sscf, "FactoryNo", summarise, Spend = sum(PO_VI_Total_Value),
                                sum_unitsordered = sum(PO_Unit),
                                sum_unitsdelivered = sum(PO_VI_Unit),
                                mean_order_fill_rate = mean(Order_Fill_Rate),
                                mean_dollar_fill = mean(Dollar_Fill, na.rm = T))

unique_values <- sscf %>% count(FactoryNo, CountryofOrigin) %>% rename(c("Trans_Quant"="n")) 

plant_level <- merge(unique_values, plant_level)

save(plant_level, file = "plant_level.RData")  

### business unit level
business_unit_level <- ddply(sscf, "Business_Unit", summarise,
                             Spend = sum(PO_VI_Total_Value),
                             sum_unitsordered = sum(PO_Unit),
                             sum_unitsdelivered = sum(PO_VI_Unit),
                             mean_order_fill_rate = mean(Order_Fill_Rate),
                             mean_dollar_fill = mean(Dollar_Fill, na.rm = T))

save(business_unit_level, file = "business_unit_level.RData")  

### product division level 
product_division_level <- ddply(sscf, "Product_Division", summarise,
                                Spend = sum(PO_VI_Total_Value),
                             sum_unitsordered = sum(PO_Unit),
                             sum_unitsdelivered = sum(PO_VI_Unit),
                             mean_order_fill_rate = mean(Order_Fill_Rate),
                             mean_dollar_fill = mean(Dollar_Fill, na.rm = T))

save(product_division_level, file = "product_division_level.RData")  


### product level 
product_level <- ddply(sscf, "Article_Number", summarise,
                       Spend = sum(PO_VI_Total_Value),
                                sum_unitsordered = sum(PO_Unit),
                                sum_unitsdelivered = sum(PO_VI_Unit),
                                mean_order_fill_rate = mean(Order_Fill_Rate),
                                mean_price_ordered = mean(PO_Price),
                                mean_price_delivered = mean(PO_VI_Price),
                                mean_dollar_fill = mean(Dollar_Fill, na.rm = T),
                                mean_price_index = mean(price.index))

save(product_level, file = "product_level.RData")  

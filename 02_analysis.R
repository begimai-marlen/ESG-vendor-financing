setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

# load a file 
load(file = "sscf_merged.RData")

# packages
library(lubridate)
library(plyr)
library(dplyr)
library(zoo)
library(VGAM)
library(tibble)
library(utils)
library(rio)
library(naniar)
library(tidyr)
library(data.table)
library(BBmisc)
library(IndexNumR)
library(VGAM)

sscf_merged$VI_Issue_Date <- as.Date(sscf_merged$VI_Issue_Date)

# suppliers in the program overall
plyr::count(sscf_merged[sscf_merged$Supplier_in_program == TRUE,]$SID)
# AE, AG, AK, AM, BB, BJ, BQ, BS, BZ, CG, CQ, CW, DD, DF, DM, DY, EB, EN, EO, FH, FK, FR, GE, GK, GL, GM, GX 27 suppliers

# suppliers not in the program:
plyr::count(sscf_merged[sscf_merged$Supplier_in_program == FALSE,]$SID)
# CK, CR, DL, FX 4 suppliers

# suppliers in but not adopted
plyr::count(sscf_merged[sscf_merged$InvoiceFinanced == TRUE,]$SID)
# BB, DY

### 

# subset suppliers in the program
sscf_suppliers <- sscf_merged %>%
  subset(SID %in% c("AE", "AG", "AM", "BJ", "BQ", "BS",
                    "BZ", "CG", "CQ", "DD", "DF", "DM",
                    "EB", "FH", "FR", "GE", "GK", "GM",
                    "GX"))

# subset supplier in the program, but not adopters
sscf_in_no_adoption <-  sscf_merged %>% subset(SID %in% c("BB", "DY"))

# subset suppliers not in the program 
no_sscf <- subset(sscf_merged[sscf_merged$Supplier_in_program == FALSE,])

# suppliers adopted before 2018 
sscf_suppliers_before2018 <- sscf_merged %>%
  subset(SID %in% c("CW", "EN", "GL", "AK", "EO", "FK"))

# save files 
save(sscf_suppliers, file = "sscf_suppliers.RData")
save(sscf_in_no_adoption, file = "sscf_in_no_adoption.RData")
save(no_sscf, file = "no_sscf.RData")
save(sscf_suppliers_before2018, file = "sscf_suppliers_before2018.RData")

# # cut 6 suppliers who adopted before 2018: CW, EN, GL, AK, EO, FK
sscf_merged_25 <- sscf_merged %>%
  subset(SID %in% c("AE", "AG", "AM", "BJ", "BQ", "BS",
                    "BZ", "CG", "CQ", "DD", "DF", "DM",
                    "EB", "FH", "FR", "GE", "GK", "GM",
                    "GX", "CK", "CR", "DL", "FX", "BB",
                    "DY"))
# save the new file 
save(sscf_merged_25, file="sscf_merged_25.RData")

###
plyr::count(sscf_merged[sscf_merged$Supplier_in_program == TRUE,]$SID)

# Choose the biggest supplier with multiple factories and biggest spend (revenue for the supplier)
supplier_level <- ddply(sscf_merged, "SID", summarise, Spend = sum(PO_VI_Total_Value),
                        sum_unitsordered = sum(PO_Unit),
                        sum_unitsdelivered = sum(PO_VI_Unit),
                        mean_order_fill_rate = mean(Order_Fill_Rate),
                        mean_dollar_fill = mean(Dollar_Fill, na.rm = T))

#I chose supplier CQ as this supplier has second biggest spend and two factories + supplier that adopted the program
# Analysis of 1 supplier with SAFE
#plyr::count(sscf_merged[sscf_merged$SID == "CQ",]$FactoryNo)

plot(sscf_merged[sscf_merged$SID == "CQ",]$InvoiceFinanced ~ sscf_merged[sscf_merged$SID == "CQ",]$VI_Issue_Date,
     main = "Supplier CQ", xlab= "Invoice Issue Date", ylab= "Invoice Financed")
plot(sscf_merged[sscf_merged$FactoryNo == "101",]$InvoiceFinanced ~ sscf_merged[sscf_merged$FactoryNo == "101",]$VI_Issue_Date,
     main = "Factory Number 101", xlab= "Invoice Issue Date", ylab= "Invoice Financed")
plot(sscf_merged[sscf_merged$FactoryNo == "102",]$InvoiceFinanced ~ sscf_merged[sscf_merged$FactoryNo == "102",]$VI_Issue_Date,
     main = "Factory Number 102", xlab= "Invoice Issue Date", ylab= "Invoice Financed")

#overview for all safe variables 
safe <- plyr::count(sscf_merged$Grade)
safe$percentage <- (safe$freq/sum(safe$freq))*100
safe$Spend <- sscf_merged %>%  group_by(Grade) %>% summarise(Spend = sum(PO_VI_Total_Value))
write.csv(x=safe, file="safe.csv")

#overview of the supplier "EB"
df <- sscf_merged %>% subset(sscf_merged$SID == "EB",)

df155 <- sscf_merged %>% subset(sscf_merged$FactoryNo == "155",)
sum(df155[df155$Grade== "B-",]$PO_VI_Total_Value)
sum(df155[df155$Grade== "B+",]$PO_VI_Total_Value)

df156 <- sscf_merged %>% subset(sscf_merged$FactoryNo == "156",)
sum(df156[df156$Grade== "A",]$PO_VI_Total_Value)
sum(df156[df156$Grade== "B+",]$PO_VI_Total_Value)

temp <- plyr::count(df[df$FactoryNo == "155",]$Grade)
temp <- plyr::count(df[df$FactoryNo == "156",]$Grade)

setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

load(file = "sscf.raw.RData")
load(file = "safe.raw.RData")

# packages
library(lubridate)
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
library(stringr)

#remove.packages("rlang")
#install.packages("rlang")
#library(tidyverse)

# rename variables
colnames(sscf.raw)[colnames(sscf.raw) == 'Supplier Code'] <- 'S_CODE'
colnames(sscf.raw)[colnames(sscf.raw) == 'Supplier Country'] <- 'S_COUNTRY_'
colnames(sscf.raw)[colnames(sscf.raw) == 'Country Of Origin'] <- 'CountryofOrigin'
colnames(sscf.raw)[colnames(sscf.raw) == 'Article No'] <- 'Article_Number'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO Unit'] <- 'PO_Unit'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO Merch Value'] <- 'PO_Merch_Value'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO Create Date'] <- 'PO_Creat_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO Confirm Date'] <- 'PO_Confirm_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'RHD'] <- 'Requested_Handover_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'EHD'] <- 'Expected_Handover_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'CHD'] <- 'Confirmed_Handover_Date_Both'
colnames(sscf.raw)[colnames(sscf.raw) == 'LCHD'] <- 'Latest_Confirmed_Handover_Date_Both'
colnames(sscf.raw)[colnames(sscf.raw) == 'AHD'] <- 'Actual_Handover_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO Status'] <- 'PO_Status'
colnames(sscf.raw)[colnames(sscf.raw) == 'VI Number'] <- 'VI_Number'
colnames(sscf.raw)[colnames(sscf.raw) == 'VI Issue Date'] <- 'VI_Issue_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'PAD AD Date'] <- 'Approved_Invoice_Date'
colnames(sscf.raw)[colnames(sscf.raw) == 'Customer Ship To'] <- 'Shipment_Destin_Country'
colnames(sscf.raw)[colnames(sscf.raw) == 'PRD Date'] <- 'VI_Payment_due_date'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO VI Units'] <- 'PO_VI_Unit'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO VI Merch Value'] <- 'PO_VI_Merch_Value'
colnames(sscf.raw)[colnames(sscf.raw) == 'PO VI Total Value'] <- 'PO_VI_Total_Value'
colnames(sscf.raw)[colnames(sscf.raw) == 'Invoice Financed'] <- 'InvoiceFinanced'

# additional variables
sscf.raw$Supplier_in_program <- !substr(sscf.raw$S_CODE,1,2) %in% c("DL","CK","CR","FX")
sscf.raw$SID = substr(sscf.raw$S_CODE,1,2)
sscf.raw$Product_Division = substr(sscf.raw$S_CODE,3,3)
sscf.raw$Business_Unit= substr(sscf.raw$S_CODE,4,4)
sscf.raw$FactoryNo= substr(sscf.raw$S_CODE,5,7)
sscf.raw$InvoiceFinanced <- sscf.raw$InvoiceFinanced =="Y" 
sscf.raw$Order_Fill_Rate <- sscf.raw$PO_VI_Unit/sscf.raw$PO_Unit
sscf.raw$PO_Price <- sscf.raw$PO_Merch_Value/sscf.raw$PO_Unit 
sscf.raw$PO_VI_Price <- sscf.raw$PO_VI_Merch_Value/sscf.raw$PO_VI_Unit 
sscf.raw$Dollar_Fill <- sscf.raw$PO_VI_Merch_Value/sscf.raw$PO_Merch_Value 

# change dates to Dates 
sscf.raw$Actual_Handover_Date <- as.Date(sscf.raw$Actual_Handover_Date)
sscf.raw$Requested_Handover_Date <- as.Date(sscf.raw$Requested_Handover_Date)
sscf.raw$Confirmed_Handover_Date_Both <- as.Date(sscf.raw$Confirmed_Handover_Date_Both)

# create On time Req variable 
sscf.raw$On_Time_Days_Req <- sscf.raw$Requested_Handover_Date-sscf.raw$Actual_Handover_Date 
sscf.raw$On_Time_Days_Req <- as.numeric(sscf.raw$On_Time_Days_Req)
sscf.raw$On_Time_Days_Req <- as.numeric(ifelse(as.numeric(sscf.raw$On_Time_Days_Req) < 10000,sscf.raw$On_Time_Days_Req,NA))
sscf.raw$On_Time_Req <-  sscf.raw$Requested_Handover_Date == sscf.raw$Actual_Handover_Date 

# create On time confirmed variable 
sscf.raw$On_Time_Days_Conf <- sscf.raw$Confirmed_Handover_Date_Both-sscf.raw$Actual_Handover_Date 
sscf.raw$On_Time_Days_Conf <- as.numeric(sscf.raw$On_Time_Days_Conf)
sscf.raw$On_Time_Days_Conf <- as.numeric(ifelse(as.numeric(sscf.raw$On_Time_Days_Conf) < 10000,sscf.raw$On_Time_Days_Conf,NA))
sscf.raw$On_Time_Days_Conf <- as.numeric(ifelse(as.numeric(sscf.raw$On_Time_Days_Conf) > -10000,sscf.raw$On_Time_Days_Conf,NA))
sscf.raw$On_Time_Conf <-  sscf.raw$Confirmed_Handover_Date_Both == sscf.raw$Actual_Handover_Date 

# renaming variables 
sscf.raw$Product_Division[sscf.raw$Product_Division=="F"] <- "Footwear"
sscf.raw$Product_Division[sscf.raw$Product_Division=="A"] <- "Apparel"
sscf.raw$Product_Division[sscf.raw$Product_Division=="C"] <- "Accessories"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="R"] <- "Run/Train"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="S"] <- "Sportstyle"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="M"] <- "Motorsport"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="B"] <- "Basketball"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="T"] <- "Teamsport"
sscf.raw$Business_Unit[sscf.raw$Business_Unit=="C"] <- "Acc"
#sscf.raw$Business_Unit[sscf.raw$Business_Unit=="S"] <- "Sportstyle Prime/ Select"
#sscf.raw$Business_Unit[sscf.raw$Business_Unit=="S"] <- "Sportstyle Core/ Kids"

# Convert character to numeric
sscf.raw$FactoryNo <- as.numeric(as.character(sscf.raw$FactoryNo))

# Replacing 1990-01-01 with NA 
replace.na <- function(DateVar){
  DateVar <- as.Date(DateVar)
  DateVar <- as.Date(ifelse(as.numeric(DateVar)!= 7305,DateVar,NA))
}

sscf.raw$Confirmed_Handover_Date_Both <- replace.na(sscf.raw$Confirmed_Handover_Date_Both)
sscf.raw$Actual_Handover_Date <- replace.na(sscf.raw$Actual_Handover_Date)
sscf.raw$Latest_Confirmed_Handover_Date_Both <- replace.na(sscf.raw$Latest_Confirmed_Handover_Date_Both)
sscf.raw$Expected_Handover_Date <- replace.na(sscf.raw$Expected_Handover_Date)
sscf.raw$Approved_Invoice_Date <- replace.na(sscf.raw$Approved_Invoice_Date)

# remove spaces in Article Number
sscf.raw$Article_Number <- gsub('\\s+', '', sscf.raw$Article_Number)

# Price index
sscf.raw <- sscf.raw[order(sscf.raw$Approved_Invoice_Date),]
initial.price.df <- as.data.frame(tapply(sscf.raw$PO_Price,sscf.raw$Article_Number,first))
initial.price.df$Article_Number <- rownames(initial.price.df)
colnames(initial.price.df) <- c("Base.price","Article_Number")
rownames(initial.price.df) <- NULL
sscf.raw <- merge(sscf.raw,initial.price.df,by="Article_Number")
sscf.raw$price.index <- (sscf.raw$PO_Price / sscf.raw$Base.price)*100

#sscf.raw <- subset(sscf.raw, format(as.Date(VI_Issue_Date),"%Y") < 2021)

# 149 252 86 safe
# delete unmatching suppliers 
sscf.raw <- sscf.raw[!sscf.raw$FactoryNo == "149", ]
sscf.raw <- sscf.raw[!sscf.raw$FactoryNo == "252", ]
sscf.raw <- sscf.raw[!sscf.raw$FactoryNo == "86", ]
# removed supplier DW

# First transaction
df <- sscf.raw[sscf.raw$InvoiceFinanced,]
setDT(df)
df <- df[order(VI_Issue_Date), .SD[1], by=SID]
df$first_scf_trans <- 1
sscf.raw <- merge(x = sscf.raw, y = df, all.x = T)
sscf.raw$first_scf_trans[is.na(sscf.raw$first_scf_trans)] <- 0
# removed supplier DW

# clean SAFE Dataset
colnames(safe.raw)[colnames(safe.raw) == 'Country'] <- 'CountryofOrigin'
colnames(safe.raw)[colnames(safe.raw) == 'Assessment Grade Score'] <- 'Grade'
colnames(safe.raw)[colnames(safe.raw) == 'Assessment Date'] <- 'Assessment_Date'
colnames(safe.raw)[colnames(safe.raw) == 'Factory Mask'] <- 'Factory_Mask'
safe.raw$SID = substr(safe.raw$Factory_Mask,1,2)
safe.raw$FactoryNo = substr(safe.raw$Factory_Mask,3,5)
safe.raw$FactoryNo = str_remove(safe.raw$FactoryNo, "^0+")
safe.raw$FactoryNo <- as.numeric(as.character(safe.raw$FactoryNo))

#sscf.raw <- subset(sscf.raw, format(as.Date(VI_Issue_Date),"%Y") < 2021)
#safe.raw <- subset(safe.raw, format(as.Date(Assessment_Date),"%Y") < 2021)

#remove na values in safe
#random.sscf <- as.data.frame(plyr::count(sscf.raw$FactoryNo))
#random.safe <- as.data.frame(plyr::count(safe.raw$FactoryNo))
#random.both <- merge(random.sscf, random.safe, by ="x", all = T, na.rm=T)

# check suppliers who adopted
#random <- plyr::count(sscf.raw[sscf.raw$InvoiceFinanced,]$SID)
#random2 <- plyr::count(sscf.raw[sscf.raw$first_scf_trans==1,]$SID)
#random3 <- merge(random, random2, by ="x", all=T)

#save the file
sscf <- rlang::duplicate(sscf.raw, shallow = FALSE)
safe <- rlang::duplicate(safe.raw, shallow = FALSE)
save(sscf, file = "sscf.RData")
save(safe, file = "safe.RData")
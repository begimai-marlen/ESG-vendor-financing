setwd("C:\\Research\\Sustainable Supply Chain Finance\\Data")

load(file = "sscf.RData")
load(file = "safe.RData")

# packages
library(lubridate)
library(plyr) 
library(dplyr)
library(zoo)
library(tibble)
library(data.table)
library(plm)
library(utils)
library(rio)
library(naniar)
library(tidyr)
library(stringr)

sscf <- sscf %>% drop_na() # 18290 NA values in VI Issue date and 25 adopters
safe <- safe %>% drop_na() # 4 NA values in Grades: 4 suppliers after 09.2021

sscf$VI_Issue_Date <- as.Date(sscf$VI_Issue_Date)
safe$Assessment_Date <- as.Date(safe$Assessment_Date)

#merge those two with conditioning 
max(safe$Assessment_Date) - min(safe$Assessment_Date)

# create a data frame with about 34110 observations
days <- as.Date(min(as.Date(safe$Assessment_Date)) : max(as.Date(safe$Assessment_Date)))
N=length(days)
IDS <- unique(safe$FactoryNo)
M <- length(IDS)
ID.col <- IDS
days.col <- days
new.factory.df <- merge(days.col, ID.col)

colnames(new.factory.df)[1] <- "Date"
colnames(new.factory.df)[2] <- "ID"

safe$Assessment_Date <- as.Date(safe$Assessment_Date)
new.factory.df$Date <- as.Date(new.factory.df$Date)

# merge with safe but keep all variables rows in new.factory.df
safe_df <- merge(new.factory.df, safe, by.x = c("Date", "ID"), 
                 by.y = c("Assessment_Date", "FactoryNo"), all.x=T)

safe_df$IDdate <- paste(safe_df$ID,safe_df$Date)
safe_df_sorted <- safe_df %>% arrange(ID, Date)

# replace NA by last observation
safe_df_locf <- rlang::duplicate(safe_df_sorted, shallow = FALSE)
safe_df_locf <- zoo::na.locf(safe_df_sorted, na.rm = TRUE, fromLast = FALSE) #main 

safe_df_locf$temporary.fac.id = substr(safe_df_locf$Factory_Mask,3,5)
safe_df_locf$temporary.fac.id = str_remove(safe_df_locf$temporary.fac.id, "^0+")
safe_df_locf$temporary.fac.id <- as.numeric(safe_df_locf$temporary.fac.id)

safe_df_locf$binary <- ifelse(safe_df_locf$ID != safe_df_locf$temporary.fac.id, 1,0)
safe_df_locf$SID <- ifelse(safe_df_locf$binary == 1, NA, safe_df_locf$SID)
safe_df_locf$CountryofOrigin <- ifelse(safe_df_locf$binary == 1, NA, safe_df_locf$CountryofOrigin)
safe_df_locf$Factory_Mask <- ifelse(safe_df_locf$binary == 1, NA, safe_df_locf$Factory_Mask)
safe_df_locf$Grade <- ifelse(safe_df_locf$binary == 1, NA, safe_df_locf$Grade)

#each row corresponds to a factory-day and has an entry of then latest safe rating, so each supplier has about 1450 rows

#change the date format
safe_df_locf$Date <- as.Date(safe_df_locf$Date)
sscf$VI_Issue_Date <- as.Date(sscf$VI_Issue_Date)

#merge sscf with safe (continious)
sscf_merged <- merge(sscf, safe_df_locf, by.x = c("VI_Issue_Date", "FactoryNo"),
              by.y = c("Date","ID"), all.x = T)

# df <- subset(sscf_merged, sscf_merged$SID.x == "DD")
# plyr::count(df[df$InvoiceFinanced == TRUE,]$FactoryNo) Supplier DD's with a factory 117 will be removed as the first pvfp 
# was used on 2020-05-04 but the assessment was done only on 2020-10-05

# cut all data from 2017 and 2021
sscf_merged <- subset(sscf_merged, format(as.Date(VI_Issue_Date),"%Y") > 2017)
# sscf_merged <- subset(sscf_merged, format(as.Date(VI_Issue_Date),"%Y") < 2021)

# drop NAs - some factories were not assessed before 
sscf_merged <- sscf_merged %>% drop_na()

#remove unnecessary columns 
sscf_merged <-  subset(sscf_merged, select = -c(SID.y, CountryofOrigin.y, IDdate, temporary.fac.id, binary))

#rename column names
colnames(sscf_merged)[colnames(sscf_merged) == 'SID.x'] <- 'SID'
colnames(sscf_merged)[colnames(sscf_merged) == 'CountryofOrigin.x'] <- 'CountryofOrigin'

# order the data from A to B+ to B- to C
sscf_merged$Grade[sscf_merged$Grade == "B+"]  <- "B1" 
sscf_merged$Grade[sscf_merged$Grade == "B-"]  <- "B2"
#save the file
save(sscf_merged, file ="sscf_merged.RData")
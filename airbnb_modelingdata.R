library(data.table)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
#READ DATA----
setwd("~/Desktop/Learn/APAN/APAN5200 daniel")
train = read.csv('./airbnb-recruiting-new-user-bookings/train_users_2.csv',stringsAsFactors = T)
test = read.csv('./airbnb-recruiting-new-user-bookings/test_users.csv',stringsAsFactors = T)

##add country_destination to train data
test$country_destination = NA

##combine train and test data
df_all = rbind(train, test)
setDT(df_all)

# DATA CLEANING----
#change date variables to consistent format
df_all$date_account_created <- as.Date(df_all$date_account_created)
df_all$timestamp_first_active <- as.character(df_all$timestamp_first_active)
df_all$timestamp_first_active <- as.Date(df_all$timestamp_first_active, "%Y%m%d%H%M%S")

##remove 'date_first_booking'
##in test data the column is empty
##thus the column is not useful for predicting
summary(test$date_first_booking)
df_all[,date_first_booking:= NULL]
names(df_all)

##remove outliers of 'age'(<15 or >90)
df_all[age<15 | age > 90, .N ] #counts of outliers: 2987
df_all[age<15 | age > 90, age:='na' ] #remove outliers as na
df_all[is.na(age), .N] #NA of 'age' in df_all = 119853 
df_all[is.na(age), age:= -1] #change all na values to -1
summary(df_all$age)

df_all[is.na(first_affiliate_tracked), .N]


# FEATURE SELECTION----
## one-hot-encoding 
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.table(predict(dummies, newdata = df_all))
###combine data and remove original columns
df_all_combined <- cbind(df_all[,c(which(colnames(df_all) %in% ohe_feats)):= NULL],df_all_ohe)

##create new date features
###divide 'date_account_created' into day, month, quarter, and year
df_all_combined[, day_account_created:= wday(df_all_combined$date_account_created, week_start = 1)] #week starts on Mon
df_all_combined[, month_account_created:= month(df_all_combined$date_account_created)]
df_all_combined[, quarter_account_created:= quarter(df_all_combined$date_account_created)]
df_all_combined[, year_account_created:= year(df_all_combined$date_account_created)]
###divide 'timestamp_first_active' into hour, day, month, quarter, and year
df_all_combined[, hour_first_active:= hour(df_all_combined$timestamp_first_active)]
df_all_combined[, day_first_active:= wday(df_all_combined$timestamp_first_active, week_start = 1)] #week starts on Mon
df_all_combined[, month_first_active:= month(df_all_combined$timestamp_first_active)]
df_all_combined[, quarter_first_active:= quarter(df_all_combined$timestamp_first_active)]
df_all_combined[, year_first_active:= year(df_all_combined$timestamp_first_active)]
###add new variable: time between account created and first active
df_all_combined[, created_less_active:= as.numeric(difftime(df_all_combined$date_account_created, 
                                                            df_all_combined$timestamp_first_active, units = "days"))]
###drop columns
#columns_to_drop = c('date_account_created', 'timestamp_first_active', 'date_first_booking', 'country_destination')
#df_all_combined[,c(which(colnames(df_all) %in% columns_to_drop)):= NULL]

# split train and test
X = df_all_combined[df_all_combined$id %in% train$id,]
X = X[-grep('country_destination', colnames(X))]

#y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% test$id,]

write.csv(X, "./airbnb-recruiting-new-user-bookings/df_all_combined_train.csv", row.names=FALSE)
write.csv(X_test, "./airbnb-recruiting-new-user-bookings/df_all_combined_test.csv", row.names=FALSE)


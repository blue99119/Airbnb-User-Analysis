library(data.table)
library(DT)
library(ggplot2)
library(dplyr)
#read data
setwd("~/Desktop/Learn/APAN/APAN5200 daniel")
train = read.csv('./airbnb-recruiting-new-user-bookings/train_users_2.csv',stringsAsFactors = T)
test = read.csv('./airbnb-recruiting-new-user-bookings/test_users.csv',stringsAsFactors = T)
sessions = read.csv('./airbnb-recruiting-new-user-bookings/sessions.csv',stringsAsFactors = T)
sample_submission_NDF = read.csv('./airbnb-recruiting-new-user-bookings/sample_submission_NDF.csv',stringsAsFactors = T)
countries = read.csv('./airbnb-recruiting-new-user-bookings/countries.csv',stringsAsFactors = T)
age_gender_bkts = read.csv('./airbnb-recruiting-new-user-bookings/age_gender_bkts.csv',stringsAsFactors = T)
training.data <- fread(input = "./airbnb-recruiting-new-user-bookings/train_users_2.csv",stringsAsFactors = T)
df_train = setDT(train)

#add country_destination to train data
test$country_destination = NA

#combine train and test data
df_all = rbind(train, test)

##Data exploration----
#explore data
str(train)
table(train$gender)
table(train$signup_method)
table(train$language)
table(train$affiliate_channel)
table(train$affiliate_provider)
table(train$country_destination)
table(sample_submission_NDF$country)
summary(train)

#check categorical variables
df_train[, sort(unique(gender))] #"-unknown-" "FEMALE"    "MALE"      "OTHER" 
df_train[, sort(unique(signup_method))] #"basic" "facebook" "google"
df_train[, sort(unique(language))]
df_train[, sort(unique(affiliate_channel))] #"api" "content" "direct" "other" "remarketing"   "sem-brand"     "sem-non-brand" "seo" 
df_train[, sort(unique(affiliate_provider))]
df_train[, .N, ]
df_train[, summary(age)]

##Data cleaning----
#parse date variable
train$date_account_created <- as.Date(train$date_account_created)

#create year and month variable of date_account_created
train$date_account_created.year <- as.numeric(format(train$date_account_created, "%y"))
train$date_account_created.month <- as.numeric(format(train$date_account_created, "%m"))
#
train$timestamp_first_active <- as.character(train$timestamp_first_active)
train$timestamp_first_active <- as.Date(train$timestamp_first_active, "%Y%m%d%H%M%S")

##visualization----
#age_gender_bkts
age_gender_bkts <- age_gender_bkts[order(age_gender_bkts$age_bucket), ]
ggplot(data=age_gender_bkts,aes(x=age_bucket,y=population_in_thousands))+ 
  geom_bar(stat = 'identity') +
  geom_line(aes(group=country_destination, color=country_destination))

#plot date_account_created.month and country_destination
ggplot(train, aes(date_account_created.month, fill=country_destination))+
  geom_bar(position="identity", alpha=0.5)+ 
  scale_y_log10()+
  facet_wrap(~country_destination)

ggplot(train, aes(x = date_account_created.year))+
  geom_bar(position="identity", alpha=0.5)

#date-country-count
#hist
ggplot(data=train,mapping = aes(x=date_account_created,fill=factor(country_destination)))+
  geom_histogram()
#line
ggplot(data=train,mapping = aes(x=date_account_created,color=factor(country_destination)))+
  geom_freqpoly(linewidth=1.2, bins = 30)
#density line
ggplot(data=train,mapping = aes(x=date_account_created,color=factor(country_destination)))+
  geom_density(linewidth=1.2)

#boxplot-country???
ggplot(data=train,aes(x=age,y=country_destination))+
  geom_boxplot()

ggplot(train, aes(x = date_account_created))+
  geom_line(y = )


##model----
#split data
library(caret)
set.seed(1031)
split = createDataPartition(y=df_train$country_destination,p = 0.7,list = F,groups = 20)
df_train_sp = df_train[split,]
df_test_sp = df_train[-split,]


#age_gender_bkts----
setDT(age_gender_bkts)
summary(age_gender_bkts)
table(age_gender_bkts$population_in_thousands, age_gender_bkts$country_destination)
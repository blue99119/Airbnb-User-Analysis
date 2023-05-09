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

# session data
setDT(sessions)
sessions_device <- sessions[,.SD, .SDcols = c('user_id', 'device_type', 'secs_elapsed')]
aggregated_lvl1 <- sessions_device[, .(total_secs_elapsed = sum(secs_elapsed)), by = c('user_id', 'device_type')]
## set na value as 0
na.values <- c("n/a", "NA", "N/A")
aggregated_lvl1[total_secs_elapsed %in% na.values, total_secs_elapsed := NA]
aggregated_lvl1[is.na(total_secs_elapsed), total_secs_elapsed := 0]
## determine primary device (longest secs)
setorderv(x = aggregated_lvl1, cols = c('user_id','total_secs_elapsed'), order = -1)
df_primary <- aggregated_lvl1[, .SD[1], keyby = "user_id"]
df_primary <- df_primary[,.('user_id' = get('user_id'), 'primary_device'=get('device_type'), 'primary_secs'=get('total_secs_elapsed'))]

##Data exploration----
#explore data
str(train) #213451 obs. of  16 variables
table(train$gender) #ORDER: 'unknown' 'Female' 'Male'
table(train$signup_method) #ORDER: 'basic' 'facebook' 'google' 
table(train$language) #ORDER: 'en'
table(train$affiliate_channel) #ORDER: 'direct'
table(train$affiliate_provider) #ORDER:'direct' 'google' 'other'
table(train$country_destination) #ORDER:'NDF' 'US' 'other'
table(train$first_affiliate_tracked) #ORDER: 'untracked'
table(train$signup_app) #ORDER: 'Web' 'iOS'
table(train$first_device_type) #ORDER: 'Mac Desktop' 'Windows Desktop' 'iPhone'
table(train$first_browser) #ORDER: 'Chrome' 'Safari' 'Firefox'

summary(train)


##Data cleaning----
###check na
mean.missing <- function(x) {
  return(mean(is.na(x)))
}
df_train[, lapply(X = .SD, FUN = "mean.missing")]



##Visualization----
#age_gender_bkts
age_gender_bkts <- age_gender_bkts[age_bucket == '100+', age_bucket:= '99+']
age_gender_bkts <- age_gender_bkts[age_bucket == '5-9'|age_bucket == '0-4', age_bucket:= NULL]
age_gender_bkts <- age_gender_bkts[order(age_gender_bkts$age_bucket), ]
ggplot(data=age_gender_bkts,aes(x=age_bucket,y=population_in_thousands))+ 
  geom_bar(stat = 'identity') +
  geom_line(aes(group=country_destination, color=country_destination))

# Plot count of user account created daily/monthly/quarterly/yearly----
## INSIGHT: USER ACCOUNT CREATED HAD AN EXPONENTIAL GROWTH IN 2013-2014
ggplot(X, aes(x = date_account_created)) +
  geom_line(stat = 'count') +
  labs(x = "Year", y = "Number of Users Created") +
  ggtitle("Time Series of User Account Creation (year)")

## INSIGHT: USER ACCOUNT CREATED MUCH MORE FROM JAN to JUNE
ggplot(X, aes(x = month_account_created)) +
  geom_line(stat = 'count') +
  labs(x = "Month", y = "Number of Users Created") +
  ggtitle("Time Series of User Account Creation (month)") +
  scale_x_continuous(breaks = 1:12)

## INSIGHT: USER TENDED TO MAKE TRAVEL PLAN IN THE SECOND QUARTER, LEAST IN THE FORTH QUARTER
ggplot(X, aes(x = quarter_account_created)) +
  geom_line(stat = 'count') +
  labs(x = "Month", y = "Number of Users Created") +
  ggtitle("Time Series of User Account Creation (month)") +
  scale_x_continuous(breaks = 1:12)

#Plot quarter_account_created and country_destination----
## MORE ACCOUNTS WERE CREATED IN SECOND QUARTER(SUMMER) NO MATTER THE DESTINATION
ggplot(X, aes(x = quarter_account_created, fill=country_destination))+
  geom_bar(position="identity", alpha=0.5)+ 
  facet_wrap(~country_destination)

ggplot(X, aes(x = month_account_created, fill=country_destination))+
  geom_bar(position="identity", alpha=0.5)+ 
  facet_wrap(~country_destination)


## INSIGHT: USER WHOSE DESTINATION COUNTRY IS USA, MORE THAN HALF HAS 'unknown' GENDER
ggplot(train, aes(x = gender))+
  geom_bar(position="identity", alpha=0.5)+ 
  facet_wrap(~country_destination)

## INSIGHT: USER WHOSE SIGNUP APP WAS 'WEB' HAS HIGHER BOOKING RATE
ggplot(train, aes(x = country_destination, fill = signup_app)) +
  geom_bar(position = "fill", alpha = 0.5) + 
  labs(x = "Country Destination", y = "Proportion") +
  ggtitle("Signup App by Country Destination") +  
  theme(legend.position = "bottom")

## INSIGHT: USER USING UNKNOWN BROWSER HAD MUCH HIGHER NO BOOKING RATE
df_browser <- df_train[, .N, by = 'first_browser']
setorderv(df_browser, cols = 'N', order = -1)
df_browser <- df_browser[1:6]
df_browser_plot <- df_train[df_train$first_browser %in% df_browser$first_browser,]

ggplot(df_browser_plot, aes(x = country_destination, fill = first_browser)) +
  geom_bar(position = "fill", alpha = 0.5) + 
  labs(x = "Country Destination", y = "Proportion") +
  ggtitle("First Browser (top 6) by Country Destination") +  
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



# Plot Distribution of Primary Devices----
## Create a new data frame with counts of each device
device_counts <- df_primary[, .(count = .N), by = primary_device][order(-count)][1:8]

## Plot
### THE PLOT SHOWS ORDER OF THE PRIMARY DEVICE COUNT (DEVICE THAT USERS USE THE MOST TIME)
### ORDER(HIGH TO LOW): MAC, WINDOWS DESKTOP, IPHONE
ggplot(device_counts, aes(x = reorder(primary_device, -count), y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Primary Device", y = "Count") +
  ggtitle("Distribution of Primary Devices") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Plot relation of country destination and primary device----
df_plot <- df_primary[, .(count = .N), by = primary_device][order(-count)][1:8]
df_primary_filtered <- df_primary[df_primary$primary_device %in% df_plot$primary_device,]

setkey(df_primary_filtered, user_id)
setkey(df_train, id)
## merge data.tables by id (user_id)
df_train_primary <- df_primary_filtered[df_train, nomatch = 0]
## Plot
### Create a new data frame with counts of each combination of country and device
counts <- df_train_primary[, .N, by = c("country_destination", "primary_device")]
### Convert to wide format
counts_wide <- dcast(counts, country_destination ~ primary_device, value.var = "N")
# Reorder the country_destination factor by count in descending order
counts_wide$country_destination <- reorder(counts_wide$country_destination, -rowSums(counts_wide[, -1]))

### Add destination distance information
counts_wide$distance <- ifelse(counts_wide$country_destination == "NDF", "No Booking",
                               ifelse(counts_wide$country_destination %in% c("US", "CA", "MX"), "Domestic",
                                      "International"))

### Melt the data frame to long format
counts_long <- melt(counts_wide, id.vars = c("country_destination", "distance"),
                    variable.name = "primary_device", value.name = "count")
### INSIGHT: USER USING DESKTOP HAS LOWER NO BOOKING RATE
ggplot(counts_long, aes(x = primary_device, y = count, fill = distance)) +
  geom_bar(stat = "identity", position = 'fill') +
  labs(x = "Primary Device", y = "Destination Proportion") +
  ggtitle("Relationship between Primary Device and Country Destination") +  
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))

# Insights for Airbnb users----
## INSIGHT: USER ACCOUNT CREATED HAD AN EXPONENTIAL GROWTH IN 2013-2014
## INSIGHT: USER ACCOUNT CREATED MUCH MORE FROM JAN to JUNE
## INSIGHT: USER TENDED TO MAKE TRAVEL PLAN IN THE SECOND QUARTER, LEAST IN THE FORTH QUARTER
## INSIGHT: USER WHOSE DESTINATION COUNTRY IS USA, MORE THAN HALF HAS 'unknown' GENDER
## INSIGHT: USER WHOSE SIGNUP APP WAS 'WEB' HAS HIGHER BOOKING RATE
## INSIGHT: USER USING UNKNOWN BROWSER HAD MUCH HIGHER NO BOOKING RATE
## INSIGHT: USER USING DESKTOP HAS LOWER NO BOOKING RATE
## INSIGHT: USERS WHOSE PRIMARY DEVICE WAS DESKTOP HAD HIGHER BOOKING RATE AND HIGHER INTERNATIONAL TRAVELING BOOKING RATE
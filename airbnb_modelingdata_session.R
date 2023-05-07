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
df_primary<- df_primary[, c(levels(df_primary$primary_device), "primary_device") := 
            c(lapply(levels(primary_device), function(x) as.integer(x == primary_device)), .(NULL))]
####check
#setorderv(x = df_primary, cols = 'primary_secs', order = -1)
#head(df_primary)

## determine secondary device (second longest secs)
aggregated_lvl2 <- setorderv(x = aggregated_lvl1, cols = c('user_id','total_secs_elapsed'), order = -1)
df_secondary <- aggregated_lvl2[, .SD[2], keyby = "user_id"]
df_secondary <- df_secondary[,.('user_id' = get('user_id'), 'secondary_device'=get('device_type'), 'secondary_secs'=get('total_secs_elapsed'))]
df_secondary<- df_secondary[, c(levels(df_secondary$secondary_device), "secondary_device") := 
                          c(lapply(levels(secondary_device), function(x) as.integer(x == secondary_device)), .(NULL))]
## merge primary and secondary as device data 
### set user_id as key
setkey(df_primary, user_id)
setkey(df_secondary, user_id)
### merge data.tables by user_id
device_data <- merge(df_primary, df_secondary, by = "user_id", all = TRUE)

## merge device and user data
### set id as key
setkey(df_all_combined, id)
setkey(device_data, user_id)
### merge data.tables by id (user_id)
df_user_device <- df_all_combined[device_data, nomatch = 0]

# split train and test
X_ud = df_user_device[df_user_device$id %in% train$id,]
X_ud = X_ud[-grep('country_destination', colnames(X_ud))]

#y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_ud_test = df_user_device[df_user_device$id %in% test$id,]

write.csv(X_ud, "./airbnb-recruiting-new-user-bookings/df_ud_train.csv", row.names=FALSE)
write.csv(X_ud_test, "./airbnb-recruiting-new-user-bookings/df_ud_test.csv", row.names=FALSE)



# Count occurrences of value in a column
session_actions <- sessions[, c("user_id", "action", "action_type", "action_detail")]
columns_to_convert <- c("action", "action_type", "action_detail")
session_actions[is.na(session_actions)] <- "not provided"

convert_to_counts <- function(df, id_col, column_to_convert) {
  id_list <- unique(df[, id_col])
  
  df_counts <- df[, c(id_col, column_to_convert)]
  df_counts$count <- 1
  df_counts <- aggregate(count ~ ., data = df_counts, sum, na.action = na.pass)
  
  new_df <- reshape(df_counts, idvar = id_col, timevar = column_to_convert, direction = "wide", sep = "_")
  new_df[is.na(new_df)] <- 0
  
  # Rename Columns
  categories <- unique(df[, column_to_convert])
  for (category in categories) {
    cat_name <- gsub(" ", "_", gsub("(/|\\(|\\)|-)", "_", tolower(category)))
    col_name <- paste(column_to_convert, cat_name, sep = "_")
    colnames(new_df)[colnames(new_df) == category] <- col_name
  }
  
  return(new_df)
}

# Aggregate and combine actions taken columns
session_actions <- sessions[, c("user_id", "action", "action_type", "action_detail")]
columns_to_convert <- c("action", "action_type", "action_detail")
session_actions[is.na(session_actions)] <- "not provided"
first <- TRUE

for (column in columns_to_convert) {
  current_data <- convert_to_counts(df = session_actions, id_col = 'user_id', column_to_convert = column)
  
  # If first loop, current data becomes existing data, otherwise merge existing and current
  if (first) {
    first <- FALSE
    actions_data <- current_data
  } else {
    actions_data <- merge(actions_data, current_data, by = 'user_id')
  }
}



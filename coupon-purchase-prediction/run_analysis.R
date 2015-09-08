# Load prefecture data
prefecture_data <- read.csv('input/prefecture_locations.csv',
                            colClasses = c('character',
                                           'character',
                                           'numeric',
                                           'numeric'))
# print(head(prefecture_data))
# str(prefecture_data)

# Load user data
user_data <- read.csv('input/user_list.csv',
                      na.strings = c('', 'NA'),
                      colClasses = c('character',
                                     'factor',
                                     'integer',
                                     'character',
                                     'character',
                                     'character'))

# Convert dates to correct format
user_data$REG_DATE <- as.POSIXct(user_data$REG_DATE, tz = 'UTC')
user_data$WITHDRAW_DATE <- as.POSIXct(user_data$WITHDRAW_DATE, tz = 'UTC')

# Save whether the user has deregistered
user_data$IS_WITHDRAWN <- !is.na(user_data$WITHDRAW_DATE)

# Add data on each user's prefecture
user_data <- merge(user_data, prefecture_data, all.x = TRUE)

# Print a summary
print(head(user_data))
str(user_data)

# coupon_classes <- c('character', 'character', 'integer', 'integer', 'integer',
#                     'POSIXct', 'POSIXct', 'integer', 'POSIXct', 'POSIXct',
#                     'integer', 'integer', 'integer', 'integer', 'integer',
#                     'integer', 'integer', 'integer', 'integer', 'character',
#                     'character', 'character', 'character')
# 
# # Load coupon data
# coupon_train_data <- read.csv('input/coupon_list_train.csv',
#                               colClasses = coupon_classes)
# 
# coupon_train_data[, 12:20] <- sapply(coupon_train_data[, 12:20], as.logical)
# 
# # Print a summary
# print(head(coupon_train_data))
# str(coupon_train_data)

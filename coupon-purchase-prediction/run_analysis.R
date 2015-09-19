# Load prefecture data
prefecture_data <- read.csv('input/prefecture_locations.csv',
                            colClasses = c('character',
                                           'character',
                                           'numeric',
                                           'numeric'))
# Load user purchase history data
purchase_history_data <- read.csv('input/coupon_detail_train.csv',
                                  colClasses = c('numeric',
                                                 'character',
                                                 'character',
                                                 'character',
                                                 'character',
                                                 'character'))

# Convert dates to correct format
purchase_history_data$I_DATE <- as.POSIXct(purchase_history_data$I_DATE, tz = 'UTC')

# Merge purchases for each user
#user_purchase_history <- aggregate(COUPON_ID_hash ~ USER_ID_hash, data = purchase_history_data, FUN = function(x) paste(x, collapse = ','))
#names(user_purchase_history) <- c('USER_ID_hash', 'PURCHASE_HISTORY_COUPON_ID_hash')

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

# Add data on each user's purchase history
user_purchase_data <- merge(user_data, purchase_history_data)
#user_data$COUPONS_PURCHASED <- round(nchar(user_data$PURCHASE_HISTORY_COUPON_ID_hash) / 33)

# Print a summary
#print(head(user_purchase_data))
#str(user_purchase_data)

# Find the most commonly purchased coupon for each prefecture
prefecture_purchase_data <- aggregate(COUPON_ID_hash ~ PREF_NAME, user_purchase_data, FUN = function(x) names(which.max(table(x))))

# Print it and summarise it in a table
print(prefecture_purchase_data)
print(table(prefecture_purchase_data$COUPON_ID_hash))

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


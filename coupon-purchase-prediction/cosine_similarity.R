### Kaggle Scripts: Ponpare Coupon Purchase Prediction ###
### Author: Subhajit Mandal ###

require(Matrix)

sigmoid <- function(x) {
    return (1 / (1 + exp(-x)))
}

getWeightMatrix <- function(x) {
    #Weight Matrix: GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ large_area_name ken_name small_area_name BIAS
    return (as.matrix(Diagonal(x = x)))
}

getHyp <- function(weights, user_prefs, coupons) {
    weight_matrix <- getWeightMatrix(weights)
    hyp <- as.matrix(user_prefs[,2:ncol(user_prefs)]) %*% weight_matrix %*% t(as.matrix(coupons[,2:ncol(coupons)]))
    hyp <- sigmoid(hyp)
    
    return (hyp)
}

getCost <- function(y, weights, user_prefs, coupons) {
    hyp <- getHyp(weights, user_prefs, coupons)
    
    #return (sum(0.5 * (hyp - y)**2) / length(y))
    
    return ((-1 / length(y)) * (sum(y * log(hyp) + (1 - y) * log(1 - hyp))))
}

saveSubmission <- function(weights, user_prefs, coupons, full_user_list) {
    hyp <- getHyp(weights, user_prefs, coupons)
    
    #order the list of coupons according to similairties and take only first 10 coupons
    user_prefs$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(user_prefs),FUN=function(i){
        purchased_cp <- paste(coupons$COUPON_ID_hash[order(hyp[i,], decreasing = TRUE)][1:10],collapse=" ")
        return(purchased_cp)
    }))

    #make submission
    submission <- merge(full_user_list, user_prefs, all.x=TRUE)
    submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
    write.csv(submission, file="cosine_sim.csv", row.names=FALSE)
}

debug <- FALSE
weight_vector <- scan('weight_vector.txt')

#read in all the input data
coupon_detail_train <- read.csv("input/coupon_detail_train.csv")
coupon_list_train <- read.csv("input/coupon_list_train.csv")
coupon_list_test <- read.csv("input/coupon_list_test.csv")
user_list <- read.csv("input/user_list.csv")

if (debug) {
    user_list <- user_list[1:100, ]
    coupon_detail_train <- coupon_detail_train[coupon_detail_train$USER_ID_hash %in% user_list$USER_ID_hash, ]
    coupon_list_train <- coupon_list_train[coupon_list_train$COUPON_ID_hash %in% coupon_detail_train$COUPON_ID_hash, ]
    coupon_list_test <- coupon_list_test[1:100, ]
}

#making of the training set
training_coupons <- merge(coupon_detail_train,coupon_list_train)
training_coupons <- training_coupons[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name")]
#combine the test set with the training set
coupon_list_test$USER_ID_hash <- "dummyuser"
testing_coupons <- coupon_list_test[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name","small_area_name")]

training_coupons <- rbind(training_coupons,testing_coupons)
#NA imputation
training_coupons[is.na(training_coupons)] <- 1
#feature engineering
training_coupons$DISCOUNT_PRICE <- 1/log10(training_coupons$DISCOUNT_PRICE)
training_coupons$PRICE_RATE <- (training_coupons$PRICE_RATE*training_coupons$PRICE_RATE)/(100*100)
#convert the factors to columns of 0's and 1's
training_coupons <- cbind(training_coupons[,c(1,2)],model.matrix(~ -1 + .,training_coupons[,-c(1,2)],
                                                    contrasts.arg=lapply(training_coupons[,names(which(sapply(training_coupons[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#add bias node
training_coupons$BIAS <- 1

#separate the test from training_coupons
testing_coupons <- training_coupons[training_coupons$USER_ID_hash=="dummyuser",]
testing_coupons <- testing_coupons[,-2]
training_coupons <- training_coupons[training_coupons$USER_ID_hash!="dummyuser",]

#y <- matrix(nrow = nrow(user_list), ncol = nrow(coupon_list_train))
#for (i in 1:nrow(coupon_list_train)) {
#    coupon <- as.character(sort(coupon_list_train$COUPON_ID_hash)[i])
#    y[, i] <- as.numeric(sort(user_list$USER_ID_hash) %in% training_coupons[training_coupons$COUPON_ID_hash == coupon, 'USER_ID_hash'])
#}

#data frame of user characteristics
user_ideal_coupon <- aggregate(.~USER_ID_hash, data=training_coupons[,-1],FUN=mean)
user_ideal_coupon <- merge(data.frame(USER_ID_hash = sort(user_list$USER_ID_hash)), user_ideal_coupon, all.x = TRUE)
user_ideal_coupon[is.na(user_ideal_coupon)] <- 0
user_ideal_coupon$DISCOUNT_PRICE <- 1
user_ideal_coupon$PRICE_RATE <- 1
#training_coupons <- aggregate(.~COUPON_ID_hash, data=training_coupons[,-2],FUN=mean)

#calculation of cosine similarities of users and coupons
#test_scores <- getHyp(weight_vector, user_ideal_coupon, testing_coupons)
#train_scores <- getHyp(weight_vector, user_ideal_coupon, training_coupons)

saveSubmission(weight_vector, user_ideal_coupon, testing_coupons, user_list)

### Kaggle Scripts: Ponpare Coupon Purchase Prediction ###
### Author: Subhajit Mandal ###

#read in all the input data
coupon_detail_train <- read.csv("input/coupon_detail_train.csv")
coupon_list_train <- read.csv("input/coupon_list_train.csv")
coupon_list_test <- read.csv("input/coupon_list_test.csv")
user_list <- read.csv("input/user_list.csv")

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

#separate the test from training_coupons
testing_coupons <- training_coupons[training_coupons$USER_ID_hash=="dummyuser",]
testing_coupons <- testing_coupons[,-2]
training_coupons <- training_coupons[training_coupons$USER_ID_hash!="dummyuser",]

#data frame of user characteristics
user_ideal_coupon <- aggregate(.~USER_ID_hash, data=training_coupons[,-1],FUN=mean)
user_ideal_coupon$DISCOUNT_PRICE <- 1
user_ideal_coupon$PRICE_RATE <- 1

#Weight Matrix: GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ large_area_name ken_name small_area_name
require(Matrix)
W <- as.matrix(Diagonal(x=c(rep(2.05,13), rep(2.01,1), rep(-0.13,1), rep(0,9), rep(0.51,9), rep(1.01,47), rep(4.85,55))))

#calculation of cosine similairties of users and coupons
score = as.matrix(user_ideal_coupon[,2:ncol(user_ideal_coupon)]) %*% W %*% t(as.matrix(testing_coupons[,2:ncol(testing_coupons)]))
#order the list of coupons according to similairties and take only first 10 coupons
user_ideal_coupon$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(user_ideal_coupon),FUN=function(i){
  purchased_cp <- paste(testing_coupons$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))

#make submission
submission <- merge(user_list, user_ideal_coupon, all.x=TRUE)
submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="cosine_sim.csv", row.names=FALSE)

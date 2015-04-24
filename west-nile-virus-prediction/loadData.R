trainRaw <- read.csv("data/train.csv",
                     colClasses = c("Date",
                                    "NULL",#character",
                                    "factor",
                                    "NULL",#"character",
                                    "NULL",#"character",
                                    "character",
                                    "NULL",#"character",
                                    "numeric",
                                    "numeric",
                                    "integer",
                                    "integer",
                                    "integer"))

# Ignore satellite traps, whose trap names are ended with a letter
trainRaw <- subset(trainRaw, nchar(Trap) == 4)

# Aggregate observations on the same day with the same trap
train <- aggregate(NumMosquitos ~ Date + Species + Trap + Latitude + Longitude + AddressAccuracy,
                   FUN = "sum",
                   data = trainRaw)
trainSup <- aggregate(WnvPresent ~ Date + Species + Trap,
                      FUN = "sum",
                      data = trainRaw)
# and save WnvPresent as a bool
train$WnvPresent <- as.logical(trainSup$WnvPresent)
# then tidy up
rm(trainRaw)
rm(trainSup)

# Save day of year (0-366)
train$DayOfYear <- as.integer(format(train$Date, "%j"))


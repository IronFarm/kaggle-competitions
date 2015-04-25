meanNormalise <- function(X, col) {
  data <- X[, col]
  
  data <- (data - mean(data))/sd(data)
  
  X[, col] <- data
  return(X)
}

loadData <- function() {
  trainClasses = c("Date",
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
                   "integer")
  
  # Read in CSV
  input <- read.csv("data/train.csv",
                    colClasses = trainClasses)
  
  # Ignore satellite traps, whose trap names are ended with a letter
  input <- subset(input, nchar(Trap) == 4)
  
  # Perform mean normalisation
  input <- meanNormalise(input, 4)
  input <- meanNormalise(input, 5)
  
  # Save day of year (0-366)
  input$DayOfYear <- as.integer(format(input$Date, "%j"))
  
  # Load weather data
  weatherData <- loadWeatherData()
  
  # Merge in weather data from station 1 (Chicago O'Hare)
  input <- merge(data,
                 weatherData[weatherData$Station == 1, ],
                 all.x = TRUE)
  
  input <- na.omit(input)
  
  return(input)
}

loadWeatherData <- function() {
  weatherData <- read.csv("data/weather.csv",
                          stringsAsFactors = FALSE,
                          na.strings = c("M", "  T"))
  weatherData <- transform(weatherData,
                           Station = as.factor(Station),
                           Date = as.Date(Date))
  
  # Reorder and save temp. and precip. data
  weatherData <- weatherData[, c(2, 1, 4, 3, 5, 17)]
}

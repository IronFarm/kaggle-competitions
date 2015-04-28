meanNormalise <- function(data) {

  data <- (data - mean(data))/sd(data)

  return(data)
}

movingAverage <- function(data, days = 7) {
  # Do not use with days > 25!
  output <- rep(0, length(data))
  
  for (i in days:length(data)) {
    output[i] <- mean(data[(i - days):i],
                      na.rm = TRUE)
  }
  
  return(output)
}

slidingSum <- function(data, days = 7) {
  # Do not use with days > 25!
  output <- rep(0, length(data))
  
  for (i in days:length(data)) {
    output[i] <- sum(data[(i - days):i],
                     na.rm = TRUE)
  }
  
  return(output)
}

loadData <- function(filename) {
  # Read in CSV
  input <- read.csv(filename,
                    stringsAsFactors = FALSE)
  input <- transform(input,
                     Date = as.Date(Date),
                     Species = as.factor(Species))
  
  # Ignore satellite traps, whose trap names are ended with a letter
  # input <- subset(input, nchar(Trap) == 4)
  
  # Save day and month of year
  input$DayOfYear <- as.integer(format(input$Date, "%j"))
  input$MonthOfYear <- as.integer(format(input$Date, "%m"))
  
  # Load weather data
  weatherData <- loadWeatherData()
  
  # Merge in weather data
  input <- merge(input,
                 weatherData,
                 all.x = TRUE)
  #input <- na.omit(input)
  
  # Perform mean normalisation where required
  input$Latitude <- meanNormalise(input$Latitude)
  input$Longitude <- meanNormalise(input$Longitude)
  input$DayOfYear <- meanNormalise(input$DayOfYear)
  input$MonthOfYear <- meanNormalise(input$MonthOfYear)
  input$Tmin <- meanNormalise(input$Tmin)
  input$Tmax <- meanNormalise(input$Tmax)
  input$Tavg <- meanNormalise(input$Tavg)
  input$SevenDayMeanTavg <- meanNormalise(input$SevenDayMeanTavg)
  input$Block <- meanNormalise(input$Block)

  return(input)
}

loadWeatherData <- function() {
  weatherData <- read.csv("data/weather.csv",
                          stringsAsFactors = FALSE,
                          na.strings = c("M", "  T"))
  weatherData <- transform(weatherData,
                           Station = as.factor(Station),
                           Date = as.Date(Date))
  
  # Use Chicago O'Hare data only for now to simplify moving average calculation
  weatherData <- subset(weatherData, Station == 1)
  
  # Add moving average for Tavg
  weatherData$SevenDayMeanTavg <- movingAverage(weatherData$Tavg, 7)
  # and sliding sum for PrecipTotal
  weatherData$SevenDaySumPrecipTotal <- slidingSum(weatherData$PrecipTotal, 7)

  # Set NAs to zero
  weatherData$PrecipTotal[is.na(weatherData$PrecipTotal)] <- 0

  # Reorder and save temp. and precip. data
  weatherData <- weatherData[, c(2, 1, 4, 3, 5, 17, 23, 24)]
}

haversine <- function(theta) {
  return((sin(theta / 2))^2)
}

getSprayDensity <- function(latitude, longitude, sprayData) {
  res <- 2 * 6371 * asin(sqrt(haversine(latitude - sprayData$Latitude) + cos(latitude) * cos(sprayData$Latitude) * haversine(longitude - sprayData$Longitude)))

  res <- sum(exp(-res / 2))

  return(res)
}

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

loadData <- function(filename = "data/train.csv") {
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

  # Load spray data
  sprayData <- loadSprayData()

  # Convert latitudes and longitudes to radians
  input <- transform(input,
                     Latitude = Latitude * pi / 180,
                     Longitude = Longitude * pi / 180)
  sprayData <- transform(sprayData,
                         Latitude = Latitude * pi / 180,
                         Longitude = Longitude * pi / 180)

  # Copy trap data
  trapData <- unique(input[, c("Latitude", "Longitude")])
  trapData$SprayDensity <- rep(0, nrow(trapData))

  # Calculate density of spraying for each trap
  for (i in 1:nrow(trapData)) {
    trapData$SprayDensity[i] <- getSprayDensity(trapData$Latitude[i], trapData$Longitude[i], sprayData)
  }
  trapData$SprayDensity <- trapData$SprayDensity / max(trapData$SprayDensity)

  # and merge it into the input data
  input <- merge(input,
                 trapData,
                 all.x = TRUE)
  
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

loadSprayData <- function() {
  sprayData <- read.csv("data/spray.csv",
                        stringsAsFactors = FALSE)

  sprayData <- transform(sprayData,
                         Date = as.Date(Date))
}


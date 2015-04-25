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
  
  # Save WNV presence as a bool
  #input$WnvPresent <- as.logical(input$WnvPresent)
  
  # Perform mean normalisation
  input <- meanNormalise(input, 4)
  input <- meanNormalise(input, 5)
  
  # Save day of year (0-366)
  input$DayOfYear <- as.integer(format(input$Date, "%j"))
  
  return(input)
}


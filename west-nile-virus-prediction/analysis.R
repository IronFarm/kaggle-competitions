source("loadData.R")
source("trainNN.R")
source("utility.R")

splitSample <- function(X, y, split = 0.8) {
  mTot <- nrow(X)
  mSplit <- round(split * mTot)
  
  idxShuffled <- sample(mTot)
  
  idxTrain <- idxShuffled[1:mSplit]
  idxCv <- idxShuffled[(mSplit + 1):mTot]
  
  XTrain <- X[idxTrain, ]
  yTrain <- y[idxTrain, ]
  
  XCv <- X[idxCv, ]
  yCv <- y[idxCv, ]
  
  return(list(XTrain, yTrain, XCv, yCv))
}

calculateLearningCurve <- function(XTrain, yTrain, XCv, yCv) {
  # Make learning curves, using 1000-8000 training examples
  # Data is already randomised
  trainingExamples <- seq(1000, 8000, 500)

  learningCurve <- data.frame(TrainingExamples = numeric(length(trainingExamples)),
                              costCv = numeric(length(trainingExamples)),
                              costTrain = numeric(length(trainingExamples)))

  for (i in 1:length(trainingExamples)) {
    res <- trainNN(XTrain[1:trainingExamples[i], ],
                   yTrain[1:trainingExamples[i]],
                   lambda = 0.0)
    theta1 <- res[[1]]; theta2 <- res[[2]];
    
    costTrain <- getCost(XTrain[1:trainingExamples[i], ],
                         yTrain[1:trainingExamples[i]],
                         theta1,
                         theta2)[[1]]
    costCv <- getCost(XCv,
                      yCv,
                      theta1,
                      theta2)[[1]]
    
    learningCurve[i, ] <- c(trainingExamples[i], costCv, costTrain)
    print(learningCurve[i, ])
  }
  
  return(learningCurve)
}

calculatePerformanceMetrics <- function(X, y, theta1, theta2, nThresholds = 100) {
  m <- nrow(X)

  hyp <- getCost(X,
                 y,
                 theta1,
                 theta2)[[3]]

  # Find threshold which maximises F1 score
  perfData <- data.frame(threshold = seq(0, 1, 1 / nThresholds),
                         f1 = numeric(nThresholds + 1),
                         TPR = numeric(nThresholds + 1),
                         FPR = numeric(nThresholds + 1))

  for (i in 1:nrow(perfData)) {
    decision <- hyp > perfData$threshold[i]
    
    tp <- sum(decision == 1 & y == 1)
    fp <- sum(decision == 1 & y == 0)
    fn <- sum(decision == 0 & y == 1)
    tn <- sum(decision == 0 & y == 0)
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    perfData$TPR[i] <- recall
    perfData$FPR[i] <- fp / (fp + tn)
    perfData$f1[i] <- 2 * precision * recall / (precision + recall)
  }

  AUROC <- 0
  for (i in 1:nThresholds) {
    AUROC <- AUROC + ((perfData$TPR[i] + perfData$TPR[i + 1]) / 2) * (perfData$FPR[i] - perfData$FPR[i + 1])
  }
  
  cat("Max. F1 score: ", max(perfData$f1, na.rm = TRUE), "\n")
  cat("AUROC score: ", AUROC, "\n")
  
  return(perfData)
}

saveSubmissionFile <- function(theta1, theta2) {
  input <- loadData("data/test.csv")
  
  X <- cbind(input$DayOfYear,
             #input$MonthOfYear,
             input$Latitude,
             input$Longitude,
             input$Tmin,
             input$Tmax,
             input$Tavg,
             input$PrecipTotal,
             input$SevenDayMeanTavg,
             input$SevenDaySumPrecipTotal,
             #input$Block,
             input$Species,
             input$SprayDensity)

  y <- rep(0, nrow(X))
  
  hyp <- getCost(X, y, theta1, theta2)[[3]]

  output <- data.frame(Id = input$Id, WnvPresent = hyp)
  
  output <- merge(data.frame(Id = 1:116293), output, all.x = TRUE)
  output[is.na(output[, 2]), 2] <- 0
  
  write.csv(output, "submission.csv", row.names = FALSE, quote = FALSE)  
  
  return(output)
}

input <- loadData("data/train.csv")

# Copy variables used to train NN
X <- cbind(input$DayOfYear,
           #input$MonthOfYear,
           input$Latitude,
           input$Longitude,
           input$Tmin,
           input$Tmax,
           input$Tavg,
           input$PrecipTotal,
           input$SevenDayMeanTavg,
           input$SevenDaySumPrecipTotal,
           #input$Block,
           input$Species,
           input$SprayDensity)

# y contains 0 for no WNV and 1 for WNV present
y <- matrix(input$WnvPresent)

res <- splitSample(X, y)
XTrain <- res[[1]]; yTrain <- res[[2]]; XCv <- res[[3]]; yCv <- res[[4]];

mTrain <- nrow(XTrain)
mCv <- nrow(XCv)
nVar <- ncol(XTrain)

res <- trainNN(XTrain,
               yTrain,
               lambda = 0.0)
theta1 <- res[[1]]; theta2 <- res[[2]];

# learningCurve <- calculateLearningCurve(XTrain,
#                                         yTrain,
#                                         XCv,
#                                         yCv)

perfData <- calculatePerformanceMetrics(XCv,
                                        yCv,
                                        theta1,
                                        theta2)

hyp <- getCost(XTrain, yTrain, theta1, theta2)[[3]]

# saveSubmissionFile(theta1, theta2)


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

getCost <- function(X, y, theta1, theta2) {
  m <- nrow(X)
  
  # Add bias node
  X <- cbind(rep(1, m),
             X)
  
  z2 <- X %*% t(theta1)
  a2 <- cbind(rep(1, m),
              sigmoid(z2))
  
  # Calculate output layer
  z3 <- a2 %*% t(theta2)
  hyp <- sigmoid(z3)
  
  J <- sum(-y * log(hyp) - (1 - y) * log(1 - hyp)) / m
  
  return(c(J, hyp))
}

calculateLearningCurve <- function(XTrain, yTrain, XCv, yCv) {
  # Make learning curves, using 200-8000 training examples
  # Data is already randomised
  learningCurve <- matrix(nrow = 40, ncol = 3)
  for (i in 1:40) {
    trainingExamples <- 200 * i
    
    res <- trainNN(XTrain[1:trainingExamples, ],
                   yTrain[1:trainingExamples])
    theta1 <- res[[1]]; theta2 <- res[[2]]; costHist <- res[[3]];
    
    costTrain <- getCost(XTrain[1:trainingExamples, ],
                         yTrain[1:trainingExamples],
                         theta1,
                         theta2)[1]
    costCv <- getCost(XCv,
                      yCv,
                      theta1,
                      theta2)[1]
    
    learningCurve[i, ] <- c(trainingExamples, costTrain, costCv)
    print(learningCurve[i, ])
  }
  
  return(learningCurve)
}

input <- loadData("data/train.csv")

# Copy variables used to train NN
X <- cbind(input$DayOfYear,
           input$Latitude,
           input$Longitude,
           input$Tmin,
           input$Tmax,
           input$Tavg,
           input$PrecipTotal)

# y contains 0 for no WNV and 1 for WNV present
y <- matrix(input$WnvPresent)

res <- splitSample(X, y)
XTrain <- res[[1]]; yTrain <- res[[2]]; XCv <- res[[3]]; yCv <- res[[4]];

mTrain <- nrow(XTrain)
mCv <- nrow(XCv)
nVar <- ncol(XTrain)

res <- trainNN(XTrain,
               yTrain)
theta1 <- res[[1]]; theta2 <- res[[2]]; costHist <- res[[3]];

# learningCurve <- calculateLearningCurve(XTrain,
#                                         yTrain,
#                                         XCv,
#                                         yCv)

# Find threshold which maximises F1 score
nThresholds <- 1000
f1Hist <- matrix(nrow = nThresholds, ncol = 2)
for (i in 1:nThresholds) {
  threshold <- i / nThresholds
  
  hyp <- getCost(XCv,
                 yCv,
                 theta1,
                 theta2)[2]
  decision <- hyp > threshold
  
  tp <- sum(decision == 1 & yCv == 1)
  fp <- sum(decision == 1 & yCv == 0)
  fn <- sum(decision == 0 & yCv == 1)
  tn <- sum(decision == 0 & yCv == 0)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  f1 <- 2 * precision * recall / (precision + recall)
  f1Hist[i, ] <- c(i, f1)
}
cat("Max. F1 score: ", max(f1Hist[, 2], na.rm = TRUE))

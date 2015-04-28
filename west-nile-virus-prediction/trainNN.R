source("utility.R")

getCost <- function(X, y, theta1, theta2, lambda = 0) {
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
  J <- J + (lambda / (2 * m)) * (sum(theta1^2) + sum(theta2^2))
  
  return(list(J, hyp))
}

runGradientChecking <- function(X, y, theta1, theta2, epsilon = 0.001) {
  theta1Grad <- matrix(rep(0, length(theta1)),
                       nrow = nrow(theta1),
                       ncol = ncol(theta1))
  
  for (i in 1:length(theta1)) {
    theta1Up <- theta1
    theta1Dn <- theta1
    
    theta1Up[i] <- theta1Up[i] + epsilon
    theta1Dn[i] <- theta1Dn[i] - epsilon
    
    theta1Grad[i] <- (getCost(X, y, theta1Up, theta2)[[1]] - getCost(X, y, theta1Dn, theta2)[[1]]) / (2 * epsilon)
  }

  theta2Grad <- matrix(rep(0, length(theta2)),
                       nrow = nrow(theta2),
                       ncol = ncol(theta2))
  
  for (i in 1:length(theta2)) {
    theta2Up <- theta2
    theta2Dn <- theta2
    
    theta2Up[i] <- theta2Up[i] + epsilon
    theta2Dn[i] <- theta2Dn[i] - epsilon
    
    theta2Grad[i] <- (getCost(X, y, theta1, theta2Up)[[1]] - getCost(X, y, theta1, theta2Dn)[[1]]) / (2 * epsilon)
  }
  
  return(list(theta1Grad, theta2Grad))
}

trainNN <- function(X, y, learningRate = 5, nIter = 100, lambda = 0, theta1 = NA, theta2 = NA) {
  m <- nrow(X)
  nVar <- ncol(X)
  
  # Add bias node
  X <- cbind(rep(1, m),
             X)
  
  # Randomly initialise weights if not provided
  if (is.na(theta1)) {
    theta1 <- matrix(runif(nVar * (nVar + 1)) - 0.5,
                     nrow = nVar,
                     ncol = nVar + 1)
  }
  if (is.na(theta2)) {
    theta2 <- matrix(runif(nVar + 1) - 0.5,
                     nrow = 1,
                     ncol = nVar + 1)
  }
  
  # Gradient descent
  costHist <- matrix(nrow = nIter, ncol = 2)
  for (i in 1:nIter) {
    # Calculate hidden layer
    z2 <- X %*% t(theta1)
    a2 <- cbind(rep(1, m),
                sigmoid(z2))
    
    # Calculate output layer
    z3 <- a2 %*% t(theta2)
    hyp <- sigmoid(z3)
    
    # Calculate and save cost
    J <- sum(-y * log(hyp) - (1 - y) * log(1 - hyp)) / m
    J <- J + (lambda / (2 * m)) * (sum(theta1^2) + sum(theta2^2))
    costHist[i, ] <- c(i, J)
    if (i %% 10 == 0) {
      cat("i = ", i, "\tJ = ", J, "\n")
    }
    
    # Backpropagation
    delta3 <- hyp - y
    delta2 <- (delta3 %*% theta2)[, 2:(nVar + 1)] * sigmoidGradient(X %*% t(theta1))
    
    grad2 <- (t(delta3) %*% a2) / m
    grad1 <- (t(delta2) %*% X) / m
    
    # Regularisation
    grad2[, 2:ncol(grad2)] <- grad2[, 2:ncol(grad2)] + (lambda / m) * theta2[, 2:ncol(grad2)]
    grad1[, 2:ncol(grad1)] <- grad1[, 2:ncol(grad1)] + (lambda / m) * theta1[, 2:ncol(grad1)]
    
    # and gradient descent
    theta1 <- theta1 - grad1 * learningRate
    theta2 <- theta2 - grad2 * learningRate
  
    # gradCheck <- runGradientChecking(X[, 2:ncol(X)],
    #                                  y,
    #                                  theta1,
    #                                  theta2)
  
    # print(grad1); print(gradCheck[[1]]);
    # print(grad2); print(gradCheck[[2]]);
  }
  
  return(list(theta1, theta2, costHist))
}

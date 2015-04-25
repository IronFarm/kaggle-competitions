source("utility.R")

trainNN <- function(X, y, lambda = 0.1, nIter = 100) {
  m <- nrow(X)
  nVar <- ncol(X)
  
  # Add bias node
  X <- cbind(rep(1, m),
             X)
  
  # Randomly initialise weights
  theta1 <- matrix(rnorm(nVar * (nVar + 1)),
                   nrow = nVar,
                   ncol = nVar + 1)
  theta2 <- matrix(rnorm(nVar + 1),
                   nrow = 1,
                   ncol = nVar + 1)
  
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
    costHist[i, ] <- c(i, J)
    
    # Backpropagation
    delta3 <- hyp - y
    delta2 <- (delta3 %*% theta2)[, 2:(nVar + 1)] * sigmoidGradient(X %*% t(theta1))
    
    grad2 <- (t(delta3) %*% a2) / m
    grad1 <- (t(delta2) %*% X) / m
    
    # and gradient descent
    theta1 <- theta1 - grad1 * lambda
    theta2 <- theta2 - grad2 * lambda
  }
  
  return(list(theta1, theta2, costHist))
}

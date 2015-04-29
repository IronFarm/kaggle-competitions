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
  
  # Calculate cost
  J <- sum(-y * log(hyp) - (1 - y) * log(1 - hyp)) / m
  # and regulate
  J <- J + (lambda / (2 * m)) * (sum(theta1^2) + sum(theta2^2))
  
  # Gradient calculation using backpropagation
  delta3 <- hyp - y
  delta2 <- (delta3 %*% theta2)[, 2:(nVar + 1)] * sigmoidGradient(X %*% t(theta1))
  
  grad2 <- (t(delta3) %*% a2) / m
  grad1 <- (t(delta2) %*% X) / m
  
  # Regularise the gradient
  grad2[, 2:ncol(grad2)] <- grad2[, 2:ncol(grad2)] + (lambda / m) * theta2[, 2:ncol(grad2)]
  grad1[, 2:ncol(grad1)] <- grad1[, 2:ncol(grad1)] + (lambda / m) * theta1[, 2:ncol(grad1)]

  grad <- c(grad1[1:length(grad1)], grad2[1:length(grad2)])

  return(list(J, grad, hyp))
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

f <- function(parameters, X, y, lambda = 0) {
  nVar <- ncol(X)

  # Reshape parameters into theta matrices
  theta1 <- matrix(parameters[1:(nVar * (nVar + 1))],
                   nrow = nVar,
                   ncol = nVar + 1)
  theta2 <- matrix(parameters[(nVar * (nVar + 1) + 1):length(parameters)],
                   nrow = 1,
                   ncol = nVar + 1)

  # Get cost and gradient
  ret <- getCost(X, y, theta1, theta2, lambda)

  res <- ret[[1]]
  attr(res, "gradient") <- ret[[2]]

  return(res)
}

trainNN <- function(X, y, lambda = 0) {
  m <- nrow(X)
  nVar <- ncol(X)

  minCost <- 1

  cat("Training")
  
  for (i in 1:10) {
    cat(" ", i)
    # Randomly initialise weights
    theta1 <- runif(nVar * (nVar + 1)) - 0.5
    theta2 <- runif(nVar + 1) - 0.5

    # Minimise cost using nlm()
    ret <- nlm(f, c(theta1, theta2), X = X, y = y, lambda = lambda)

    if (ret[[1]] < minCost) {
      minCost <- ret[[1]]
      parameters <- ret[[2]]
    }
  }
  cat("\n")

  # Reshape parameters into theta matrices
  theta1 <- matrix(parameters[1:(nVar * (nVar + 1))],
                   nrow = nVar,
                   ncol = nVar + 1)
  theta2 <- matrix(parameters[(nVar * (nVar + 1) + 1):length(parameters)],
                   nrow = 1,
                   ncol = nVar + 1)
  
  return(list(theta1, theta2))
}

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

sigmoidGradient <- function(z) {
  sigmoid(z) * (1 - sigmoid(z))
}


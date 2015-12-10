ComputeArgmaxInRows <- function(input.matrix) {
  m <- dim(input.matrix)[1]
  n <- dim(input.matrix)[2]
  argmax <- rep(0, m)
  for (i in 1:m) {
    current.max <- -Inf
    for (j in 1:n) {
      if (input.matrix[i, j] > current.max) {
        current.max <- input.matrix[i, j]
        argmax[i] <- j
      }
    }
  }
  argmax
}

sigmoid <- function(z) {
  1/(1 + exp(-z))
}

learnModel <- function(data, labels) {
  data <- cbind(data / 256, 1)
  objects.number <- dim(data)[1]
  features.number <- dim(data)[2]
  classifier <- matrix(, nrow = features.number, ncol = 10)
  mu <- 0.5
  kLambda <- 0.05
  
  # fit theta for each class against all others
  for (class.num in 0:9) {
    theta <- matrix(rep(0, features.number), nrow = features.number, ncol = 1)
    while (T) {
      gradient <- t(t(sigmoid(data %*% theta) - (class.num == labels)) %*% data) / objects.number
      gradient <- gradient + 2 * kLambda * theta
      gradient[features.number] <- gradient[features.number] - 2 * kLambda * theta[features.number]
      theta <- theta - mu * gradient
      if (sum(abs(gradient)) < 0.1) {
        break
      }
    }
    classifier[, class.num + 1] <- theta
  }
  classifier
}

testModel <- function(classifier, data) {
  ComputeArgmaxInRows(sigmoid(cbind(data / 256, 1) %*% classifier)) - 1
}

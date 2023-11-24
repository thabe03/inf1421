perceptron <- function (x, y, lr) {
  poids <- c(0.5 , 0 , 0 , 0)
  for (j in 1:length (y)) {
    z <- 0
    for (i in 1:3) {
      z <- z + poids [i + 1] * as.numeric (x[i, j])
    }
    z <- z + poids [1]
    if (z <= 0) {
      ypred <- 0
    } else {
      ypred <- 1
    }
    for (i in 1:3) {
      poids [i + 1] <-
        poids [i + 1] + lr * (y[j] - ypred) * as.numeric (x[i, j])
    }
    poids [1] <- poids [1] + lr * (y[j] - ypred)
    print (poids)
  }
}
x <-
  matrix (c(3, 2, 1, 1 , 1 , 1, 1 , 2 , 3) , ncol = 3, nrow = 3)
y <- c(0 , 1 , 1)

err <- perceptron (x, y , 0.1)
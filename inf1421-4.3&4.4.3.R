D <- c(4, 5, 5, 6, 12, 14, 15, 15, 16, 17) # vecteur de point
y <- c(1, 1, 2, 1, 1, 1, 1, 2, 1, 1)
dev.new()
plot(
  D,
  y,
  ylim = c(0, 10),
  pch = 16,
  cex = 2,
  yaxt = 'n',
  ylab = 'P(x)',
  col = "red",
  main = "Representation des points"
)
grid()

densite <- function(Points, x, h) {
  densite <- 0
  Tempo <- 0
  for (i in 1:length(Points))
  {
    if ((abs(x - Points[i]) / h) > 0.5)
    {
      Tempo <- Tempo + 0
    }
    else{
      Tempo <- Tempo + 1
    }
  }
  densite <- (1 / (length(Points) * h ^ 1)) * Tempo
}

den <- densite(D, 3, 4)
print(den)

den <- densite(D, 10, 4)
print(den)

den <- densite(D, 15, 4)
print(den)

# 4.4.3
library(readxl)
iris <- read_excel('iris.xlsx', sheet = 1)

# COLORS = c("red", "blue", "green")
# plot(
#   x = iris$Sepal.Length,
#   y = iris$Petal.Length,
#   col = COLORS[suppressWarnings(as.numeric(iris$Species))],
#   pch = 19,
#   cex = 1.5,
#   main = "Representation des ’Sepal.Length’ en fonction des ’Petal.Length’",
#   cex.main = 1
# )
# 
# grid()
# legend(
#   "topleft",
#   pch = 19,
#   col = COLORS,
#   legend= levels(iris$Species) #legend null ???
# )

# 4.4.4
library('caTools')
set.seed(147)
sample_iris <- sample.split(iris$Species, SplitRatio = 0.80) # ensemble d'entraînement 80% et test 20%
iris_train <- subset(iris, sample_iris == T)
iris_test <- subset(iris, sample_iris == F)

library(class)
knn.3<-knn(iris_train[,c(1,3)],iris_test[,c(1,3)],iris_train$Species,k=7)
print(100*sum(iris_test$Species==knn.3)/length(iris_test$
Species))
print(t(table(knn.3,iris_test$Species)))

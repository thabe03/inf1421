dev.new()
plot(
  function(x)
    dnorm(x, 0, 1),
  -5,
  5,
  col = "red",
  main = "Représentation graphique de p(x|w1) et p(x|w2)",
  xlab = "",
  ylab = "",
  cex.lab = 0.8,
  cex.axis = 0.8,
  cex.main = 0.8,
  cex.sub = 0.8,
  lwd = 2
)
curve(
  dnorm(x, 1, 1),
  add = TRUE,
  col = "blue",
  lty = 2,
  lwd = 2
)
grid()
legend(
  "topleft",
  legend = c("p(x|w1)", "p(x|w2)"),
  col = c("red", "blue"),
  lty = 1:2,
  cex = 0.8,
  lwd = 2
)

library(readxl)
iris <- read_excel("data.xlsx", sheet = 1)
print(summary(iris))
print(table(iris$Species))

library('caTools')
set.seed(6) # pour garder un nombre aléatoire avec sample.split
sample_iris <- sample.split(iris$Species, SplitRatio = 0.80) # ensemble d'entraînement 80% et test 20%
iris_train <- subset(iris, sample_iris == T)
iris_test <- subset(iris, sample_iris == F)

library('e1071')
nb_model_iris <- naiveBayes(Species ~ ., data = iris_train)
print(summary(nb_model_iris))

print(nb_model_iris$apriori)
nb_predict_iris <- predict(nb_model_iris, iris_test)
conf <- table(pred = nb_predict_iris, true = iris_test$Species)
TauxDeClassification <- sum(diag(conf)) / sum(conf)
print(TauxDeClassification * 100)


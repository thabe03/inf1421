vnorm <- function(theta, obs) {
  mu <- theta[1]
  sigma <- theta[2]
  - sum(log(dnorm(x = obs, mu, sigma)))
}
obs <- rnorm(100, 0, 2)

Estim <- nlm(f = vnorm , p = c (0 , 0.9) , obs = obs)
print(mean(obs))
print(sd(obs))

u <- seq (-4 , 4 , by = 0.1)
hist(obs , probability = T)
lines(u,
      dnorm(u , Estim$estimate[1] , Estim$estimate[2]) , #Data>Estim $minimum,$estimate, $gradient, $code, $iterations
      col = "red",
      lwd = 3)
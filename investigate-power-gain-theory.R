test <- replicate(1000, {
  a0 <- sample(10, 4)
  a1 <- sample(10, 4)
  c(1 / sum(1 / (a0 + a1)), 1 / sum(1 / a0) + 1 / sum(1 / a1))
})
plot(t(test), pch = 20); abline(0, 1, col = "red", lwd = 3)

library(bigstatsr)
N <- 10000; h2 <- 0.5
test <- replicate(100, {
  X <- FBM(N, 2, init = rbinom(2 * N, 2, rep(c(0.2, 0.4), each = N)))
  X[] <- scale(X[])
  # colMeans(X[]) / 2
  y <- rowSums(X[]) * sqrt(h2 / 2)
  y <- y + rnorm(N, sd = sqrt(1 - h2))
  y <- (y > 1) + 0L

  c(
    big_univLogReg(X, y)$estim,
    big_univLogReg(X, y, ind.col = 1, covar.train = X[, 2, drop = FALSE])$estim,
    big_univLogReg(X, y, ind.col = 2, covar.train = X[, 1, drop = FALSE])$estim
  )
})
plot(test[1, ], test[3, ], pch = 20); abline(0, 1, col = "red", lwd = 3)
plot(test[2, ], test[4, ], pch = 20); abline(0, 1, col = "red", lwd = 3)
# std errors are always larger
# but, scores are always larger
# because, effect sizes are larger too


test2 <- replicate(100, {
  X <- FBM(N, 2, init = rbinom(2 * N, 2, rep(c(0.2, 0.4), each = N)))
  X[] <- scale(X[])
  # colMeans(X[]) / 2
  y <- rowSums(X[]) * sqrt(h2 / 2)
  y <- y + rnorm(N, sd = sqrt(1 - h2))
  y <- (y > 1) + 0L

  c(
    big_univLinReg(X, y)$estim,
    big_univLinReg(X, y, ind.col = 1, covar.train = X[, 2, drop = FALSE])$estim,
    big_univLinReg(X, y, ind.col = 2, covar.train = X[, 1, drop = FALSE])$estim
  )
})
plot(test2[1, ], test2[3, ], pch = 20); abline(0, 1, col = "red", lwd = 3)
plot(test2[2, ], test2[4, ], pch = 20); abline(0, 1, col = "red", lwd = 3)
# std errors are always smaller
# and, effect sizes are the same, approx
# so, scores are always larger

## Conclusion
# linear and logistic SNP effects are usually the sames,
# except when conditioning on large effects

## Question
# What is the best estimator of effects?

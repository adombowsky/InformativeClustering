# full conditional distribution of the partition c under CC
fc_c_CC <- function(c, W, y, theta, g, K){
  # sourcing
  source("ccfuncts/sym_dir_product.R")
  p <- rep(0, K)
  for (i in 1:n) {
    for (h in 1:K){
      c[i] = h
      p[h] <- 2 * sum( W[i, c == h]) + 
        log_sym_dir_product(c, g, K) +
        dnorm(y[i], theta[h], 1, log = T)
    }
    p <- exp(p - max(p))
    c[i] <- sample(1:K, size = 1, replace = T, prob = p)
  }
  return(c)
}
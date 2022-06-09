# full conditional distribution of the partition c under CP
fc_c_CP <- function(c, c_0, psi, y, theta, g, K){
  # sourcing
  source("ccfuncts/log_cluster_distance.R")
  source("ccfuncts/sym_dir_product.R")
  p <- rep(0, K)
  for (i in 1:n) {
    for (h in 1:K){
      c[i] = h
      p[h] <- log_cluster_distance(c, c_0, psi) + 
        log_sym_dir_product(c, g, K) +
        dnorm(y[i], theta[h], 1, log = T)
    }
    p <- exp(p - max(p))
    c[i] <- sample(1:K, size = 1, replace = T, prob = p)
  }
  return(c)
}

# apply function, used in future versions to speed up computation
app_prob_calculation <- function(x, n, c_0, psi) {
  c <- x[1:n]
  y_i <- x[(n+1)]
  theta_h <- x[(n+2)]
  prb <- log_cluster_distance(c, c_0, psi) + 
    log_sym_dir_product(c, g, K) +
    dnorm(y_i, theta_h, 1, log = T)
  return(prb)
}
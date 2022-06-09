# Gibbs sampler for the CP model
gibbs_CP <- function(R, y, c_0, psi, g, K, theta_0, sigma0_sq, stops) {
  
  # sourcing
  source("ccfuncts/fc_c_CP.R")
  source("ccfuncts/fc_theta.R")
  
  # preliminaries
  n <- length(y)
  
  # storage
  theta <- matrix(0, nrow = R, ncol = K)
  c <- matrix(sample(1:K, size = n * R, replace = T, prob = rep(1/K,K)), nrow = R, ncol = n)
  
  # configuring stops
  stops <- (1:(R/stops)) * stops
  
  # sampling
  print("sampling")
  for (r in 2:R) {
    # sample c
    c[r, ] <- fc_c_CP(c = c[r-1,], c_0 = c_0, psi = psi, y = y, theta = theta[r-1,], g = g, K = K )
    # sample theta
    theta[r, ] <- fc_theta(c = c[r,], y = y, K = K, theta_0 = theta_0, sigma0_sq = sigma0_sq)
    # print stops
    if (r %in% stops){
      print(r)
    }
  }
  
  # returning
  return(list(c = c, theta = theta))
}
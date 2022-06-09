## full conditional for theta (normal-normal)
fc_theta <- function(c, y, K, theta_0, sigma0_sq){
  theta <- rep(0, K)
  for (h in 1:K){
    y_h <- y[c == h]
    n_h <- length(y_h)
    v_h <- 1/(n_h + (1/sigma0_sq))
    m_h <- v_h * ( n_h * ifelse(n_h > 0, mean(y_h), 0) + (theta_0/sigma0_sq) )
    theta[h] <- m_h + sqrt(v_h)*rnorm(n = 1)
  }
  return(theta)
}
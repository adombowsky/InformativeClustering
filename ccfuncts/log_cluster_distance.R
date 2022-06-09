## computes the log-penalization term in Paganin (2021) based on the VI loss
log_cluster_distance <- function(c, c_0, psi) {
  require(mcclust)
  L_d <- - psi * vi.dist(c, c_0)
  return(L_d)
}
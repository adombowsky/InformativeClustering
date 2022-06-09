## computes the proportionality constant in the symmetric Dirichlet case
log_sym_dir_product <- function(c, g, K) {
  pr <- lgamma( g/K + table(factor(c, levels = 1:K)) )
  return(sum(pr))
}
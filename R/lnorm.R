#' Compute mean and variance of log-normal distribution
#'
#' Compute mean and varianve of LogNormal(mu, sigma)
#'
#' @param mu a vector
#' @param sigma a vector
#'
#' @return a matrix of mean and variance parameter
#' @export
params_to_moments <- function(mu, sigma) {
  cbind(
    mean = as.vector(exp(mu + (sigma ^ 2) / 2)),
    var = as.vector((exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2))
  )
}

#' Compute mean and variance of the sum of log-normal distribution
#'
#' Compute the mean and variance of the sum of log-normal distributions based on Fenton-Wilkinson method
#'
#' @param mu a vector
#' @param sigma a vector
#'
#' @return a vector of mu and sigma parameter
#' @export
params_sum_lnorm <- function(mu, sigma) {
  moments <- params_to_moments(mu, sigma)
  m <- moments[, 1L]; v <- moments[, 2L]
  # Compute mu, sigma parameters of the sum of log-normal distributions based on Fenton-Wilkinson method
  sigma2 <- log(sum(v) / sum(m) ^ 2 + 1)
  mu <- log(sum(m)) - sigma2 / 2
  c(mu = mu, sigma = sqrt(sigma2))
}

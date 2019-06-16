#' @export

# computing CES demand structure matrix
CES_A <- function(sigma, alpha, Beta, p, Theta = NULL) {
  .CES_A <- function(sigma, alpha, Beta, p) {
    if (!is.matrix(Beta)) Beta <- cbind(Beta)

    n <- nrow(Beta)
    m <- ncol(Beta)

    A <- matrix(0, n, m)

    for (cn in 1:m) {
      e1 <- 1 / (1 - sigma[cn])
      e2 <- sigma[cn] / (sigma[cn] - 1)
      e3 <- -1 / sigma[cn]
      k <- alpha[cn]
      beta <- Beta[, cn]
      for (rn in 1:n) {
        A[rn, cn] <- 1 / k * (beta[rn] / p[rn])^e1 * (sum(beta^e1 * p^e2))^e3
      }
    }
    A
  }

  # beginning ---------------------------------------------------------------
  if (is.null(Theta)) {
    return(.CES_A(sigma, alpha, Beta, p))
  }
  else {
    if (!is.matrix(Theta)) Theta <- cbind(Theta)
    result <- matrix(0, nrow(Beta), ncol(Beta))
    for (k in 1:ncol(Beta)) {
      result[, k] <- .CES_A(sigma[k], alpha[k], Beta[, k], p * Theta[, k]) * Theta[, k]
    }
    return(result)
  }
}

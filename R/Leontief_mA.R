#' @export

Leontief_mA <- function(A.pre, p) {
  # computing Leontief demand structure matrix in a monetary economy
  A <- A.pre

  nonnegativeA <- A
  nonnegativeA[nonnegativeA < 0] <- 0

  Indx <- which(A < 0, arr.ind = T)
  for (k in 1:nrow(Indx)) {
    A[Indx[k, 1], Indx[k, 2]] <- t(p) %*% nonnegativeA[, Indx[k, 2]] / (-A[Indx[k, 1], Indx[k, 2]])
  }

  A
}

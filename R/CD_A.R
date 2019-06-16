#' @export

CD_A <- function(alpha, Beta, p) {
  # computing Cobb-Douglas demand structure matrix
  if (is.numeric(Beta) && any(abs(colSums(Beta) - 1) > 10^-10)) {
    stop("Li: colSum(Beta)~=1, CD_A")
  }

  A <- dg(1 / p) %*% Beta %*% dg(apply((dg(1 / p) %*% Beta)^(-Beta), 2, prod) %*% dg(1 / alpha))
  A
}

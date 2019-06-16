#' @export

dg <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  if (is.vector(x) || nrow(x) == 1 || ncol(x) == 1) {
    return(diag(c(x)))
  } else {
    return(diag(x))
  }
}

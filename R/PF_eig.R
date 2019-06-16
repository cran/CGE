#' @export

# Compute the PF eigenvalue and eigenvector.
PF_eig <- function(M) {
  ev <- eigen(M)

  tmp <- Re(ev$values)

  indx <- which(tmp == max(tmp))
  if (length(indx) != 1) {
    print(M)
    print(ev$values)
    print(indx)
    stop("Li:PF_eig, none or multiple PF eig value")
  }

  PFVector <- ev$vectors[, indx]
  PFVector <- PFVector / sum(PFVector)
  list(val = abs(ev$values[indx]), vec = abs(PFVector))
}

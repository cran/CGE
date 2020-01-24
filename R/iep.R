#' @export
iep <- function(AT = NULL, A, B, SExg, InitialEndowments, nPeriods, ...) {

    ge.next <- function(._ge, ._A, ._B, ._SExg, ...) {
    result <- sdm(
      A = ._A,
      B = ._B,
      S0Exg = {
        S0Exg <- ._B %*% diag(._ge$z)
        S0Exg <- ifelse(!is.na(._SExg), ._SExg, S0Exg)
      },
      p0 = ._ge$p,
      z0 = ._ge$z,
      ...
    )
  }

  ge <- sdm(
    A = {
      if (is.null(AT)) A else AT(1)
    },
    B = B,
    S0Exg = InitialEndowments,
    ...
  )
  ge.list <- list(ge)
  for (time in 2:nPeriods) {
    ge <- ge.next(
      ge.list[[time - 1]],
      if (is.null(AT)) A else AT(time),
      B,
      if (is.function(SExg)) SExg(time) else SExg,
      ...
    )
    ge.list[[time]] <- ge
  }

  return(ge.list)
}

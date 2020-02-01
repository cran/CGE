#' @export
iep <- function(A.iep = NULL, A = NULL, B.iep = NULL, B = NULL, SExg.iep, InitialEndowments, nPeriods.iep, ...) {
  ge.next <- function(._ge, ._A, ._B, ._SExg.iep, ...) {
    result <- sdm(
      A = ._A,
      B = ._B,
      S0Exg = {
        if (is.function(._B)) {
          S0Exg <- ._B(list(p = ._ge$p)) %*% diag(._ge$z)
        } else {
          S0Exg <- ._B %*% diag(._ge$z)
        }

        S0Exg <- ifelse(!is.na(._SExg.iep), ._SExg.iep, S0Exg)
      },
      p0 = ._ge$p,
      z0 = ._ge$z,
      ...
    )
  }

  ge <- sdm(
    A = {
      if (is.null(A.iep)) A else A.iep(list(time = 1))
    },
    B = {
      if (is.null(B.iep)) B else B.iep(list(time = 1)) # Actually B will be ignored here.
    },
    S0Exg = InitialEndowments,
    ...
  )

  ge.list <- list(ge)
  for (time.iep in 2:nPeriods.iep) {
    state.iep <- list(
      time = time.iep,
      p = ge.list[[time.iep - 1]]$p,
      z = ge.list[[time.iep - 1]]$z
    )

    ge <- ge.next(
      ge.list[[time.iep - 1]],
      if (is.null(A.iep)) A else A.iep(state.iep),
      if (is.null(B.iep)) B else B.iep(list(time = time.iep - 1)),
      if (is.function(SExg.iep)) SExg.iep(state.iep) else SExg.iep,
      ...
    )
    ge.list[[time.iep]] <- ge
  }

  return(ge.list)
}

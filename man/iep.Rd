\name{iep}
\alias{iep}
\title{
Compute Instantaneous Equilibrium Path (alias Market Clearing Path)
}
\usage{
iep(A.iep = NULL, A = NULL,  B.iep = NULL, B = NULL,
    SExg.iep, InitialEndowments, nPeriods.iep, ...)
}
\arguments{
\item{A.iep}{A.iep(state.iep) is a function which returns a demand coefficient matrix or a function A(state). state.iep is a list consisting of time (the iep time), p (the price vector at the iep time), z (output and utility vector at the iep time).}

\item{A}{a demand coefficient matrix or a function A(state) which returns a demand coefficient matrix. If A.iep is not NULL, A will be ignored.}

\item{B.iep}{B.iep(state.iep) is a function which returns a supply coefficient matrix or a function B(state) at the iep time.}

\item{B}{a supply coefficient matrix or a function B(state) which returns a supply coefficient matrix. If B.iep is not NULL, B will be ignored.}

\item{SExg.iep}{an exogenous supply matrix or a function SExg.iep(state.iep) which returns an exogenous supply matrix at the iep time.}

\item{InitialEndowments}{a matrix indicating the initial endowments.}

\item{nPeriods.iep}{number of periods of the instantaneous equilibrium path.}

\item{...}{parameters of the function sdm.}

}
\value{
a list of general equilibria.
}
\description{
This function computes the instantaneous equilibrium path (alias market clearing path).
}
\details{
This function computes the instantaneous equilibrium path (alias market clearing path) of a dynamic economy with the structural dynamic model (the sdm function).
}
\references{
Acemoglu, D. (2009, ISBN: 9780691132921) Introduction to Modern Economic Growth. Princeton University Press.

LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

LI Wu (2010) A Structural Growth Model and its Applications to Sraffa's System. http://www.iioa.org/conferences/18th/papers/files/104_20100729011_AStructuralGrowthModelanditsApplicationstoSraffasSstem.pdf

Torres, Jose L. (2016, ISBN: 9781622730452). Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\seealso{
\code{\link{sdm}};
\code{\link{Example7.2}}
}
\examples{
\donttest{
## example 6.4 of Acemoglu (2009, page 206)
discount.factor <- 0.97
return.rate <- 1 / discount.factor - 1

A <- function(state) {
  a1 <- CD_A(
    1, rbind(0.35, 0.65, 0),
    c(state$p[1] * (1 + return.rate), state$p[2:3])
  )
  a2 <- c(1, 0, 0)
  a1[3] <- state$p[1] * a1[1] * return.rate / state$p[3]
  cbind(a1, a2)
}

B <- matrix(c(
  1, 0,
  0, 1,
  0, 1
), 3, 2, TRUE)


SExg.iep <- {
  tmp <- matrix(NA, 3, 2)
  tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 2)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

ge.list <- iep(
  A = A, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 50
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")

z[1:49, 1] * (1 - 0.97 * 0.35) # the same as z[-1,2] (i.e. consumption)

# stochastic (instantaneous) equilibrium path (SEP) in the economy above.
nPeriods.iep <-  150
set.seed(1)
alpha.SEP <- rep(1, 50)
for (t in 51:nPeriods.iep) {
  alpha.SEP[t] <- exp(0.95 * log(alpha.SEP[t - 1]) +
    rnorm(1, sd = 0.01))
}

A.iep <- function(state.iep) {
  A <- function(state) {
    a1 <- CD_A(
      alpha.SEP[state.iep$time],
      rbind(0.35, 0.65, 0),
      c(state$p[1] * (1 + return.rate), state$p[2:3])
    )
    a2 <- c(1, 0, 0)
    a1[3] <- state$p[1] * a1[1] * return.rate / state$p[3]
    cbind(a1, a2)
  }

  return(A)
}

ge.list <- iep(
  A.iep = A.iep, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = nPeriods.iep
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")

## an example with two firms
sigma <- 0 # 0 implies Cobb-Douglas production functions
gamma1 <- 0.01
gamma2 <- 0.01
gamma3 <- 0.01
beta1 <- 0.35
beta2 <- 0.4

A.iep <- function(state.iep) {
  A <- function(state) {
    a1 <- CES_A(sigma, exp(gamma1 * (state.iep$time - 1)), rbind(beta1, 0, 1 - beta1), state$p)
    a2 <- CES_A(sigma, exp(gamma2 * (state.iep$time - 1)), rbind(beta2, 0, 1 - beta2), state$p)
    a3 <- c(0, 1, 0)
    cbind(a1, a2, a3)
  }

  return(A)
}

B <- diag(3)

SExg.iep <- function(state.iep) {
  tmp <- matrix(NA, 3, 3)
  tmp[3, 3] <- exp(gamma3 * (state.iep$time - 1))
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 3)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- 0.02
  tmp[3, 3] <- 1
  tmp
}

ge.list <- iep(
  A.iep = A.iep, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 100, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z)) # outputs and utility
matplot(z, type = "l")

diff(log(z)) # logarithmic growth rate

## an example with heterogeneous firms
A <- function(state) {
  a1 <- CD_A(1, rbind(0.35, 0.65), state$p)
  a2 <- CD_A(1.3, rbind(0.9, 0.1), state$p)
  a3 <- c(1, 0)
  cbind(a1, a2, a3)
}

B <- matrix(c(
  1, 1, 0,
  0, 0, 1
), 2, 3, TRUE)

SExg.iep <- {
  tmp <- matrix(NA, 2, 3)
  tmp[2, 3] <- 1
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 2, 3)
  tmp[1, 1] <- tmp[1, 2] <- 0.01
  tmp[2, 3] <- 1
  tmp
}

ge.list <- iep(
  A = A, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 200, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")

## an iep of the example (see Table 2.1 and 2.2) of the canonical dynamic
## macroeconomic general equilibrium model in Torres (2016).
discount.factor <- 0.97
return.rate <- 1 / discount.factor - 1
depreciation.rate <- 0.06

A <- function(state) {
  a1 <- CD_A(1, rbind(0, 0.65, 0.35, 0), state$p)
  a2 <- CD_A(1, rbind(0.4, 1 - 0.4, 0, 0), state$p)
  a3 <- c(1, 0, 0, state$p[1] * return.rate / state$p[4])
  cbind(a1, a2, a3)
}

B <- matrix(c(
  1, 0, 1 - depreciation.rate,
  0, 1, 0,
  0, 0, 1,
  0, 1, 0
), 4, 3, TRUE)

SExg.iep <- {
  tmp <- matrix(NA, 4, 3)
  tmp[2, 2] <- tmp[4, 2] <- 1
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 4, 3)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- tmp[4, 2] <- 1
  tmp[3, 3] <- 0.01
  tmp
}

ge.list <- iep(
  A = A, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 200, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")

## another iep of the economy above
discount.factor <- 0.97
return.rate <- 1 / discount.factor - 1
depreciation.rate <- 0.06

A <- function(state) {
  a1 <- CD_A(
    1, rbind(0.35, 0.65, 0),
    c(state$p[1] * (return.rate + depreciation.rate), state$p[2:3])
  )
  a2 <- CD_A(1, rbind(0.4, 1 - 0.4, 0), state$p)
  a1[3] <- state$p[1] * a1[1] * return.rate / state$p[3]
  cbind(a1, a2)
}

B <- function(state) {
  tmp <- matrix(c(
    1, 0,
    0, 1,
    0, 1
  ), 3, 2, TRUE)

  tmp[1] <- tmp[1] + A(state)[1, 1] * (1 - depreciation.rate)
  tmp
}

SExg.iep <- {
  tmp <- matrix(NA, 3, 2)
  tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 2)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

ge.list <- iep(
  A = A, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 100, n = 3, m = 2, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")

## TFP shock in the economy above (see Torres, 2016, section 2.8).
nPeriods.iep <- 200

discount.factor <- 0.97
return.rate <- 1 / discount.factor - 1
depreciation.rate <- 0.06

set.seed(1)
alpha.shock <- rep(1, 100)
alpha.shock[101] <- exp(0.01)
for (t in 102:nPeriods.iep) {
  alpha.shock[t] <- exp(0.95 * log(alpha.shock[t - 1]))
}

A.iep <- function(state.iep) {
  A <- function(state) {
    a1 <- CD_A(
      alpha.shock[state.iep$time],
      rbind(0.35, 0.65, 0),
      c(state$p[1] * (return.rate + depreciation.rate), state$p[2:3])
    )
    a2 <- CD_A(1, rbind(0.4, 1 - 0.4, 0), state$p)
    a1[3] <- state$p[1] * a1[1] * return.rate / state$p[3]
    cbind(a1, a2)
  }

  return(A)
}

B.iep <- function(state.iep) {
  B <- function(state) {
    tmp <- matrix(c(
      1, 0,
      0, 1,
      0, 1
    ), 3, 2, TRUE)

    a1 <- CD_A(
      alpha.shock[state.iep$time],
      rbind(0.35, 0.65, 0),
      c(state$p[1] * (return.rate + depreciation.rate), state$p[2:3])
    )

    tmp[1] <- tmp[1] + a1[1] * (1 - depreciation.rate)
    tmp
  }

  return(B)
}

SExg.iep <- {
  tmp <- matrix(NA, 3, 2)
  tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 2)
  tmp[1, 1] <- tmp[2, 2] <- tmp[3, 2] <- 1
  tmp
}

ge.list <- iep(
  A.iep = A.iep, B.iep = B.iep, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = nPeriods.iep, n = 3, m = 2, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
c <- sapply(ge.list, function(x) x$A[1,2]*x$z[2]) #consumption

par(mfrow = c(2, 2))
matplot(z, type = "l")
x <- 100:140
plot(x, z[x, 1] / z[x[1], 1], type = "b", pch = 20)
plot(x, z[x, 2] / z[x[1], 2], type = "b", pch = 20)
plot(x, c[x] / c[x[1]], type = "b", pch = 20)

## an iep of example 7.2 (a monetary economy) in Li (2019).
A <- function(state) {
  alpha <- rbind(1, 1, 1)
  Beta <- matrix(c(
    0.5, 0.5, 0.5,
    0.5, 0.5, 0.5,
    -1, -1, -1
  ), 3, 3, TRUE)
  CD_mA(alpha, Beta, state$p)
}

B <- diag(3)

SExg.iep <- {
  tmp <- matrix(NA, 3, 3)
  tmp[2, 2] <- 100
  tmp[3, 3] <- 100
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 3)
  tmp[1, 1] <- 10
  tmp[2, 2] <- tmp[3, 3] <- 100
  tmp
}

ge.list <- iep(
  A = A, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 20,
  moneyIndex = 3,
  moneyOwnerIndex = 3,
  pExg = rbind(NA, NA, 0.25)
)

par(mfrow = c(1, 2))
z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "b", pch = 20)
p <- t(sapply(ge.list, function(x) x$p))
matplot(p, type = "b", pch = 20)

## an example of structural transition policy
A.iep <- function(state.iep) {
  a <- 15
  b <- 25
  A <- function(state) {
    alpha1 <- 5
    alpha2 <- 15

    if (state.iep$time == 1 || state.iep$z[1] <= a) {
      alpha <- alpha1
    } else if (state.iep$z[1] > b) {
      alpha <- alpha2
    } else {
      alpha <- (b - state.iep$z[1]) / (b - a) * alpha1 +
        (state.iep$z[1] - a) / (b - a) * alpha2
    }

    return(cbind(
      CD_A(alpha, c(0.5, 0.5), state$p),
      c(1, 0)
    ))
  }

  return(A)
}

B <- matrix(c(
  1, 0,
  0, 1
), 2, 2, TRUE)

SExg.iep <- function(state.iep) {
  if (state.iep$time >= 15 && state.iep$z[1] < 30) {
    result <- matrix(c(
      NA, NA,
      0.6, 0.4
    ), 2, 2, TRUE)
  } else {
    result <- matrix(c(
      NA, NA,
      0, 1
    ), 2, 2, TRUE)
  }

  return(result)
}

InitialEndowments <- {
  tmp <- matrix(0, 2, 2)
  tmp[1, 1] <- 1
  tmp[2, 2] <- 1
  tmp
}

ge.list <- iep(
  A.iep = A.iep, B = B, SExg.iep = SExg.iep,
  InitialEndowments = InitialEndowments,
  nPeriods.iep = 30, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "b", pch = 20)
}
}

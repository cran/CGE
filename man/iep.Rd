\name{iep}
\alias{iep}
\title{
Compute Instantaneous Equilibrium Path (alias Market Clearing Path)
}
\usage{
iep(AT = NULL, A, B, SExg, InitialEndowments, nPeriods, ...)
}
\arguments{
\item{AT}{AT(time) is a function which returns a demand coefficient matrix or a function A(state) at the time.}

\item{A}{a demand coefficient matrix or a function A(state) which returns a demand coefficient matrix. If AT is not NULL, A will be ignored.}

\item{B}{a supply coefficient matrix.}

\item{SExg}{an exogenous supply matrix or a function SExg(time) which returns an exogenous supply matrix at that time.}

\item{InitialEndowments}{a matrix indicating the initial endowments.}

\item{nPeriods}{number of periods of the instantaneous equilibrium path.}

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
\code{\link{sdm}}
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


SExg <- {
  tmp <- matrix(NA, 3, 2)
  tmp[2, 2] <- 1
  tmp[3, 2] <- 1
  tmp
}


InitialEndowments <- {
  tmp <- matrix(0, 3, 2)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- 1
  tmp[3, 2] <- 1
  tmp
}
ge.list <- iep(AT = NULL, A, B, SExg, InitialEndowments, nPeriods = 50)

z <- t(sapply(ge.list, function(x) x$z))

matplot(z, type = "l")

z[1:49, 1] * (1 - 0.97 * 0.35) # the same as z[-1,2] (i.e. consumption)

## iep of the example (see Table 2.1 and 2.2) of the canonical dynamic
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


SExg <- {
  tmp <- matrix(NA, 4, 3)
  tmp[2, 2] <- 1
  tmp[4, 2] <- 1
  tmp
}


InitialEndowments <- {
  tmp <- matrix(0, 4, 3)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- 1
  tmp[4, 2] <- 1
  tmp[3, 3] <- 0.01
  tmp
}
ge.list <- iep(AT = NULL, A, B, SExg, InitialEndowments, nPeriods = 200, trace = FALSE)

z <- t(sapply(ge.list, function(x) x$z))

matplot(z, type = "l")

## an example with two firms
sigma <- 0 # 0 implies Cobb-Douglas production functions
gamma1 <- 0.01
gamma2 <- 0.01
gamma3 <- 0.01
beta1 <- 0.35
beta2 <- 0.4

AT <- function(time) {
  f <- function(state) {
    a1 <- CES_A(sigma, exp(gamma1 * (time - 1)), rbind(beta1, 0, 1 - beta1), state$p)
    a2 <- CES_A(sigma, exp(gamma2 * (time - 1)), rbind(beta2, 0, 1 - beta2), state$p)
    a3 <- c(0, 1, 0)
    cbind(a1, a2, a3)
  }

  return(f)
}

B <- diag(3)

SExg <- function(time) {
  tmp <- matrix(NA, 3, 3)
  tmp[3, 3] <- exp(gamma3 * (time - 1))
  tmp
}

InitialEndowments <- {
  tmp <- matrix(0, 3, 3)
  tmp[1, 1] <- 0.01
  tmp[2, 2] <- 0.02
  tmp[3, 3] <- 1
  tmp
}
ge.list <- iep(AT, A = NULL, B, SExg, InitialEndowments, nPeriods = 100, trace = FALSE)

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
), 2, 3, T)

SExg <- {
  tmp <- matrix(NA, 2, 3)
  tmp[2, 3] <- 1
  tmp
}
InitialEndowments <- {
  tmp <- matrix(0, 2, 3)
  tmp[1, 1] <- 0.01
  tmp[1, 2] <- 0.01
  tmp[2, 3] <- 1
  tmp
}

ge.list <- iep(
  AT = NULL, A, B, SExg, InitialEndowments,
  nPeriods = 200, trace = FALSE
)

z <- t(sapply(ge.list, function(x) x$z))
matplot(z, type = "l")
}

}

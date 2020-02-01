\name{sdm}
\alias{sdm}
\title{Structural Dynamic Model (alias Structural Growth Model) }
\description{
This function computes the general equilibrium and simulates the economic dynamics. The key part of this function is an exchange function (see \code{\link{F_Z}}), which is expounded in Li (2010, 2019).
}
\usage{
sdm(
  A,
  B = diag(nrow(A)),
  n = nrow(B),
  m = ncol(B),
  S0Exg = matrix(NA, n, m),
  p0 = matrix(1, nrow = n, ncol = 1),
  z0 = matrix(100, nrow = m, ncol = 1),
  GRExg = NA,
  moneyOwnerIndex = NULL,
  moneyIndex = NULL,
  pExg = NULL,
  tolCond = 1e-5,
  maxIteration = 200,
  numberOfPeriods = 300,
  depreciationCoef = 0.8,
  thresholdForPriceAdjustment = 0.99,
  priceAdjustmentMethod = "variable",
  priceAdjustmentVelocity = 0.15,
  priceAdjustmentVelocityCoefficient = 0.95,
  substitutionMethod = "finalValue",
  trace = TRUE,
  ts = FALSE,
  policy = NULL,
  exchangeFunction = F_Z
)
}
\arguments{
\item{A}{a demand coefficient n-by-m matrix (alias demand structure matrix) or a function A(state) which returns an n-by-m matrix.}
\item{B}{a supply coefficient n-by-m matrix (alias supply structure matrix) or a function which returns an n-by-m matrix. If (i,j)-th element of S0Exg is not NA, the value of the (i,j)-th element of B will be usless and ignored.}
\item{n}{the number of commodities.}
\item{m}{the number of economic agents (or sectors).}
\item{S0Exg}{an initial exogenous supply n-by-m matrix.}
\item{p0}{an initial price n-vector.}
\item{z0}{an m-vector consisting of the initial exchange levels (i.e. activity levels, production levels or utility levels).}
\item{GRExg}{an exogenous growth rate of the exogenous supplies in S0Exg.}
\item{moneyOwnerIndex}{a vector consisting of the indices of agents supplying money.}
\item{moneyIndex}{a vector consisting of the commodity indices of all types of money.}
\item{pExg}{an n-vector indicating the exogenous prices (if any).}
\item{tolCond}{the tolerance condition.}
\item{maxIteration}{the maximum iteration count. If the main purpose of running this function is to do simulation instead of calculating equilibrium, then maxIteration should be set to 1.}
\item{numberOfPeriods}{the period number in each iteration.}
\item{depreciationCoef}{the depreciation coefficient (i.e. 1 minus the depreciation rate) of the unsold products.}
\item{thresholdForPriceAdjustment}{the threshold for the fixed percentage price adjustment method.}
\item{priceAdjustmentMethod}{the price adjustment method. Normally it should be set to "variable". If it is set to "fixed", a fixed percentage price adjustment method will be used.}
\item{priceAdjustmentVelocity}{the price adjustment velocity.}
\item{priceAdjustmentVelocityCoefficient}{the changing coefficient of the price adjustment velocity.}
\item{substitutionMethod}{the substitution method for iterations.}
\item{trace}{if TRUE, information is printed during the running of sdm.}
\item{ts}{if TRUE, the time series of the last iteration are returned.}
\item{policy}{a policy function.}
\item{exchangeFunction}{the exchange function.}
}
\value{
sdm returns a list containing the following components:
\item{tolerance}{the tolerance of the results.}
\item{p}{equilibrium prices.}
\item{z}{equilibrium exchange levels (i.e. activity levels, output levels or utility levels).}
\item{S}{equilibrium supply matrix at the initial period.}
\item{e}{equilibrium foreign exchange rates in a multi-money economy.}
\item{growthRate}{the endogenous equilibrium growth rate in a pure production economy.}
\item{A}{the equilibrium demand coefficient matrix.}
\item{B}{If B is a function, the equilibrium supply coefficient matrix is returned.}
\item{ts.p}{the time series of prices in the last iteration.}
\item{ts.z}{the time series of exchange levels (i.e. activity levels, production levels or utility levels) in the last iteration.}
\item{ts.S}{the time series of supply matrix in the last iteration.}
\item{ts.q}{the time series of sales rates in the last iteration.}
\item{ts.e}{the time series of foreign exchange rates in the last iteration.}
\item{policy.data}{the policy data.}
}
\details{
The parameters A may be a function A(state) wherein state is a list consisting of p (the price vector), z (the output and utility vector), w (the wealth vector), t (the time) and e (the foreign exchange rate vector if any). state indicates the states at time t.

The parameters B also may be a function B(state) wherein state is a list consisting of p (the price vector), z (the output and utility vector) and t (the time).
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

LI Wu (2010) A Structural Growth Model and its Applications to Sraffa's System. http://www.iioa.org/conferences/18th/papers/files/104_20100729011_AStructuralGrowthModelanditsApplicationstoSraffasSstem.pdf

Torres, Jose L. (2016, ISBN: 9781622730452). Introduction to Dynamic Macroeconomic General Equilibrium Models (Second Edition). Vernon Press.

Varian, Hal R. (1992, ISBN: 0393957357). Microeconomic Analysis. W. W. Norton & Company.
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\seealso{
\code{\link{iep}};
\code{\link{Example2.2}};
\code{\link{Example2.3}};
\code{\link{Example.Section.3.1.2.corn}};
\code{\link{Example3.1}};
\code{\link{Example3.2}};
\code{\link{Example3.4}};
\code{\link{Example3.8}};
\code{\link{Example3.9}};
\code{\link{Example3.10}};
\code{\link{Example3.12}};
\code{\link{Example3.14}};
\code{\link{Example4.2}};
\code{\link{Example4.8}};
\code{\link{Example4.9}};
\code{\link{Example4.10}};
\code{\link{Example4.11.1}};
\code{\link{Example4.11.2}};
\code{\link{Example4.12}};
\code{\link{Example4.13}};
\code{\link{Example4.15}};
\code{\link{Example4.16}};
\code{\link{Example5.1}};
\code{\link{Example5.2}};
\code{\link{Example5.3.2}};
\code{\link{Example5.4}};
\code{\link{Example5.5}};
\code{\link{Example5.6}};
\code{\link{Example5.10}};
\code{\link{Example5.11.1}};
\code{\link{Example5.11.2}};
\code{\link{Example6.2.1}};
\code{\link{Example6.2.2}};
\code{\link{Example6.3}};
\code{\link{Example6.4}};
\code{\link{Example6.5}};
\code{\link{Example6.6.1}};
\code{\link{Example6.6.2}};
\code{\link{Example6.6.3}};
\code{\link{Example6.7}};
\code{\link{Example6.9}};
\code{\link{Example6.10}};
\code{\link{Example6.11}};
\code{\link{Example7.1}};
\code{\link{Example7.2}};
\code{\link{Example7.3}};
\code{\link{Example7.4}};
\code{\link{Example7.5.1}};
\code{\link{Example7.5.2}};
\code{\link{Example7.6}};
\code{\link{Example7.7}};
\code{\link{Example7.8}};
\code{\link{Example7.9X}};
\code{\link{Example7.10}};
\code{\link{Example7.10.2}};
\code{\link{Example7.11}};
\code{\link{Example7.12}};
\code{\link{Example7.13}};
\code{\link{Example7.14}};
\code{\link{Example7.15}};
\code{\link{Example8.1}};
\code{\link{Example8.2}};
\code{\link{Example8.7}};
\code{\link{Example8.8}};
\code{\link{Example8.9}};
\code{\link{Example9.3}};
\code{\link{Example9.4}};
\code{\link{Example9.5}};
\code{\link{Example9.6}};
\code{\link{Example9.7}};
\code{\link{Example9.10}};
}
\examples{
## the example on page 352 in Varian (1992)
ge <- sdm(
  A = function(state) {
    a <- 0.5

    alpha <- rep(1, 3)
    Beta <- matrix(c(0,   a,   a,
                     0.5, 0,   0,
                     0.5, 1 - a, 1 - a), 3, 3, TRUE)

    #the demand coefficient matrix.
    CD_A(alpha, Beta, state$p)
  },
  B = diag(3),
  S0Exg = matrix(c(NA, NA, NA,
                   NA, 1, NA,
                   NA, NA, 1), 3, 3, TRUE),
  GRExg = 0,
  tolCond = 1e-10
)

ge$p/ge$p[1]


## the example (see Table 2.1 and 2.2) of the canonical dynamic
## macroeconomic general equilibrium model in Torres (2016).
discount.factor <- 0.97
return.rate <- 1 / discount.factor - 1
depreciation.rate <- 0.06

ge <- sdm(
  n = 4, m = 3,
  A = function(state) {
    a1 <- CD_A(1, rbind(0, 0.65, 0.35, 0), state$p)
    a2 <- CD_A(1, rbind(0.4, 1 - 0.4, 0, 0), state$p)
    a3 <- c(1, 0, 0, state$p[1] * return.rate / state$p[4])
    cbind(a1, a2, a3)
  },
  B = matrix(c(
    1, 0, 1 - depreciation.rate,
    0, 1, 0,
    0, 0, 1,
    0, 1, 0
  ), 4, 3, TRUE),
  S0Exg = {
    tmp <- matrix(NA, 4, 3)
    tmp[2, 2] <- 1
    tmp[4, 2] <- 1
    tmp
  },
  priceAdjustmentVelocity = 0.03,
  maxIteration = 1,
  numberOfPeriods = 5000,
  ts = TRUE
)

ge$A \%*\% diag(ge$z) # the demand matrix
ge$p / ge$p[1]

plot(ge$ts.z[, 1], type = "l")
}

\name{Example9.10}
\alias{Example9.10}
\title{Example 9.10-9.14 in Li (2019)}
\description{This is Example 9.10-14 in Li (2019), which illustrates economic cycles in a monetary economy and economic policies ironing economic cycles.}
\usage{
Example9.10(
  policy = NULL,
  pExg = rbind(NA, NA, 0.25),
  p0 = rbind(0.625, 0.375, 0.25),
  priceAdjustmentVelocity = 0.3,
  ts = TRUE
)
}
\arguments{
Those arguments will be passed to the function sdm. See \code{\link{sdm}}.

\item{policy}{a policy function}
\item{pExg}{an n-vector indicating the exogenous prices (if any).}
\item{p0}{an initial price n-vector.}
\item{priceAdjustmentVelocity}{the price adjustment velocity.}
\item{ts}{if TRUE, the time series of the last iteration are returned.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\seealso{
\code{\link{sdm}};
\code{\link{Example9.10.policy.interest.rate}};
\code{\link{Example9.10.policy.money.supply}};
\code{\link{Example9.10.policy.deflation}};
\code{\link{Example9.10.policy.quantitative.easing}};
\code{\link{Example9.10.policy.tax}};
\code{\link{Example9.10.policy.deficit.fiscal}}
}
\examples{
\donttest{
##### no policy
ge <- Example9.10()
matplot(ge$ts.p, type = "l")
matplot(ge$ts.z, type = "l")

##### interest rate policy (Fig. 9.12)
Example9.10(policy = Example9.10.policy.interest.rate)

##### monetary supply policy (Fig. 9.13)
Example9.10(policy = Example9.10.policy.money.supply)

##### deflation policy (Fig. 9.14)
ge <- Example9.10(
  policy = Example9.10.policy.deflation,
  pExg = rbind(NA, NA, 0),
  p0 = rbind(0.625, 0.375, 0), ts = TRUE
)
plot(ge$ts.S[3, 3, ], type = "l")
plot(ge$ts.q[, 3], type = "l")

##### quantitative easing policy (Fig. 9.15)
ge <- Example9.10(
  policy = Example9.10.policy.quantitative.easing,
  pExg = rbind(NA, NA, 0),
  p0 = rbind(0.625, 0.375, 0),
  ts = TRUE
)
plot(log(ge$ts.S[3, 3, ]), type = "l")
plot(ge$ts.q[, 3], type = "l")
plot(log(ge$ts.p[, 1]), type = "l")
lines(log(ge$ts.p[, 2]), col = "blue")

##### deficit fiscal policy (Fig. 9.17; Fig. 9.18)
ge <- Example9.10(
  policy = Example9.10.policy.deficit.fiscal,
  priceAdjustmentVelocity = 0.5, ts = TRUE
)
plot(ge$ts.S[3, 3, ], type = "l")
plot(ge$ts.q[, 1], type = "l")

deficit.Example9.10 <- ge$policy.data
plot(deficit.Example9.10, type = "l")
plot(deficit.Example9.10[, 1], cumsum(deficit.Example9.10[, 2]), type = "l")
plot(deficit.Example9.10[, 1],
  cumsum(deficit.Example9.10[, 2]) /
    (tail(ge$ts.z[, 1] * ge$ts.p[, 1], -399)),
  type = "l"
)

##### tax policy (Fig. 9.16)
ge <- Example9.10(policy = Example9.10.policy.tax)
plot(ge$policy.data, type = "l")
}
}

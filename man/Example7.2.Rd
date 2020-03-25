\name{Example7.2}
\alias{Example7.2}
\title{Example 7.2 in Li (2019)}
\description{This is Example 7.2 in Li (2019),which illustrates a monetary Cobb-Douglas zero-growth corn economy.}
\usage{Example7.2()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}

\examples{
## Another way to compute this equilibrium, i.e. treating money as tax receipt.
r <- 0.25
ge <- sdm(
  A = function(state) {
    alpha <- rbind(1, 1, 1)
    Beta <- matrix(c(
      0.5, 0.5, 0.5,
      0.5, 0.5, 0.5
    ), 2, 3, TRUE)
    tmp.A <- CD_A(alpha, Beta, state$p[1:2])
    tmp <- apply(tmp.A, 2, function(x) sum(x * state$p[1:2]))

    rbind(tmp.A, r * tmp / state$p[3])
  },
  B = diag(3),
  S0Exg = {
    tmp <- matrix(NA, 3, 3)
    tmp[2, 2] <- 100
    tmp[3, 3] <- 100
    tmp
  }
)

ge$p / ge$p[3] * r

p <- ge$p
p[3] <- p[3] / r
p / p[3]

}

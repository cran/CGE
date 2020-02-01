\name{F_Z}
\alias{F_Z}
\title{Exchange Function}
\description{Given a price vector, a demand coefficient matrix and a supply matrix, this function computes the (disequilibrium) exchange results of an exchange process. There are n commodities and m agents in the exchange process.}
\usage{
F_Z(A, p, S)
}
\arguments{
\item{A}{a n-by-m demand coefficient matrix.}
\item{p}{a price n-vector.}
\item{S}{a n-by-m supply matrix.}
}
\value{
F_Z returns a list containing the following components:
\item{z}{an exchange amount m-vector.}
\item{q}{a sales rate n-vector.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
A <- matrix(c(
  0.05, 0.05, 0.1,
  0.1, 0, 0.1,
  0, 0.15, 0.05
), 3, 3, TRUE)
S <- diag(3)

# a market-clearing price vector
p <- c(0.6, 0.9, 1)
result <- F_Z(A, p, S)
# Each sales rate is equal to 1
result$q
# the purchase matrix
A \%*\% diag(result$z)

# a non-market-clearing price vector
p <- c(1, 1, 1)
result <- F_Z(A, p, S)
# Some sales rates don't equal 1
result$q
# the purchase matrix
A \%*\% diag(result$z)
}


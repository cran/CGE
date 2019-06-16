\name{CES_mA}
\alias{CES_mA }
\title{CES Monetary Demand Structure Matrix}
\description{
This function computes a CES monetary demand structure matrix in a monetary economy.
}
\usage{
CES_mA(sigma, alpha, Beta, p, Theta = NULL)
}
\arguments{
\item{sigma}{a numeric m-vector or m-by-1 matrix.}
\item{alpha}{a nonnegative numeric m-vector or m-by-1 matrix.}
\item{Beta}{a nonnegative numeric n-by-m matrix whose each column sum equals 1.}
\item{p}{a nonnegative numeric n-vector or n-by-1 matrix.}
\item{Theta}{null or a positive numeric n-by-m matrix.}
}
\value{
A n-by-m matrix is computed which indicates the (monetary) demand structure of agents (firms or consumers) with CES production functions or utility functions under the price vector p.
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\details{
Some elements of Beta corresponding to money equal -1.
}

\examples{
alpha <- matrix(1, 6, 1)
Beta <- matrix(c(
  0,   1,  1,   0,   1,   1,
  0.5, 0,  0,   0,   0,   0,
  -1, -1, -1,   0,   0,   0,
  0.5, 0,  0,   0.5, 0,   0,
  0,   0,  0,   0.5, 0,   0,
  0,   0,  0,  -1,  -1,  -1
), 6, 6, TRUE)
p <- 1:6
CES_mA(rep(-1, 6), alpha, Beta, p)
}

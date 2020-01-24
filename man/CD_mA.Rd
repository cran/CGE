\name{CD_mA}
\alias{CD_mA}
\title{Cobb-Douglas Monetary Demand Structure Matrix}

\description{
This function computes a Cobb-Douglas monetary demand structure matrix in a monetary economy.
}
\usage{
CD_mA(alpha, Beta, p)
}
\arguments{
\item{alpha}{a nonnegative numeric m-vector or m-by-1 matrix.}
\item{Beta}{nonnegative numeric n-by-m matrix whose each column sum equals 1.}
\item{p}{a nonnegative numeric n-vector or n-by-1 matrix.}
}
\value{
A n-by-m matrix is computed which indicates the (monetary) demand structure of agents (firms or consumers) with Cobb-Douglas production functions or utility functions under the price vector p.
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
alpha <- c(1, 1, 1)
Beta <- matrix(c(
  0.5, 0.5, 0.5,
  0.5, 0.5, 0.5,
  -1,  -1,  -1
), 3, 3, TRUE)
p <- c(1, 2, 0.1)
CD_mA(alpha, Beta, p)
}

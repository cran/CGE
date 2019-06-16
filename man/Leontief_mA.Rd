\name{Leontief_mA}
\alias{Leontief_mA }
\title{Leontief Monetary Demand Structure Matrix}
\description{
This function computes a Leontief monetary demand structure matrix in a monetary economy.
}
\usage{
Leontief_mA(A, p)
}
\arguments{
\item{A}{a numeric n-by-m matrix.}
\item{p}{a nonnegative numeric n-vector or n-by-1 matrix.}
}
\value{
A n-by-m matrix is computed which indicates the (monetary) demand structure of agents (firms or consumers) with Leontief production functions or utility functions under the price vector p.
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\details{
Some elements of A corresponding to money equal -1.
}

\examples{
A<-matrix(c(0.5, 1, 1,
            0.1, 0, 0,
             -1, -1, -1),3,3,TRUE)
p<-1:3
Leontief_mA(A,p)
}

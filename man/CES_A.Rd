\name{CES_A}
\alias{CES_A}
\title{ CES Demand Structure Matrix}
\description{
This function computes the CES demand structure matrix.
}
\usage{CES_A(sigma,alpha,Beta,p)}
\arguments{
\item{sigma}{a numeric m-vector or m-by-1 matrix.}
\item{alpha}{a nonnegative numeric m-vector or m-by-1 matrix.}
\item{Beta}{a nonnegative numeric n-by-m matrix.}
\item{p}{a nonnegative numeric n-vector or n-by-1 matrix.}
}
\value{
A n-by-m matrix is computed which indicates the demand structure of agents (firms or consumers) with CES production functions or utility functions under the price vector p.
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}

\examples{
CES_A(-1, 2, rbind(0.2,0.1), c(1,2))
#####
sigma<-c(-1,-1,-1)
alpha<-c(1,1,1)
Beta<-matrix(c(0, 1, 1,
               1, 0, 0,
               1, 0, 0),3,3,TRUE)
p<-1:3
CES_A(sigma,alpha,Beta,p)}

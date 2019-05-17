\name{CD_A}
\alias{CD_A}
\title{Cobb-Douglas Demand Structure Matrix}
\description{This function computes the Cobb-Douglas demand structure matrix.}
\usage{CD_A(alpha,Beta,p)}
\arguments{
\item{alpha}{a nonnegative numeric m-vector or m-by-1 matrix.}
\item{Beta}{a nonnegative numeric n-by-m matrix whose each column sum equals 1.}
\item{p}{a nonnegative numeric n-vector or n-by-1 matrix.}
}
\value{
A n-by-m matrix is computed which indicates the demand structure of agents (firms or consumers) with Cobb-Douglas production functions or utility functions under the price vector p.
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}

\examples{
CD_A(1,rbind(0.5,0.5),c(1,2))

#####
alpha<-rbind(5,3,1)
Beta<-matrix(c(0.6, 0.4,  0.2,
               0.1,  0.4,  0.7,
               0.3,  0.2,  0.1),3,3,TRUE)
p<-1:3
CD_A(alpha,Beta,p)
}



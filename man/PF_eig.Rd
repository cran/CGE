\name{PF_eig}
\alias{PF_eig}
\title{P-F (i.e. Perron-Frobenius) Eigenvalue and Eigenvector}
\description{This function computes the P-F (i.e. Perron-Frobenius) eigenvalue and eigenvector of an indecomposable nonnegative square matrix.}
\usage{PF_eig(M)}
\arguments{
\item{M}{an indecomposable nonnegative square matrix.}
}
\value{
PF_eig returns a list containing the following components:
\item{val}{the P-F eigenvalue of M.}
\item{vec}{the normalized P-F eigenvector of M.}
}
\references{
Horn, R. A., Johnson, C. R. (2012, ISBN: 0521548233) Matrix Analysis. Cambridge University Press.
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}

\examples{
M<-matrix(c(0.5,1,
            1,  0),2,2,TRUE)
PF_eig(M)
}

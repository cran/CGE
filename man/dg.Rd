\name{dg}
\alias{dg}
\title{A Modified diag Function}
\description{This function works in the way analogous to the diag function of Matlab.}
\usage{dg(x)}
\arguments{
\item{x}{a number, vector or square matrix.}
}
\value{
If x is a number, dg returns itself. If x is a vector, a one-row matrix or a one-column matrix, dg returns a matrix  with x as the main diagnol. Otherwise dg returns diag(x).
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}

\examples{
diag(matrix(2,3))
dg(matrix(2,3))
}

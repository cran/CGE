\name{Example.MWG.15.B.2}
\alias{Example.MWG.15.B.2}
\title{Example 15.B.2 in MWG (1995)}
\description{This is Example 15.B.2 in MWG (1995, P521), which is a pure exchange 2-by-2 economy with quasilinear utility functions.}
\usage{
Example.MWG.15.B.2(p0 = c(1, 0.3))
}
\arguments{
\item{p0}{an initial price 2-vector, which will be passed to the function sdm.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

Mas-Colell, Andreu and Whinston, Michael Dennis and Green, Jerry R. (1995, ISBN: 0195073401). Microeconomic Theory. Oxford University Press (New York).
}
\examples{
\donttest{
ge <- Example.MWG.15.B.2()
ge$p

#####
ge <- Example.MWG.15.B.2(p0 = c(0.3, 1))
ge$p

#####
ge <- Example.MWG.15.B.2(p0 = c(1, 1))
ge$p
}
}

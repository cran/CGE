\name{Example.MWG.Exercise.15.B.6}
\alias{Example.MWG.Exercise.15.B.6}
\title{Exercise 15.B.6 in MWG (1995)}
\description{This is Exercise 15.B.6 in MWG (1995, P541), which is a pure exchange CES 2-by-2 economy.}
\usage{
Example.MWG.Exercise.15.B.6(p0 = c(1, 2))
}
\arguments{
\item{p0}{an initial price 2-vector, which will be passed to the function sdm.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

Mas-Colell, Andreu and Whinston, Michael Dennis and Green, Jerry R. (1995, ISBN: 0195073401) Microeconomic Theory. Oxford University Press (New York).

}
\examples{
\donttest{
ge <- Example.MWG.Exercise.15.B.6()
ge$p / ge$p[2] # (3/4)^3

#####
ge <- Example.MWG.Exercise.15.B.6(p0 = c(2, 1))
ge$p / ge$p[2] # (4/3)^3

#####
ge <- Example.MWG.Exercise.15.B.6(p0 = c(1, 1))
ge$p
}
}

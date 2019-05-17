\name{Example.MWG.Exercise.15.B.9}
\alias{Example.MWG.Exercise.15.B.9}
\title{Exercise 15.B.9 in MWG (1995)}
\description{This is Exercise 15.B.9 in MWG (1995, P541), which is a pure exchange 2-by-2 economy.}
\usage{Example.MWG.Exercise.15.B.9(S0Exg=matrix(c(30, 0, 0, 20),2,2,TRUE))}
\arguments{
\item{S0Exg}{an exogenous supply matrix, which will be passed to the function sdm.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

Mas-Colell, Andreu and Whinston, Michael Dennis and Green, Jerry R. (1995, ISBN: 0195073401). Microeconomic Theory. Oxford University Press (New York).
}
\examples{
Example.MWG.Exercise.15.B.9()

#####
S<-matrix(c(5, 0,
            0, 20),2,2,TRUE)
Example.MWG.Exercise.15.B.9(S0Exg=S)
}

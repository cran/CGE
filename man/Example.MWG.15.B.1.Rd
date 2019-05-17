\name{Example.MWG.15.B.1}
\alias{Example.MWG.15.B.1}
\title{Example 15.B.1 in MWG (1995)}
\description{This is Example 15.B.1 in MWG (1995, P519), which is a pure exchange Cobb-Douglas 2-by-2 economy.}
\usage{Example.MWG.15.B.1(a=0.1,S0Exg=matrix(c(1, 2, 2, 1), 2, 2, TRUE))}
\arguments{
\item{a}{Each consumer has the Cobb-Douglas utility function x1^a*x2^(1-a).}
\item{S0Exg}{exogenous supply matrix which will be passed to the function sdm.}
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

Mas-Colell, Andreu and Whinston, Michael Dennis and Green, Jerry R. (1995, ISBN: 0195073401). Microeconomic Theory. Oxford University Press (New York).
}
\examples{
Example.MWG.15.B.1()

#####
Example.MWG.15.B.1(a=0.2)

#####
S<-matrix(c(18, 72,
            40, 20), 2, 2, TRUE)
ge<-Example.MWG.15.B.1(a=0.2, S0Exg=S)
ge$p/ge$p[1]
}

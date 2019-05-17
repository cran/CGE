\name{Example6.13}
\alias{Example6.13}
\title{Example 6.13 in Li (2019)}
\description{This is Example 6.13 in Li (2019),which illustrates a two-country economy.}
\usage{Example6.13()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example6.13()
matplot(ge$ts.p)
matplot(ge$ts.z)
}

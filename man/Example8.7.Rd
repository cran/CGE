\name{Example8.7}
\alias{Example8.7}
\title{Example 8.7 in Li (2019)}
\description{This is Example 8.7 in Li (2019), which discusses price changes in the coffee economy.}
\usage{Example8.7()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example8.7()
matplot(ge$ts.p)
matplot(ge$ts.z)
}

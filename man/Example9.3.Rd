\name{Example9.3}
\alias{Example9.3}
\title{Example 9.3 in Li (2019)}
\description{This is Example 9.3 in Li (2019), which illustrates economic cycles in a pure production economy.}
\usage{Example9.3()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example9.3()
matplot(ge$ts.p)
matplot(ge$ts.z)
}

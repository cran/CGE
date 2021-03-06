\name{Example9.7}
\alias{Example9.7}
\title{Example 9.7 in Li (2019)}
\description{This is Example 9.7 in Li (2019), which illustrates fixed assets and economic cycles.}
\usage{Example9.7()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example9.7()
matplot(ge$ts.p, type="l")
matplot(ge$ts.z, type="l")
}

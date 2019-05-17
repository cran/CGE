\name{Example9.6}
\alias{Example9.6}
\title{Example 9.6 in Li (2019)}
\description{This is Example 9.6 in Li (2019), which illustrates the technological progress and capital accumulation in the corn economy.}
\usage{Example9.6()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example9.6()
matplot(ge$ts.p)
matplot(ge$ts.z)
}

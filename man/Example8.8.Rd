\name{Example8.8}
\alias{Example8.8}
\title{Example 8.8 in Li (2019)}
\description{This is Example 8.8 in Li (2019), which illustrates a dynamic exchange model with one type of money.}
\usage{Example8.8()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge<-Example8.8()
matplot(ge$ts.p)
matplot(ge$ts.z)
}

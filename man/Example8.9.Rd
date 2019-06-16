\name{Example8.9}
\alias{Example8.9}
\title{Example 8.9 in Li (2019)}
\description{This is Example 8.9 in Li (2019), which illustrates a dynamic exchange model with multiple types of money.}
\usage{Example8.9()}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)
}
\examples{
ge <- Example8.9()
matplot(ge$ts.p, type = "l")
matplot(ge$ts.z, type = "l")
}

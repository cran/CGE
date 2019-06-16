\name{Example.Varian.Exercise.18.2}
\alias{Example.Varian.Exercise.18.2}
\title{Exercise 18.2 in Varian (1992)}
\description{This is Exercise 18.2 in Varian (1992, P357), which is a Cobb-Douglas 3-by-4 economy.}
\usage{
Example.Varian.Exercise.18.2()
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

Varian, Hal R. (1992, ISBN: 0393957357). Microeconomic Analysis. W. W. Norton & Company.
}
\examples{
ge <- Example.Varian.Exercise.18.2()
ge$A \%*\% diag(ge$z)  #input matrix
}

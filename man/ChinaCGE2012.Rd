\name{ChinaCGE2012}
\docType{data}
\alias{ChinaCGE2012}
\title{A CGE Model of China based on the Input-Output Table of 2012 (Unit: Ten Thousand RMB)}
\description{This data set gives parameters of a CGE model of China based on the input-output table of 2012.}
\usage{ChinaCGE2012}
\format{
A list containing the following components:
  \tabular{lll}{
    A(state)  \tab function \tab a function which returns a demand structure 41-by-38 matrix under a given price 41-vector.\cr
    B\tab numeric \tab a supply matrix 41-by-38 matrix.\cr
    S0Exg \tab numeric \tab an exogenous supply 41-by-38 matrix.\cr
    z0 \tab numeric \tab an initial activity level (or production level) 38-vector.\cr
    subject.names \tab character \tab names of 41 subjects (or commodities).\cr
    sector.names  \tab character \tab names of 38 sectors.
  }
}
\references{
LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing:
Economic Science Press. (In Chinese)
}
\author{
LI Wu <liwu@staff.shu.edu.cn>
}

\examples{
\donttest{
ChinaCGE2012$A(list(p=rep(1,41)))

#####
cge<-function(GRExg=0){
  sdm(
  A=ChinaCGE2012$A,
  B=ChinaCGE2012$B,
  S0Exg=ChinaCGE2012$S0Exg,
  GRExg=GRExg,
  z0=ChinaCGE2012$z0,
  priceAdjustmentVelocity=0.03)
}

#####
ge0<-cge()
names(ge0$p)<-ChinaCGE2012$subject.names
ge0$p

names(ge0$z)<-ChinaCGE2012$sector.names
ge0$z

#####
ge6<-cge(GRExg=0.06)
names(ge6$p)<-ChinaCGE2012$subject.names
ge6$p

names(ge6$z)<-ChinaCGE2012$sector.names
ge6$z
}
}

#' @export

#MWG_exchange_quasiLinear_2_2
Example.MWG.15.B.2<-function(p0=c(1,0.3)){
  r<-2^(8/9)-2^(1/9)

  MWG_quasi_linear_demand_1<-function(p,w){ #only for case 'MWG_exchange_quasiLinear_2_2'
    d<-rbind(0,0)
    w<-w/p[1];  p<-p/p[1]; #normalize wealth and prices
    d[2]<-(1/p[2])^(1/9);
    if (d[2]*p[2]>w){
      d[2]<-w/p[2]
      d[1]<-0
    } else d[1]<- w-d[2]*p[2]

    d
  }

  MWG_quasi_linear_demand_2<-function(p,w){  #only for case 'MWG_exchange_quasiLinear_2_2'
    d<-rbind(0,0)
    w<-w/p[2];  p<-p/p[2];
    d[1]<-(1/p[1])^(1/9);
    if (d[1]*p[1]>w) {
      d[1]<-w/p[1];
      d[2]<-0;
    } else d[2]<- w-d[1]*p[1];

    d
  }

  sdm(
    A=function(state){
      result1<-MWG_quasi_linear_demand_1(state$p, state$w[1])
      result2<-MWG_quasi_linear_demand_2(state$p, state$w[2])
      cbind(result1, result2)
    },
    B=diag(2),
    S0Exg=matrix(c(2,r,
                   r,2),2,2,TRUE), #Exogenous supplies
    GRExg=0,
    p0=p0,
    tolCond=1e-7
  )
}


#' @export

#Varian, H. R.(1992) Microeconomic Analysis... P351-353; (MWG) Mas-Colell, A., Whinston, M. and Green, J. (1995) Microeconomic Theory... P542, 15.C.2.
#case 'CD_3_3'
#column 1: firm (say, wheat producer);
#column 2: firm owner;
#column 3: laborer;
#row 1: wheat;
#row 2: ownership of firm (or land, see Varian (1992, P353));
#row 3: labor (or leisure);
Example.Varian.P352<-function(agent.number=3){
  if (agent.number==3){
    sdm(
      A=function(state){
        a<-0.5;
        alpha<-rep(1,3)
        Beta<-matrix(c(0,   a,   a,
                       0.5, 0,   0,
                       0.5, 1-a, 1-a),3,3,TRUE)

        CD_A(alpha,Beta,state$p) #Demand functions of agents constitute an input coefficient matrx.
      },
      B=diag(3),
      S0Exg=matrix(c(NA, NA, NA,
                     NA, 1, NA,
                     NA, NA, 1), 3, 3, TRUE),  #Exogenous supplies,
      GRExg=0,
      tolCond=1e-10
    )
  } else {
    sdm(
      A=function(state){
        a<-0.5;
        alpha<-rep(1,2)
        Beta<-matrix(c(0,   a,
                       0.5, 0,
                       0.5, 1-a),3,2,TRUE)

        CD_A(alpha,Beta,state$p)
      },
      B=matrix(c(1, 0,
                 0, 1,
                 0, 1), 3, 2, TRUE),
      S0Exg=matrix(c(NA, NA,
                     NA, 1,
                     NA, 1), 3, 2, TRUE),
      GRExg=0,
      tolCond=1e-10
    )
  }
}


#' @export

#MWG_exchange_CES_2_2, MWG, P541
Example.MWG.Exercise.15.B.6<-function(p0=c(1,2)){
  sdm(
    A=function(state){
      alpha<-c(1,1)
      Beta<-matrix(c(1, (12/37)^3,
                     (12/37)^3, 1), 2, 2, TRUE)

      CES_A(c(-2,-2),alpha,Beta,state$p)
    },
    B=diag(2),
    S0Exg=diag(2),
    GRExg=0,
    p0=p0,
    priceAdjustmentVelocity=0.3,
    tolCond = 1e-10
  )
}

#' @export

#MWG_exchange_CD_2_2, MWG, 15.B.1, P519
Example.MWG.15.B.1<-function(a=0.1,
                             S0Exg=matrix(c(1, 2, 2, 1), 2, 2, TRUE)){
    sdm(
      A=function(state){
        alpha<-c(1,1)
        Beta<-matrix(c(a, a,
                       1-a, 1-a),2,2,TRUE)

        CD_A(alpha,Beta,state$p)
      },
      B=diag(2),
      S0Exg=S0Exg,
      GRExg=0
    )
}

#' @export

#MWG_exchange_Leontief_Leontief(CD)_2_2, MWG, P541, 15.B.9
Example.MWG.Exercise.15.B.9<-function(S0Exg=matrix(c(30, 0,
                                            0, 20),2,2,TRUE)){
  sdm(
    A=function(state){
      result1<-c(1,1)
      result2<-c(1,state$z[2])
      cbind(result1,result2)
      },
    B=diag(2),
    S0Exg=S0Exg,
    GRExg=0,
    z0=c(1,1)
  )
}




#' @export

#labor_CD_3_4, Varian (1992), P357, 18.2.
#column 1: wheat producer;
#column 2: iron producer;
#column 3: laborer 1;
#column 4: laborer 2;
#row 1: wheat;
#row 2: iron;
#row 3: labor;
Example.Varian.Exercise.18.2<-function(){
  sdm(
    A=function(state){
      alpha<-c(2, 3, 1, 1)
      Beta<-matrix(c(0,   0, 0.4, 0.5,
                     0,   0, 0.6, 0.5,
                     1,   1,   0,   0), 3, 4, TRUE)

      CD_A(alpha,Beta, state$p)
    },
    B=matrix(c(1,   0,   0,   0,
               0,   1,   0,   0,
               0,   0,   1,   1), 3, 4, TRUE),
    S0Exg={
      S0Exg<-matrix(NA, 3,4)
      S0Exg[3,3]<-10
      S0Exg[3,4]<-10
      S0Exg
      },
    GRExg=0
  )
}


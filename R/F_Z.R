#' @export

F_Z<-function (A,p,S){
  s<-rowSums(S)
  S_bar<-S
  m<-ncol(S)
  for (tk in 1:length(s)) {
    if (s[tk]!=0) S_bar[tk,]<-S_bar[tk,]/s[tk]
  }

  Z<-dg(1/(t(p)%*%A))%*%t(S_bar)%*%dg(p)%*%A

  tmp<-PF_eig(Z)
  z_structure<-tmp$vec

  zeta<-min(s/(A%*%z_structure))
  z=zeta*z_structure;
  q=A%*%z/s
  list(z=z,q=q)
}

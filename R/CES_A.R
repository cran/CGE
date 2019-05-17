#' @export
CES_A<-function(sigma,alpha,Beta,p){
  #computing CES demand structure matrix
  if (!is.matrix(Beta)) Beta<-cbind(Beta)

  n<-nrow(Beta)
  m<-ncol(Beta)

  A<-matrix(0,n,m)
  #if isnumeric(sigma)&&isnumeric(alpha)&&isnumeric(Beta)&&isnumeric(p)
  #else
  #  A=sym(zeros(n,m));
  #end;

  for (cn in 1:m){
    e1<-1/(1-sigma[cn]);
    e2<-sigma[cn]/(sigma[cn]-1);
    e3<--1/sigma[cn];
    k<-alpha[cn];
    beta<-Beta[ ,cn];
    for (rn in 1:n){
      A[rn,cn]<-1/k* (beta[rn]/p[rn])^e1 * (sum(beta^e1*p^e2)) ^e3
    }
  }
  A
}

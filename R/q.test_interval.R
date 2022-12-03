#' @title A quantile.test with confidence interval(two sided) using R
#' @description A quantile.test with confidence interval(two sided) using R
#' @param x the sample data
#' @param xstar the pre-specified points
#' @param quant the pre-specified quantile
#' @param alternative the test types:two.sided&left.sided&right.sided
#' @param conf.level the confidence level for two.sided test
#' @return The first statistic of quantile.test \code{T1}
#' @return The second statistic of quantile.test \code{T2}
#' @return The p-value of quantile.test \code{p.value}
#' @return the confidence level of quantile.test with two.sided \code{conf.level}
#' @return the confidence interval of quantile.test with two.sided \code{conf.interval}
#' @examples
#' \dontrun{
#' x<-c(199,213,202,193,174,166,248,189,233,195,160,212,176,231,185)
#' q.test_interval(x,xstar=193,quant=0.75,conf.level=0.9,alternative="two.side")
#' q.test_interval(x,xstar=193,quant=0.75,conf.level=0.9,alternative="left.side")
#' q.test_interval(x,xstar=193,quant=0.75,conf.level=0.9,alternative="right.side")
#' }
#' @importFrom stats pbinom qbinom
#' @export
q.test_interval <-function(x,xstar=0,quant=.5,alternative="two.sided",conf.level=NULL){
  n<-length(x)
  p<-quant
  T1<-sum(x<=xstar)
  T2<-sum(x< xstar)
  if (alternative=="left.side"){
    p.value<-1-pbinom(T2-1,n,p)
    r <- s <- clo <- chi <- NULL
  }
  if (alternative=="right.side"){
    p.value<-pbinom(T1,n,p)
    r <- s <- clo <- chi <- NULL
  }
  if (alternative=="two.side"){
    p.value<-2*min(1-pbinom(T2-1,n,p),pbinom(T1,n,p))
    alpha<-1-conf.level
    r <- qbinom(alpha/2,n,p)
    rmin1<-r-1
    alpha1<-pbinom(rmin1,n,p)
    s <- qbinom(1-alpha/2,n,p)+1
    smin1 <- s-1
    alpha2<-1-pbinom(smin1,n,p)
    clo<-sort(x)[r]
    chi<-sort(x)[s]
    conf.level<-1-alpha1-alpha2
  }
  list(T1=T1,T2=T2,p.value=p.value,conf.level=conf.level,conf.interval=c(clo,chi))
}
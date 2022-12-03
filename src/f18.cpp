#include <Rcpp.h>
using namespace Rcpp;

//' @title A simple statistical function generator using Rcpp.
//' @description A simple statistical function generator using Rcpp.
//' @return a density fucntion \code{y1}.
//' @return a distribution fucntion \code{y2}.
//' @return a quantile fucntion \code{y3}.
//' @examples
//' \dontrun{
//' f18()
//' }
//' @export
// [[Rcpp::export]]

List f18(){
  NumericVector x = 
    NumericVector::create(0, 1.96, 2.58);
  NumericVector p = 
    NumericVector::create(0.95, 0.975, 0.995);
  NumericVector y1 = dnorm(x, 0.0, 1.0);
  NumericVector y2 = pnorm(x, 0.0, 1.0);
  NumericVector y3 = qnorm(p, 0.0, 1.0);
  return List::create(Named("y1")=y1,
                      Named("y2")=y2, Named("y3")=y3);
}

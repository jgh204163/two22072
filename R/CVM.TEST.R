#' @title A two-sample Cramer-von Mises test for equal distributions as a permutation test using R.
#' @description A two-sample Cramer-von Mises test for equal distributions as a permutation test using R.
#' @param x the data from the first distribution
#' @param y the data from the second distribution
#' @param R the number of permutation
#' @return  a statistic for the test \code{statistic} 
#' @return  a p-value for the test \code{p_value} 
#' @examples
#' \dontrun{
#' attach(InsectSprays)
#' x1 <- as.vector(count[spray == "A"])
#' x2 <- as.vector(count[spray == "B"])
#' x3 <- as.vector(count[spray == "C"])
#' x4 <- as.vector(count[spray == "D"])
#' x5 <- as.vector(count[spray == "E"])
#' x6 <- as.vector(count[spray == "F"])
#' CVM.TEST(x1, x2, 299)
#' CVM.TEST(x3, x4, 299)
#' }
#' @export
CVM.TEST <- function(x, y, R) {
  n <- length(x)
  m <- length(y)
  z <- c(x, y)
  N <- n + m
  Fn <- Gm <- numeric(N)
  cvm <- numeric(R)
  for (i in 1:N) {
    Fn[i] <- mean(as.integer(z[i] <= x))
    Gm[i] <- mean(as.integer(z[i] <= y))
  }
  cvm0 <- ((n*m)/N) * sum((Fn-Gm)^2)
  for (j in 1:R) {
    k <- sample(1:N)
    Z <- z[k]
    X <- Z[1:n]
    Y <- Z[(n + 1):N]
    for (i in 1:N) {
      Fn[i] <- mean(as.integer(Z[i] <= X))
      Gm[i] <- mean(as.integer(Z[i] <= Y))
    }
    cvm[j] <- ((n*m)/N) * sum((Fn-Gm)^2)
  }
  cvm1 <- c(cvm, cvm0)
  return(list(statistic = cvm0, p_value = mean(cvm1 >= cvm0)))
}

#' Generate Random Distrbution
#'
#' Generates random deviates. \code{aDist} determinines normal or lognormal
#' distribution of the random generation function.
#'
#' @param n_iter Number of oberservations
#' @param aDist Type of Distribution: Normal or Lognormal.
#' @param par1 Vector of Means
#' @param par2 Vector of Standard Deviation
#'
#' @seealso \code{\link[truncnorm]{rtruncnorm}}
#' @seealso \code{\link[stats]{rlnorm}}
#'
#' @importFrom truncnorm rtruncnorm
#' @importFrom stats rlnorm
GenerateRandom <- function(n_iter, aDist, par1, par2){

  #require(truncnorm)
  if(aDist=="Normal")    {  aRandom <- rtruncnorm(n=n_iter,a=0,b=Inf,mean=par1,sd=par2)  }
  if(aDist=="Lognormal") {  aRandom <- rlnorm(n=n_iter,par1,par2)              }

  return (aRandom);
}

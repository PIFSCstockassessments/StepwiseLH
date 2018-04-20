#' @import truncnorm

GenerateRandom <- function(n_iter, aDist, par1, par2){

  #require(truncnorm)
  if(aDist=="Normal")    {  aRandom <- rtruncnorm(n=n_iter,a=0,b=Inf,mean=par1,sd=par2)  }
  if(aDist=="Lognormal") {  aRandom <- rlnorm(n=n_iter,par1,par2)              }

  return (aRandom);
}

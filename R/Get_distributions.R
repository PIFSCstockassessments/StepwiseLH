#' Just a test message
#'
#'
#' @import data.table
#' @import truncnorm
#' @import MASS

Get_distributions <- function(Family, Lmax.mean, Lmax.SD, n_iter=5000){


  coefs <- Get_coefficients(Family, Lmax.mean)

  Lmax <- vector(length=n_iter); Linf <- vector(length=n_iter); K    <- vector(length=n_iter); A0<- vector(length=n_iter)
  M    <- vector(length=n_iter); Amax <- vector(length=n_iter); Lmat <- vector(length=n_iter); Amat <- vector(length=n_iter)

  for(i in 1:n_iter)
  {
    Lmax[i] <- rtruncnorm(1,a=0,b=Inf,Lmax.mean,Lmax.SD)
    Linf[i] <- Get_randomLH(coefs$LinfFunc,coefs$LinfDist,coefs$Linf_coef,coefs$Linf_cov,coefs$Linf_error, Lmax[i])
    K[i]    <- Get_randomLH(coefs$KFunc,coefs$KDist,coefs$K_coef,coefs$K_cov,coefs$K_error, Linf[i])
    A0[i]   <- coefs$A0_coef[1]
    M[i]    <- Get_randomLH(coefs$MFunc,coefs$MDist,coefs$M_coef,coefs$M_cov,coefs$M_error, K[i], Lmax[i])
    Amax[i] <- -log(0.05)/M[i]
    LLambda <- Linf[i]*(1-exp(-K[i]*(Amax[i]-A0[i])))
    Lmat[i] <- Get_randomLH(coefs$MatFunc,coefs$MatDist,coefs$Mat_coef,coefs$Mat_cov,coefs$Mat_error, LLambda)
    Amat[i] <- A0[i]-1/K[i]*log(1-Lmat[i]/Linf[i])
  }

  Out <- data.table( cbind(Lmax,Linf,K,A0,Lmat,Amat,M,Amax) )

  # Some descriptive statistics and graphs
  par(mfrow=c(2,2))
  hist(Out$Linf)
  hist(Out$K)
  hist(Out$M)
  hist(Out$Lmat)
  print(summary(Out))

  return(Out)

}

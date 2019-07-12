#' Family taxon distributions
#'
#' Returns Life History distributions based on a given Family taxon.
#'
#' @param Family_Input Selection of available family taxon.
#' @param Lmax.mean Mean of 'largest locally measured length'.
#' @param Lmax.SD Standard Deviation of 'largest locally measured length'.
#' @param M_method Select how M is calculated. Default is 0.04.
#' @param n_iter Number of obervations. Default is 5000.
#' @param dist_summary Option for descriptive statistics and graphs. Default is TRUE.
#'
#' @import data.table
#' @import truncnorm
#' @import MASS
#' @importFrom graphics hist par
#' @export

Get_distributions <- function(Family_Input, Lmax.mean, Lmax.SD, M_method=0.04, n_iter=5000, dist_summary=TRUE){

  if(is.numeric(Family_Input)){
  Family_Names <- c("Acanthuridae","Carangidae","Lethrinidae","Lutjanidae","Mullidae","Scaridae")
  Family       <- Family_Names[Family_Input]
  } else {Family <- Family_Input}

  coefs <- Get_coefficients(Family, Lmax.mean)

  Lmax <- vector(length=n_iter); Linf <- vector(length=n_iter); K    <- vector(length=n_iter); A0<- vector(length=n_iter)
  M    <- vector(length=n_iter); Amax <- vector(length=n_iter); Lmat <- vector(length=n_iter); Amat <- vector(length=n_iter)

  for(i in 1:n_iter)
  {
    Lmax[i] <- rtruncnorm(1,a=0,b=Inf,Lmax.mean,Lmax.SD)
    Linf[i] <- Get_randomLH(coefs$LinfFunc,coefs$LinfDist,coefs$Linf_coef,coefs$Linf_cov,coefs$Linf_error, Lmax[i])
    K[i]    <- Get_randomLH(coefs$KFunc,coefs$KDist,coefs$K_coef,coefs$K_cov,coefs$K_error, Linf[i])
    A0[i]   <- coefs$A0_coef[1]s
    M[i]    <- Get_randomLH(coefs$MFunc,coefs$MDist,coefs$M_coef,coefs$M_cov,coefs$M_error, K[i], Lmax[i])
    Amax[i] <- -log(0.05)/M[i]

    # Select how M is calculated
    if(is.numeric(M_method)) {
      M[i] <- -log(M_method)/Amax[i]        # rule-of-thumb approach where M=-log(Survivorship)/Amax see Hewitt & Hoenig (2005) equation (2). Default is S=0.04 based on Nadon et al. (2015).
    }else if(M_method=="Then_2014") {
      M[i] <- 4.899*Amax[i]^-0.916          # new equation from Then et al. (2014)
    }else if(M_method=="Pauly_Then_2014") {
      M[i] <- 4.118*K[i]^0.73*Linf[i]^-0.33 # updated Pauly (1980) equation from Then et al. (2014)
    }else{
      stop("Invalid M method")
    }

    LLambda <- Linf[i]*(1-exp(-K[i]*(Amax[i]-A0[i])))
    Lmat[i] <- Get_randomLH(coefs$MatFunc,coefs$MatDist,coefs$Mat_coef,coefs$Mat_cov,coefs$Mat_error, LLambda)
    Amat[i] <- A0[i]-1/K[i]*log(1-Lmat[i]/Linf[i])

  }

  Out <- data.table( cbind(Lmax,Linf,K,A0,Lmat,Amat,M,Amax) )

  if(dist_summary){
    # Some descriptive statistics and graphs
    Linf <- Out[,Linf]
    K <- Out[,K]
    M <- Out[,M]
    Lmat <- Out[,Lmat]

    par(mfrow=c(2,2))
    hist(Linf)
    hist(K)
    hist(M)
    hist(Lmat)

    out_summary <- summary(Out)
    print(out_summary)
  }

  return(Out)

}

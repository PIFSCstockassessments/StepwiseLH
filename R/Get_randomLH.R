Get_randomLH <- function(aFunction, aDist, coef, cov, error, x_value1, x_value2){

  aRandomCoef <- mvrnorm(n=1, mu=coef,Sigma=cov)

  # Expected value
  if(aFunction=="Linear")     { E <- aRandomCoef[1]+aRandomCoef[2]*x_value1       }
  if(aFunction=="Expo")       { E <- aRandomCoef[1]+aRandomCoef[2]*x_value1       }
  if(aFunction=="Power")      { E <- aRandomCoef[1]+aRandomCoef[2]*log(x_value1)  }
  if(aFunction=="Power_2var") { E <- aRandomCoef[1]+aRandomCoef[2]*log(x_value1)+aRandomCoef[3]*log(x_value2) }

  # Error distribution
  if(aDist=="Normal")  {  aRandom <- rtruncnorm(n=1,a=0,b=Inf,mean=E,sd=error)    }

  if(aDist=="Lognormal")
  {
    if(aFunction=="Linear")                                               { aRandom <- rlnorm(n=1,log(E),error) }
    else if(aFunction=="Expo"|aFunction=="Power"|aFunction=="Power_2var") { aRandom <- rlnorm(n=1,E,error)      }
  }

  return (aRandom);
}

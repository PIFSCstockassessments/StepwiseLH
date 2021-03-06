% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenerateRandom.R
\name{GenerateRandom}
\alias{GenerateRandom}
\title{Generate Random Distrbution}
\usage{
GenerateRandom(n_iter, aDist, par1, par2)
}
\arguments{
\item{n_iter}{Number of oberservations}

\item{aDist}{Type of Distribution: Normal or Lognormal.}

\item{par1}{Vector of Means}

\item{par2}{Vector of Standard Deviation}
}
\description{
Generates random deviates. \code{aDist} determinines normal or lognormal
distribution of the random generation function.
}
\seealso{
\code{\link[truncnorm]{rtruncnorm}}

\code{\link[stats]{rlnorm}}
}

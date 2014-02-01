\name{optimsimplex.log}
\alias{optimsimplex.log}
\title{
  Optimsimplex Logging
}
\description{
  This function prints a message to screen (or log file).
}
\usage{
  optimsimplex.log(this = NULL, msg = NULL)
}
\arguments{
  \item{this}{An simplex object.}
  \item{msg}{A message to print.}
}
\value{
  Do not return any value but print \code{msg} to screen if the \code{verbose}
  in \code{this} is set to 1.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex}}
}
\keyword{ method }

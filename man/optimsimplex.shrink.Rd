\name{optimsimplex.shrink}
\alias{optimsimplex.shrink}
\title{
  Simplex Shrink
}
\description{
  This function shrinks the simplex with given coefficient sigma and returns an
  updated simplex. The shrink is performed with respect to the first point in
  the simplex.
}
\usage{
  optimsimplex.shrink(this = NULL, fun = NULL, sigma = 0.5, data = NULL)
}
\arguments{
  \item{this}{An simplex object}
  \item{fun}{The function to compute at vertices. The function is expected to
    have the following input and output arguments:
    \tabular{l}{
      myfunction <- function(x, this)\{ \cr
        ... \cr
        return(list(f=f,this=this)) \cr
      \}
    }
    where x is a row vector and this a user-defined data, i.e. the \code{data}.}
  \item{sigma}{The shrinkage coefficient. The default value is 0.5.}
  \item{data}{A user-defined data passed to the function. If data is provided,
    it is passed to the callback function both as an input and output argument.
    \code{data} may be used if the function uses some additionnal parameters. It
    is returned as an output parameter because the function may modify the data
    while computing the function value. This feature may be used, for example,
    to count the number of times that the function has been called.}
}
\value{
  Return a list with the following elements: \describe{
    \item{this}{The updated simplex object.}
    \item{data}{The updated user-defined data.}
  }
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex}}
}
\keyword{ method }

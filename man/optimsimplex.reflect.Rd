\name{optimsimplex.reflect}
\alias{optimsimplex.reflect}
\title{
  Simplex Reflection
}
\description{
  This function returns a new simplex by reflection of the current simplex with
  respect to the first vertex in the simplex. This move is used in the centered
  simplex gradient.
}
\usage{
  optimsimplex.reflect(this = NULL, fun = NULL, data = NULL)
}
\arguments{
  \item{this}{An simplex object.}
  \item{fun}{The function to compute at vertices. The function is expected to
    have the following input and output arguments:
    \tabular{l}{
      myfunction <- function(x, this)\{ \cr
        ... \cr
        return(list(f=f,this=this)) \cr
      \}
    }
    where x is a row vector and this a user-defined data, i.e. the \code{data}
    argument.}
  \item{data}{A user-defined data passed to the function. If data is provided,
    it is passed to the callback function both as an input and output argument.
    \code{data} may be used if the function uses some additionnal parameters. It
    is returned as an output parameter because the function may modify the data
    while computing the function value. This feature may be used, for example,
    to count the number of times that the function has been called.}
}
\value{
  Return a list with the following elements: \describe{
    \item{r}{The reflected simplex object.}
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

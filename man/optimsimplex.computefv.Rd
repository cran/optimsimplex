\name{Function evaluations}
\alias{optimsimplex.computefv}
\alias{optimsimplex.compsomefv}
\title{
  Computation of Function Value(s)
}
\description{
  These functions compute the value of the function at the vertices points
  stored in the current simplex object and stored them back into the simplex
  object. \code{optimsimplex.computefv} determines how many vertices are stored
  in the simplex object and delegates the calculation of the function values to
  \code{optimsimplex.compsomefv}.
}
\usage{
  optimsimplex.computefv(this = NULL, fun = NULL, data = NULL)
  optimsimplex.compsomefv(this = NULL, fun = NULL, indices = NULL, data = NULL)
}
\arguments{
  \item{this}{The current simplex object, containing the nbve x n matrix of
    vertice coordinates (i.e. \code{x} element), where n is the
    dimension of the space and nbve the number of vertices.}
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
  \item{indices}{A vector of increasing integers from 1 to nbve.}
}
\value{
  \code{optimsimplex.computefv} and \code{optimsimplex.compsomefv} return a
  list with the following elements: \describe{
    \item{this}{The updated simplex object.}
    \item{data}{The updated user-defined data.}
  }
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex.new}}
}
\keyword{ method }

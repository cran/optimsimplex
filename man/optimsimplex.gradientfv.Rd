\name{Simplex gradient}
\alias{optimsimplex.gradientfv}
\alias{optimsimplex.gradcenter}
\alias{optimsimplex.gradforward}
\title{
  Simplex Gradient
}
\description{
  \code{optimsimplex.gradientfv} determines the simplex gradient of the function
  which is computed by the secondary functions \code{optimsimplex.gradcenter}
  and \code{optimsimplex.gradforward}.
}
\usage{
  optimsimplex.gradientfv(this = NULL, fun = NULL, method = "forward",
                          data = NULL)
  optimsimplex.gradcenter(this = NULL, fun = NULL, data = NULL)
  optimsimplex.gradforward(this = NULL)
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
    where x is a row vector and this a user-defined data, i.e. the \code{data}
    argument.}
  \item{method}{The method used to compute the simplex gradient. Two methods
    are available: 'forward' and 'centered'. The 'forward' method uses the
    current simplex to compute the gradient (using \code{optimsimplex.dirmat}
    and  \code{optimsimplex.deltafv}). The 'centered' method creates an
    intermediate simplex and computes the average.}
  \item{data}{A user-defined data passed to the function. If data is provided,
    it is passed to the callback function both as an input and output argument.
    \code{data} may be used if the function uses some additionnal parameters. It
    is returned as an output parameter because the function may modify the data
    while computing the function value. This feature may be used, for example,
    to count the number of times that the function has been called.}
}
\value{
  \code{optimsimplex.gradientfv} returns a list with the following elements:
  \describe{
    \item{g}{A column vector of function gradient (with length \code{this$n}).}
    \item{data}{The updated user-defined data.}
  }

  \code{optimsimplex.gradcenter} returns a list with the following elements:
  \describe{
    \item{g}{A column vector of function gradient (with length \code{this$n}).}
    \item{data}{The updated user-defined data.}
  }
  
  \code{optimsimplex.gradforward} returns a column vector of function gradient
  (with length \code{this$n}).
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex.new}},
  \code{\link[optimsimplex]{optimsimplex.dirmat}},
  \code{\link[optimsimplex]{optimsimplex.deltafv}}
}
\keyword{ method }
\name{optimsimplex.print}
\alias{optimsimplex.print}
\alias{optimsimplex.tostring}
\title{
  Simplex Formatting and Display
}
\description{
  \code{optimsimplex.tostring} formats the coordinates and function values in a
  character vector.
  
  \code{optimsimplex.print} displays to screen the content of the current
  simplex with dimensions, coordinates and function values. This function calls
  \code{optimsimplex.tostring} to format the content of the simplex.
}
\usage{
  optimsimplex.print(this = NULL)
  optimsimplex.tostring(this = NULL)
}
\arguments{
  \item{this}{A simplex object.}
}
\value{
  \code{optimsimplex.tostring} returns a vector of character string of length
  nbve, where nbve is the number of vertices.
  
  \code{optimsimplex.print} does not return any value but print to screen (or
  log file) the content of the current simplex.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\examples{
  opt <- optimsimplex.new(method='axes',x0=1:5)$newobj
  optimsimplex.tostring(opt)
  optimsimplex.print(opt)
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex.new}}
}
\keyword{ method }

\name{optimsimplex.destroy}
\alias{optimsimplex.destroy}
\title{
  Erase Simplex Object 
}
\description{
  This function erases the coordinates of the vertices (\code{x}) and the
  function values (\code{fv}) in a simplex object
}
\usage{
  optimsimplex.destroy(this = NULL)
}
\arguments{
  \item{this}{A simplex object.}
}
\value{
  Return an updated simplex object for which the content of the \code{x} and
  \code{fv} elements were set to NULL.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex}}
}
\keyword{ method }


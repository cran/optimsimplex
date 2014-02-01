\name{Set functions}
\alias{optimsimplex.setall}
\alias{optimsimplex.setallfv}
\alias{optimsimplex.setallx}
\alias{optimsimplex.setfv}
\alias{optimsimplex.setn}
\alias{optimsimplex.setnbve}
\alias{optimsimplex.setve}
\alias{optimsimplex.setx}
\title{
  Optimsimplex Set Function Class
}
\description{
  The functions assign content to various elements of a simplex object:
  \describe{
    \item{\code{optimsimplex.setall}}{Set all the coordinates and the function
      values of all the vertices.}
    \item{\code{optimsimplex.setallfv}}{Set all the function values of all the
      vertices.}
    \item{\code{optimsimplex.setallx}}{Set all the coordinates of all the
      vertices.}
    \item{\code{optimsimplex.setfv}}{Set the function value at a givenindex.}
    \item{\code{optimsimplex.setn}}{Set the dimension of the space of the
      simplex.}
    \item{\code{optimsimplex.setnbve}}{Set the number of vertices of the
      simplex.}
    \item{\code{optimsimplex.setve}}{Set the coordinates of the vertex and the
      function values at a given index in the current simplex.}
    \item{\code{optimsimplex.setx}}{Set the coordinates of the vertex at a given
      index in the current simplex.}
  }
}
\usage{
  optimsimplex.setall(this = NULL, simplex = NULL)
  optimsimplex.setallfv(this = NULL, fv = NULL)
  optimsimplex.setallx(this = NULL, x = NULL)
  optimsimplex.setfv(this = NULL, ive = NULL, fv = NULL)
  optimsimplex.setn(this = NULL, n = NULL)
  optimsimplex.setnbve(this = NULL, nbve = NULL)
  optimsimplex.setve(this = NULL, ive = NULL, fv = NULL, x = NULL)
  optimsimplex.setx(this = NULL, ive = NULL, x = NULL)
}
\arguments{
  \item{this}{A simplex object.}
  \item{simplex}{The simplex to set. It is expected to be a nbve x n+1 matrix
    where n is the dimension of the space, nbve is the number of vertices and
    with the following content: \itemize{
      \item \code{simplex[k,1]} is the function value of the vertex k, with k =
        1 to nbve,
      \item \code{simplex[k,2:(n+1)]} is the coordinates of the vertex k, with
        k = 1 to nbve.
    }
  }
  \item{fv}{A row vector of function values; \code{fv[k]} is expected to be the
    function value for the vertex k, with k = 1 to nbve. For
    \code{optimsimplex.setfv}, \code{fv} is expected to be a numerical scalar.}
  \item{x}{The nbve x n matrix of vertice coordinates; the vertex is expected to
    be stored in \code{x[k,1:n]}, with k = 1 to nbve. For
    \code{optimsimplex.setve} and \code{optimsimplex.setx}, \code{x} is expected
    to be a row matrix.}
  \item{ive}{Vertex index.}
  \item{n}{The dimension of the space of the simplex.}
  \item{nbve}{The number of vertices of the simplex.}
}
\value{
  Return a updated simplex object \code{this}.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex}}
}
\keyword{ method }

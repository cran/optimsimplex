\name{Get functions}
\alias{optimsimplex.getall}
\alias{optimsimplex.getallfv}
\alias{optimsimplex.getallx}
\alias{optimsimplex.getfv}
\alias{optimsimplex.getn}
\alias{optimsimplex.getnbve}
\alias{optimsimplex.getve}
\alias{optimsimplex.getx}
\title{
  Optimsimplex Get Function Class
}
\description{
  The functions extract the content to various elements of a simplex object:
  \describe{
    \item{\code{optimsimplex.getall}}{Get all the coordinates and the function
      values of all the vertices.}
    \item{\code{optimsimplex.getallfv}}{Get all the function values of all the
      vertices.}
    \item{\code{optimsimplex.getallx}}{Get all the coordinates of all the
      vertices.}
    \item{\code{optimsimplex.getfv}}{Get the function value at a given index.}
    \item{\code{optimsimplex.getn}}{Get the dimension of the space of the
      simplex.}
    \item{\code{optimsimplex.getnbve}}{Get the number of vertices of the
      simplex.}
    \item{\code{optimsimplex.getve}}{Get the vertex at a given index in the
      current simplex.}
    \item{\code{optimsimplex.getx}}{Get the coordinates of the vertex at a given
      index in the current simplex.}
  }
}
\usage{
  optimsimplex.getall(this = NULL)
  optimsimplex.getallfv(this = NULL)
  optimsimplex.getallx(this = NULL)
  optimsimplex.getfv(this = NULL, ive = NULL)
  optimsimplex.getn(this = NULL)
  optimsimplex.getnbve(this = NULL)
  optimsimplex.getve(this = NULL, ive = NULL)
  optimsimplex.getx(this = NULL, ive = NULL)
}
\arguments{
  \item{this}{A simplex object.}
  \item{ive}{Vertex index.}
}
\value{
  \describe{
    \item{\code{optimsimplex.getall}}{Return a nbve x n+1 matrix, where n is the
      dimension of the space, nbve is the number of vertices and with the
      following content: \itemize{
        \item \code{simplex[k,1]} is the function value of the vertex k, with k =
          1 to nbve,
        \item \code{simplex[k,2:(n+1)]} is the coordinates of the vertex k, with
          k = 1 to nbve.
      }
    }
    \item{\code{optimsimplex.getallfv}}{Return a row vector of function values,
      which k^th element is the function value for the vertex k, with k = 1 to
      nbve.}
    \item{\code{optimsimplex.getallx}}{Return a nbve x n matrix of vertice
      coordinates; any given vertex is expected to be stored at row k, with k =
      1 to nbve.}
    \item{\code{optimsimplex.getfv}}{Return a numeric scalar.}
    \item{\code{optimsimplex.getn}}{Return a numeric scalar.}
    \item{\code{optimsimplex.getnbve}}{Return a numeric scalar.}
    \item{\code{optimsimplex.getve}}{Return a list with a 'type' attribute set
      to 'T_VERTEX' and with the following elements: \describe{
        \item{n}{The dimension of the space of the simplex.}
        \item{x}{The coordinates of the vertex at index \code{ive}.}
        \item{fv}{The value of the function at index \code{ive}.}
      }
    }
    \item{optimsimplex.getx}{Return a row vector, representing the coordinates
      of the vertex at index \code{ive}.
    }
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

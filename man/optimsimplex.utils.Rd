\name{optimsimplex.utils}
\alias{optimsimplex.center}
\alias{optimsimplex.check}
\alias{optimsimplex.deltafv}
\alias{optimsimplex.deltafvmax}
\alias{optimsimplex.dirmat}
\alias{optimsimplex.fvmean}
\alias{optimsimplex.fvstdev}
\alias{optimsimplex.fvvariance}
\alias{optimsimplex.size}
\alias{optimsimplex.sort}
\alias{optimsimplex.xbar}
\title{
  Optimsimplex Utility Functions
}
\description{
  These functions enable various calculations and checks on the current simplex:
  \describe{
    \item{\code{optimsimplex.center}}{Compute the center of the current
      simplex.}
    \item{\code{optimsimplex.check}}{Check the consistency of the data in the
      current simplex.}
    \item{\code{optimsimplex.deltafv}}{Compute the vector of function value
      differences with respect to the function value at the first vertex (the
      lowest).}
    \item{\code{optimsimplex.deltafvmax}}{Compute the difference of function
      value between the lowest and the highest vertices. It is expected that the
      first vertex (\code{this$x[1,]}) is associated with the smallest function
      value and that the last vertex (\code{this$x[nbve,]}) is associated with
      the highest function value.}
    \item{\code{optimsimplex.dirmat}}{Compute the matrix of simplex direction,
      i.e. the matrix of differences of vertice coordinates with respect to the
      first vertex.}
    \item{\code{optimsimplex.fvmean}}{Compute the mean of the function values in
      the current simplex.}
    \item{\code{optimsimplex.fvstdev}}{Compute the standard deviation of the
      function values in the current simplex.}
    \item{\code{optimsimplex.fvvariance}}{Compute the variance of the function
      values in the current simplex.}
    \item{\code{optimsimplex.size}}{Determines the size of the simplex.}
    \item{\code{optimsimplex.sort}}{Sort the simplex by increasing order of
      function value, so the smallest function is at the first vertex.}
    \item{\code{optimsimplex.xbar}}{Compute the center of n vertices, by
      excluding the vertex with index \code{iexcl}. The default of \code{iexcl}
      is the number of vertices: in that case, if the simplex is sorted in
      increasing function value order, the worst vertex is excluded.}
  }
}
\usage{
  optimsimplex.center(this = NULL)
  optimsimplex.check(this = NULL)
  optimsimplex.deltafv(this = NULL)
  optimsimplex.deltafvmax(this = NULL)
  optimsimplex.dirmat(this = NULL)
  optimsimplex.fvmean(this = NULL)
  optimsimplex.fvstdev(this = NULL)
  optimsimplex.fvvariance(this = NULL)
  optimsimplex.size(this = NULL, method = NULL)
  optimsimplex.sort(this = NULL)
  optimsimplex.xbar(this = NULL, iexcl = NULL)
}
\arguments{
  \item{this}{The current simplex.}
  \item{method}{The method to use to compute the size of the simplex. The
    available methods are the following: \describe{
      \item{'sigmaplus'}{(this is the default) The sigmamplus size is the
        maximum 2-norm length of the vector from each vertex to the first
        vertex. It requires one loop over the vertices.}
      \item{'sigmaminus'}{The sigmaminus size is the minimum 2-norm length of
        the vector from each vertex to the first vertex. It requires one loop
        over the vertices.}
      \item{'Nash'}{The 'Nash' size is the sum of the norm of the norm-1 length
        of the vector from the given vertex to the first vertex. It requires one
        loop over the vertices.}
      \item{'diameter'}{The diameter is the maximum norm-2 length of all the
        edges of the simplex. It requires 2 nested loops over the vertices.}
    }
  }
  \item{iexcl}{The index of the vertex to exclude in center computation.}
}
\value{
  \describe{
    \item{\code{optimsimplex.center}}{Return a vector of length nbve, where nbve
      is the number of vertices in the current simplex.}
    \item{\code{optimsimplex.check}}{Return an error message if the dimensions
      of the various elements of the current simplex do not match.}
    \item{\code{optimsimplex.deltafv}}{Return a column vector of length nbve-1.}
    \item{\code{optimsimplex.deltafvmax}}{Return a numeric scalar.}
    \item{\code{optimsimplex.dirmat}}{Return a n x n numeric matrix, where n is
      the dimension of the space of the simplex.}
    \item{\code{optimsimplex.fvmean}}{Return a numeric scalar.}
    \item{\code{optimsimplex.fvstdev}}{Return a numeric scalar.}
    \item{\code{optimsimplex.fvvariance}}{Return a numeric scalar.}
    \item{\code{optimsimplex.size}}{Return a numeric scalar.}
    \item{\code{optimsimplex.sort}}{Return an updated simplex object.}
    \item{\code{optimsimplex.xbar}}{Return a row vector of length n.}
  }
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\references{
  "Compact Numerical Methods For Computers - Linear Algebra and Function
  Minimization", J.C. Nash, 1990, Chapter 14. Direct Search Methods

  "Iterative Methods for Optimization", C.T. Kelley, 1999, Chapter 6., section
  6.2
}
\seealso{
  \code{\link[optimsimplex]{optimsimplex}}
}
\keyword{ method }

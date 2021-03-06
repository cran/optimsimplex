\name{osimplex}
\alias{osimplex}
\alias{vertex}
\alias{print.osimplex}
\alias{is.osimplex}
\alias{print.vertex}
\alias{is.vertex}
\title{
  S3 osimplex and vertex classes
}
\description{
  These functions support the S3 classes 'osimplex' and 'vertex'. They are 
  intended to either create objects of these classes or check if an object is 
  of these classes
}
\usage{
  osimplex(verbose,x,n,fv,nbve)
  
  vertex(x,n,fv)
  
  \method{print}{osimplex}(x,...)
  
  \method{print}{vertex}(x,...)
  
  \method{is}{osimplex}(x)
  
  \method{is}{vertex}(x)
}
\arguments{
  \item{verbose}{The verbose option, controlling the amount of messages}
  \item{x}{The coordinates of the vertices, with size nbve x n in a simplex 
    object or 1 x n in a vertex.}
  \item{n}{The dimension of the space.}
  \item{fv}{The values of the function at given vertices. It is a column
        matrix of length nbve in a simplex or a single value in a vertex.}
  \item{nbve}{The number of vertices in a simplex.}
  \item{...}{optional arguments to 'print' or 'plot' methods.}
}
\details{
  A simplex of size n x nbve is essentially a collection of vertex of size n. 
}
\value{
  \code{osimplex} returns a list with the following elements: verbose, x, n, fv, 
  and nbve.
  \code{vertex} returns a list with the following elements: x, n, and fv.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }

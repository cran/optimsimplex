\name{optimsimplex.new}
\alias{optimsimplex.new}
\alias{optimsimplex.coords}
\alias{optimsimplex.axes}
\alias{optimsimplex.pfeffer}
\alias{optimsimplex.randbounds}
\alias{optimsimplex.spendley}
\alias{optimsimplex.oriented}
\title{
  Creates a Simplex Object
}
\description{
  \code{optimsimplex.new} creates a simplex list object which contains, among
  other elements, a matrix of vertices and a vector of function values
  calculated at those vertices. The object is actually created by a secondary
  function based upon the value of the \code{method} argument:\describe{
    \item{NULL}{-> \code{optimsimplex.coords}}
    \item{'axes'}{-> \code{optimsimplex.axes}}
    \item{'pfeffer'}{-> \code{optimsimplex.pfeffer}}
    \item{'randbounds'}{->  \code{optimsimplex.randbounds}}
    \item{'spendley'}{->  \code{optimsimplex.spendley}}
    \item{'oriented'}{-> \code{optimsimplex.oriented}}
  }
}
\usage{
  optimsimplex.new(coords = NULL, fun = NULL, data = NULL, method = NULL,
                   x0 = NULL, len = NULL, deltausual = NULL, deltazero = NULL,
                   boundsmax = NULL, boundsmin = NULL, nbve = NULL,
                   simplex0 = NULL)
  optimsimplex.coords(coords = NULL, fun = NULL, data = NULL)
  optimsimplex.axes(x0 = NULL, fun = NULL, len = NULL, data = NULL)
  optimsimplex.pfeffer(x0 = NULL, fun = NULL, deltausual = NULL,
                       deltazero = NULL, data = NULL)
  optimsimplex.randbounds(x0 = NULL, fun = NULL, boundsmin = NULL,
                          boundsmax = NULL, nbve = NULL, data = NULL)
  optimsimplex.spendley(x0 = NULL, fun = NULL, len = NULL, data = NULL)
  optimsimplex.oriented(simplex0 = NULL, fun = NULL, data = NULL)
}
\arguments{
  \item{coords}{The matrix of point estimate coordinates in the simplex. The
    coords matrix is expected to be a nbve x n matrix, where n is the dimension
    of the space and nbve is the number of vertices in the simplex, with nbve>=
    n+1. Only used if \code{method} is set to NULL.}
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
  \item{method}{The method used to create the new optimsimplex object, either
    'axes', 'pfeffer', 'randbounds', 'spendley' or 'oriented'.}
  \item{x0}{The initial point estimates, as a row vector of length n.}
  \item{len}{The dimension of the simplex. If length is a value, that unique
    length is used in all directions. If length is a vector with n values, each
    length is used with the corresponding direction. Only used if \code{method}
    is set to 'axes' or 'spendley'.}
  \item{deltausual}{The absolute delta for non-zero values. Only used if
    \code{method} is set to 'pfeffer'.}
  \item{deltazero}{The absolute delta for zero values. Only used if
    \code{method} is set to 'pfeffer'.}
  \item{boundsmin}{A vector of minimum bounds. Only used if \code{method} is
    set to 'randbounds'.}
  \item{boundsmax}{A vector of maximum bounds. Only used if \code{method} is
    set to 'randbounds'.}
  \item{nbve}{The total number of vertices in the simplex. Only used if
    \code{method} is set to 'randbounds'.}
  \item{simplex0}{The initial simplex. Only used if \code{method} is set to
    'oriented'.}
}
\details{
  All arguments of \code{optimsimplex.new} are optional. If no input is provided,
  the new simplex object is empty.

  If \code{method} is NULL, the new simplex object is created by
  \code{optimsimplex.coords}. If \code{coords} is NULL, the simplex object is
  empty; otherwise, \code{coords} is used as the initial vertice coordinates
  in the new simplex.

  If \code{method} is set to 'axes', the new simplex object is created by
  \code{optimsimplex.axes}. The initial vertice coordinates are stored in a nbve
  x n matrix built as follows:\tabular{llcccrclcccr}{
    [,1] \tab| \tab x0[1] \tab.... \tab x0[n] \tab | \tab \tab | \tab len[1]
      \tab... \tab 0 \tab| \cr
    [,.] \tab| \tab... \tab... \tab... \tab | \tab + \tab | \tab... \tab...
      \tab... \tab| \cr
    [,nbve] \tab| \tab x0[1] \tab... \tab x0[n] \tab | \tab \tab | \tab 0
      \tab... \tab len[n] \tab| \cr
  }
  
  If \code{method} is set to 'pfeffer', the new simplex object is created by
  \code{optimsimplex.pfeffer} using the Pfeffer's method, i.e. a relative delta
  for non-zero values and an absolute delta for zero values.

  If \code{method} is set to 'randbounds', the new simplex object is created by
  \code{optimsimplex.randbounds}. The initial vertice coordinates are stored in
  a nbve x n matrix consisting of the initial point estimates (on the first
  row) and a (nbve-1) x n matrix of randomly sampled numbers between the
  specified the bounds. The number of vertices \code{nbve} in the simplex is
  arbitrary.

  If \code{method} is set to 'spendley', the new simplex object is created by
  \code{optimsimplex.spendley} using the Spendely's method, i.e. a regular
  simplex made of nbve = n+1 vertices. 

  If \code{method} is set to 'oriented', the new simplex object is created by
  \code{optimsimplex.oriented} in sorted order. The new simplex has the same
  sigma- length of the base simplex, but is "oriented" depending on the function
  value. The created simplex may be used, as Kelley suggests, for a restart of
  Nelder-Mead algorithm.
}
\value{
  Return a list with the following elements: \describe{
    \item{newobj}{A list with a 'type' attribute set to 'T_SIMPLEX' and with
    the following elements: \describe{
      \item{verbose}{The verbose option, controlling the amount of messages.
        Set to 0.}
      \item{x}{The coordinates of the vertices, with size nbve x n.}
      \item{n}{The dimension of the space.}
      \item{fv}{The values of the function at given vertices. It is a column
        matrix of length nbve.}
      \item{nbve}{The number of vertices.}
      }
    }
    \item{data}{The updated \code{data} input argument.}
  }
}
\references{
"A Simplex Method for Function Minimization", Nelder, J. A. and Mead, R. The
Computer Journal, January, 1965, 308-313

"Sequential Application of Simplex Designs in Optimisation and Evolutionary
Operation", W. Spendley, G. R. Hext, F. R. Himsworth, Technometrics, Vol. 4, No.
4 (Nov., 1962), pp. 441-461, Section 3.1

"A New Method of Constrained Optimization and a Comparison With Other Methods",
M. J. Box, The Computer Journal 1965 8(1):42-52, 1965 by British Computer
Society

"Detection and Remediation of Stagnation in the Nelder-Mead Algorithm Using a
Sufficient Decrease Condition", SIAM J. on Optimization, Kelley C.T., 1999

"Multi-Directional Search: A Direct Search Algorithm for Parallel Machines", by
E. Boyd, Kenneth W. Kennedy, Richard A. Tapia, Virginia Joanne Torczon,
Virginia Joanne Torczon, 1989, Phd Thesis, Rice University

"Grid Restrained Nelder-Mead Algorithm", Arpad Burmen, Janez Puhan, Tadej Tuma,
Computational Optimization and Applications, Volume 34 , Issue 3 (July 2006),
Pages: 359 - 375

"A convergent variant of the Nelder-Mead algorithm", C. J. Price, I. D. Coope,
D. Byatt, Journal of Optimization Theory and Applications, Volume 113 , Issue 1
(April 2002), Pages: 5 - 19,

"Global Optimization Of Lennard-Jones Atomic Clusters", Ellen Fan, Thesis,
February 26, 2002, McMaster University
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\examples{
  myfun <- function(x,this){return(list(f=sum(x^2),this=this))}
  mat <- matrix(c(0,1,0,0,0,1),ncol=2)
  
  optimsimplex.new()
  optimsimplex.new(coords=mat,x0=1:4,fun=myfun)
  optimsimplex.new(method='axes',x0=1:4,fun=myfun)
  optimsimplex.new(method='pfeffer',x0=1:6,fun=myfun)
  opt <- optimsimplex.new(method='randbounds',x0=1:6,boundsmin=rep(0,6),
                          boundsmax=rep(10,6),fun=myfun)
  opt
  optimsimplex.new(method='spendley',x0=1:6,fun=myfun,len=10)
  optimsimplex.new(method='oriented',simplex=opt$newobj,fun=myfun)
 
}
\keyword{ method }

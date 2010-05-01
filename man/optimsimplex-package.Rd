\name{optimsimplex-package}
\alias{optimsimplex-package}
\alias{optimsimplex}
\docType{package}
\title{
  R port of the Scilab optimsimplex module
}
\description{
  The goal of this package is to provide a building block for optimization
  algorithms based on a simplex. The \pkg{optimsimplex} package may be used in
  the following optimization methods: \itemize{
    \item the simplex method of Spendley et al.,
    \item the method of Nelder and Mead,
    \item the Box's algorithm for constrained optimization,
    \item the multi-dimensional search by Torczon,
    \item etc ...
  }

  \strong{Features}
  The following is a list of features currently provided:
  \itemize{
    \item Manage various simplex initializations \itemize{
      \item initial simplex given by user,
      \item initial simplex computed with a length and along the coordinate
        axes,
      \item initial regular simplex computed with Spendley et al. formula,
      \item initial simplex computed by a small perturbation around the initial
        guess point,
      \item initial simplex computed from randomized bounds.
    }
    \item sort the vertices by increasing function values,
    \item compute the standard deviation of the function values in the simplex,
    \item compute the simplex gradient with forward or centered differences,
    \item shrink the simplex toward the best vertex,
    \item etc...
  }
}
\details{
  \tabular{ll}{
  Package: \tab optimsimplex\cr
  Type: \tab Package\cr
  Version: \tab 1.0\cr
  Date: \tab 2010-05-01\cr
  License: \tab CeCILL-2\cr
  LazyLoad: \tab yes
  }
  See \code{vignette('optimsimplex',package='optimsimplex')} for more
  information.
}
\author{
  Author of Scilab optimsimplex module: Michael Baudin (INRIA - Digiteo)

  Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ package }


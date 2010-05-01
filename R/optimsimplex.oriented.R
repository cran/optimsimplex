# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010 - Sebastien Bihorel
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#
# This source code is a R port of the optimsimplex component
# originally written by Michael Baudin for Scilab.

optimsimplex.oriented <- function(simplex0=NULL,fun=NULL,data=NULL){

  if (simplex0$nbve != simplex0$n+1)
    stop(sprintf(paste('optimsimplex.oriented: The oriented simplex can be computed only with a simplex',
                       'made of n+1 points, but the dimension is %d and the number of vertices is %d.',sep=''),
                 simplex0$n,simplex0$nbve),
         call.=FALSE)

  if (!is.null(fun))
    assert.typefunction(var=fun,varname='fun',ivar=2)
  tmp <- optimsimplex.gradientfv(this=simplex0)
    sgrad <- tmp$g
  rm(tmp)
  ssize <- optimsimplex.size(this=simplex0,method='sigmaminus')
  n <- simplex0$n

  # Compute the betas
  ipos <- which(sgrad >= 0.0)
  ineg <- which(sgrad < 0.0)
  betav <- c()
  betav[ipos] <- ssize
  betav[ineg] <- -ssize
  betav <- -0.5 * betav

  # Prepare a matrix with beta as diagonal terms
  mid <- diag(betav)

  # Compute simplex
  newobj <- optimsimplex.new()$newobj
  newobj$n <- simplex0$n
  newobj$nbve <- simplex0$n+1
  newobj$x <- matrix(0,nrow=n+1,ncol=n)
  newobj$fv <- matrix(0,nrow=n+1,ncol=1)

  # Store all points
  x1 <- simplex0$x[1,1:n]
  newobj$x[1:(n+1),1:n] <- matrix(rep(x1,n+1),nrow=n+1,byrow=TRUE)

  # Retrieve the function value for the first simplex
  # This saves one function evaluation
  newobj$fv[1] <- simplex0$fv[1]
  newobj$x[2:(n+1),1:n] <- mid[1:n,1:n] + newobj$x[2:(n+1),1:n]

  # Compute Function Value
  if (!is.null(fun)){
    tmp <- optimsimplex.computefv(this=newobj,fun=fun,data=data)
      newobj <- tmp$this
      if (!is.null(data)) data <- tmp$data
  }

  varargout <- list(newobj=newobj, data=data)

  return(varargout)
}


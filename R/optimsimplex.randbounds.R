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

optimsimplex.randbounds <- function(x0=NULL,fun=NULL,boundsmin=NULL,
                                    boundsmax=NULL, nbve=NULL,data=NULL){

  newobj <- optimsimplex.coords()$newobj
  if (size(x0,1)!=1)
    stop(sprintf('optimsimplex.randbounds: The x0 vector is expected to be a row matrix, but current shape is %d x %d.',
                 size(x0,1),size(x0,2)),
         call.=FALSE)
  if (size(boundsmin,1)!=1)
    stop(sprintf('optimsimplex.randbounds: The boundsmin vector is expected to be a row matrix, but current shape is %d x %d.',
                 size(boundsmin,1),size(boundsmin,2)),
         call.=FALSE)
  if (length(boundsmin)<length(x0))
    stop(sprintf('optimsimplex.randbounds: The boundsmin vector is expected to have %d columns, but current shape is %d x %d.',
                 size(boundsmin,1),size(boundsmin,2)),
         call.=FALSE)
  if (size(boundsmax,1)!=1)
    stop(sprintf('optimsimplex.randbounds: The boundsmax vector is expected to be a row matrix, but current shape %d x %d.',
               size(boundsmax,1),size(boundsmax,2)),
         call.=FALSE)
  if (length(boundsmax)<length(x0))
    stop(sprintf('optimsimplex.randbounds: The boundsmax vector is expected to have at least as many elements as the x0 vector, but current length is %d.',
                 size(boundsmax,1),size(boundsmax,2)),
         call.=FALSE)

  assert.typereal(var=x0,varname='x0',ivar=1)
  assert.typefunction(var=fun,varname='fun',ivar=2)
  assert.typereal(var=boundsmin,varname='boundsmin',ivar=3)
  assert.typereal(var=boundsmax,varname='boundsmax',ivar=4)
  n <- length (x0)
  if (is.null(nbve)){
    nbve <- n + 1
  }else{
    assert.typereal(var=nbve,varname='nbve',ivar=5)
  }
  newobj$n <- n
  newobj$nbve <- nbve
  newobj$x <- matrix(0,nrow=nbve,ncol=n)
  newobj$fv <- matrix(0,nrow=nbve,ncol=1)

  #
  # Set all points
  #
  newobj$x[1,1:n] <- x0[1:n,drop=FALSE]

  #
  # Set points #2 to #nbve, by randomizing the bounds
  #
  bminmat <- matrix(rep(boundsmin[1:n],nbve-1),nrow=nbve-1,byrow=TRUE)
  bmaxmat <- matrix(rep(boundsmax[1:n],nbve-1),nrow=nbve-1,byrow=TRUE)
  thetas <- matrix(runif(n*(nbve-1)),nrow=n,ncol=(nbve-1))
  newobj$x[2:nbve,1:n] <- bminmat + transpose(thetas) * (bmaxmat - bminmat)

  # Compute Function Value
  if (!is.null(fun)){
    tmp <- optimsimplex.computefv(this=newobj,fun=fun,data=data)
      newobj <- tmp$this
      if (!is.null(data)) data <- tmp$data
  }
  varargout <- list(newobj=newobj, data=data)

  return(varargout)
}


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

optimsimplex.pfeffer <- function(x0=NULL,fun=NULL,deltausual=NULL,
                                 deltazero=NULL,data=NULL){

  newobj <- optimsimplex.coords()$newobj
  if (size(x0,1)!=1)
    stop(sprintf('optimsimplex.pfeffer: The x0 vector is expected to be a row matrix, but current shape is %d x %d',
                 size(x0,1),size(x0,2)),
         call.=FALSE)
  if (!is.null(fun))
    assert.typefunction(var=fun,varname='fun',ivar=2)
  if (is.null(deltausual)){
    deltausual <- 0.05
  }
  if (is.null(deltazero)){
    deltazero <- 0.0075
  }
  assert.typereal(var=x0,varname='x0',ivar=1)
  assert.typereal(var=deltausual,varname='deltausual',ivar=3)
  assert.typereal(var=deltazero,varname='deltazero',ivar=4)

  n <- length(x0)
  newobj$n <- n
  newobj$nbve <- n + 1
  newobj$x <- matrix(0,nrow=newobj$nbve,ncol=n)
  newobj$fv <- matrix(0,nrow=newobj$nbve,ncol=1)

  #
  # Set all points
  #
  newobj$x[,1:n] <- matrix(rep(x0[1:n],newobj$nbve),nrow=newobj$nbve,byrow=TRUE)

  #
  # Set points #2 to #n+1
  #
  for (j in 2:(newobj$n+1)){
    if (x0[j-1]==0.0){
      newobj$x[j,j-1] <- deltazero
    }else{
      newobj$x[j,j-1] <- newobj$x[j,j-1] + deltausual * x0[j-1]
    }
  }
  # Compute Function Value
  if (!is.null(fun)){
    tmp <- optimsimplex.computefv(this=newobj,fun=fun,data=data)
      newobj <- tmp$this
      if (!is.null(data)) data <- tmp$data
  }
  varargout <- list(newobj=newobj, data=data)

  return(varargout)
}


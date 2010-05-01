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

optimsimplex.spendley <- function(x0=NULL,fun=NULL,len=NULL,data=NULL){

  newobj <- optimsimplex.coords()$newobj
  if (size(x0,1)!=1)
    stop(sprintf('optimsimplex.spendley: The x0 vector is expected to be a row matrix, but current shape is %d x %d',
                 size(x0,1),size(x0,2)),
         call.=FALSE)
  if (!is.null(fun))
    assert.typefunction(var=fun,varname='fun',ivar=2)
  if (is.null(len)){
   len <- 1
  }else{
    assert.typereal(var=len,varname='len',ivar=3)
    if (size(len,1)!=1 | size(len,2)!=1 )
      stop(sprintf('optimsimplex.spendley: The len vector is expected to be a row matrix, but current shape is %d x %d',
                   size(len,1),size(len,2)),
           call.=FALSE)
  }
  assert.typereal(var=x0,varname='x0',ivar=1)
  n <- length(x0)
  newobj$n <- n
  newobj$nbve <- n + 1
  newobj$x <- matrix(0,nrow=newobj$nbve,ncol=n)
  newobj$fv <- matrix(0,nrow=newobj$nbve,ncol=1)

  #
  # Compute p (diagonal term) , q (off-diagonal term)
  #
  p  <- (n - 1.0 + sqrt(n + 1))/(n * sqrt(2.0))
  q <- (sqrt(n + 1) - 1.0)/(n * sqrt(2.0))

  #
  # Set all points
  #
  nv <- newobj$nbve
  newobj$x[1:nv,] <- matrix(rep(x0[1:n],nv),nrow=nv,byrow=TRUE)
  newobj$x[2:nv,] <- newobj$x[2:nv,] + diag(rep(p,n)) + matrix(q,nrow=n,ncol=n)-
                     diag(rep(q,n))

  # Compute Function Value
  if (!is.null(fun)){
    tmp <- optimsimplex.computefv(this=newobj,fun=fun,data=data)
      newobj <- tmp$this
      if (!is.null(data)) data <- tmp$data
  }
  varargout <- list(newobj=newobj, data=data)

  return(varargout)
}


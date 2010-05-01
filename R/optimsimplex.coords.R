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

optimsimplex.coords <- function(coords=NULL,fun=NULL,data=NULL){

  if (!is.null(coords))
    assert.typereal(var=coords,varname='coords',ivar=1)

  if (!is.null(fun))
    assert.typefunction(var=fun,varname='fun',ivar=1)

  newobj <- list(verbose=0,
                 # The coordinates of the vertices, with size nbve x n
                 x=matrix(),
                 # The dimension of the space
                 n=0,
                 # The function values, with size 1 x nbve
                 fv=matrix(numeric(0),ncol=1),
                 # The number of vertices
                 nbve=0)

  attr(newobj,'type') <- 'T_SIMPLEX' 
  
  #
  # Take input arguments into account
  #
  
  if (!is.null(coords)){
    nbve <- size(coords,1)
    n <- size(coords,2)
    if (nbve<n+1)
      stop(sprintf('optimsimplex.coords: The numbers of columns of coords is %d but is expected to be at least %d',
                   nbve,n+1),
           call.=FALSE)
    newobj$n <- n
    newobj$nbve <- nbve
    newobj$x <- coords[1:nbve,1:n]
    
    if (!is.null(fun)){
      tmp <- optimsimplex.computefv(this=newobj,fun=fun,data=data)
        newobj <- tmp$this
        if (!is.null(data)) data <- tmp$data
    }
  }
  varargout <- list(newobj=newobj, data=data)

  return(varargout)
}


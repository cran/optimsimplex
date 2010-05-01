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

optimsimplex.new <- function(coords=NULL,fun=NULL,data=NULL,method=NULL,
                             x0=NULL,len=NULL,deltausual=NULL,deltazero=NULL,
                             boundsmax=NULL,boundsmin=NULL,nbve=NULL,
                             simplex0=NULL){

  # Check inputs
  varargin <- as.list(match.call())[-1]
  nargin <- length(varargin)

  if (nargin==0){
    newobj <- optimsimplex.coords()$newobj
    varargout <- list(newobj=newobj,data=data)
    return(varargout)
  }
  
  if (!is.null(method)){
    stype <- method
    if(!any(stype==c('axes','spendley','pfeffer','randbounds','oriented')))
      stop(sprintf('optimsimplex.new: Unexpected key %s',stype),call.=FALSE)
    if (stype=='axes'){
      #   newobj <- optimsimplex.new(method="axes",x0=x0,fun=fun,len=len,data=data)
      tmp <- optimsimplex.axes(x0=x0,fun=fun,len=len,data=data)
    }  
    if (stype=='spendley'){
      #   newobj <- optimsimplex.new(method="spendley",x0=x0,fun=fun,len=len,data=data)
      tmp <- optimsimplex.spendley(x0=x0,fun=fun,len=len,data=data)
    }
    if (stype=='pfeffer'){
      #   newobj <- optimsimplex.new(method="pfeffer",x0=x0,fun=fun,deltausual=deltausual,deltazero=deltazeo,data=data)
      tmp <- optimsimplex.pfeffer(x0=x0,fun=fun,deltausual=deltausual,deltazero=deltazero,data=data)
    }
    if (stype=='randbounds'){
      #   newobj <- optimsimplex.new(method="randbounds",x0=x0,fun=fun,boundsmin=boundsmin,boundsmax=boundsmax,nbve=nbve,data=data
      tmp <- optimsimplex.randbounds(x0=x0,fun=fun,boundsmin=boundsmin,boundsmax=boundsmax,nbve=nbve,data=data)
    }
    if (stype=='oriented'){
      #   newobj <- optimsimplex.new(method="oriented",simplex0=simplex0,fun=fun,data=data)
      tmp <- optimsimplex.oriented(simplex0=simplex0,fun=fun,data=data)
    }  
  } else {
    # newobj <- optimsimplex.new(coords=coords,fun=fun,data=data)
    tmp <- optimsimplex.coords(coords=coords,fun=fun,data=data)
  }

  newobj <- tmp$newobj
  if (!is.null(data)) data <- tmp$data

  varargout <- list(newobj=newobj,data=data)
  
  return(varargout)
}


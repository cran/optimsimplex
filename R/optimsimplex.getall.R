# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010-2022 - Sebastien Bihorel
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#
# This source code is a R port of the optimsimplex component
# originally written by Michael Baudin for Scilab.

optimsimplex.getall <- function(this=NULL){

  simplex <- matrix(0,nrow=this$nbve,ncol=this$n+1)
  simplex[1:this$nbve,1] <- this$fv[1:this$nbve,1,drop=FALSE]
  simplex[1:this$nbve,2:this$n+1] <- this$x[1:this$nbve,1:this$n,drop=FALSE]

  return(simplex)

}


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

optimsimplex.tostring <- function(this=NULL){

  str <- c()
  if (this$n==0){
    str <- sprintf('Empty simplex (zero dimension)\n')
    return(str)
  }
  if (this$nbve==0){
    str <- sprintf('Empty simplex (zero vertices)\n')
    return(str)
  }
  if (length(this$x)==0){
    str <- sprintf('Empty simplex (zero coordinates)\n')
    return(str)
  }
  if (length(this$fv)==0){
    str <- sprintf('Empty simplex (zero function values)\n')
    return(str)
  }
  
  for (k in 1:this$nbve){
    # Compute a string for x
    ss <- sprintf('%e',this$x[k,1])
    for (i in 2:this$n){
      ss <- paste(ss,sprintf('%e',this$x[k,i]))
    }
    str[k] <- sprintf('Vertex #%d/%d : fv=%e, x=%s\n',k,this$nbve,this$fv[k],ss)
  }

  return(str)

}


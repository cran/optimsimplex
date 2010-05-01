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

optimsimplex.print <- function(this=NULL){

  if (this$n==0)
    cat('Empty simplex (zero dimension)\n')
  if (this$nbve==0)
    cat('Empty simplex (zero vertices)\n')
  if (is.null(this$x))
    cat('Empty simplex (zero coordinates)\n')
  if (is.null(this$fv))
    cat('Empty simplex (zero function values)\n')

  if (this$n!=0 & this$nbve!=0 & !is.null(this$x) & !is.null(this$fv)){
    cat(sprintf('Dimension: %d\n',this$n))
    cat(sprintf('Number of vertices: %d\n',this$nbve))
    str <- optimsimplex.tostring(this=this)
    for (k in 1:this$nbve){
      cat(sprintf('%s',str[k]))
    }
  }
}


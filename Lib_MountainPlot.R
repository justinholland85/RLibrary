Lib.MountainPlot  <-  function(X,Axis, Cols, PlotNew, Cumulative ){
  
  if(missing(Axis)){Axis  <-  as.numeric(colnames(X))}
  if(length(Axis) == 0){Axis  <-  seq(1,ncol(X)) }
  if(missing(PlotNew)){PlotNew  <-  1 }
  if(missing(Cumulative)){Cumulative  <-  0 }
  
  N.Rows  <-  nrow(X)
  CumSum  <-  rbind(0,array(dim=dim(X)))
 for(i in 1:N.Rows){
  CumSum[i+1, ]  <- CumSum[i, ] + X[i, ]
 }
  
  if(Cumulative == 1){ PlotVal  <-  CumSum } else {PlotVal  <-  rbind(X,0)}
  if(Cumulative == 1){ MaxRow  <-  N.Rows  } else {MaxRow  <-  N.Rows-1}
  
  Ylim  <-  c(0,max(PlotVal))
  Xlim  <-  c(min(Axis),max(Axis))

  if(PlotNew == 1){ plot(Xlim,Ylim,type='n', xaxt='n', yaxt='n', xlab="", ylab="", bty='n')}
  
  for(i in 1:MaxRow){
    YY  <-  c(PlotVal[i+1,], rev(PlotVal[i,]))
    XX  <-  c(Axis, rev(Axis))
    polygon(XX,YY,col=Cols[i])
    
  }
  
}




Lib.PolyPlot  <-  function(X,Y,Col){
  
  XX  <-  c(X[1],X,X[length(X)])
  YY  <-  c(0,Y,0)
  
  polygon(XX,YY,col=Col)
}
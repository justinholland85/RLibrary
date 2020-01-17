Lib.PlotArea   <-  function(X, Y.Lower, Y.Upper, Fill, BorderCol, BorderLwd ){
  
  if(missing(Fill)){Fill   <-  "white"}
  if(missing(BorderCol)){BorderCol   <-  "black"}
  if(missing(BorderLwd)){BorderLwd   <-1}
  
  Poly.X   <-  c(X, rev(X))
  Poly.Y   <-  c(Y.Lower, rev(Y.Upper))
  
  polygon(Poly.X, Poly.Y, col=Fill, border=BorderCol, lwd =  BorderLwd)
  
  
}


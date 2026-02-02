######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.PointInPoly
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################



Lib.PointInPoly    <-  function(P.x, P.y, Poly.x, Poly.y){
  
  N   <-  length(Poly.x)
  
  Inside    <- rep(FALSE, length(P.x)) 
  Boundary  <- rep(FALSE, length(P.x)) 
  
  for(i in 1:N){
    
    #-- Polygon test coordinates
    X.0    <-  Poly.x[i]
    Y.0    <-  Poly.y[i]
    
    if(i < N){
    
      X.1    <-  Poly.x[i + 1]
      Y.1    <-  Poly.y[i + 1]
      
    } else {
      
      X.1    <-  Poly.x[1]
      Y.1    <-  Poly.y[1]  
      
    }  
    
    #-- check intersection
    
    y.InRange    <-  (P.y > Y.0) != (P.y > Y.1) 
    
    Slope.dx.dy  <-  (X.1 - X.0) / (Y.1 - Y.0 + 10^-12)   
    
    dx           <-  (P.y - Y.0) * Slope.dx.dy
    
    X.Intersect  <-  X.0 + dx
    
    x.LeftOf     <-  P.x < X.Intersect
    
    P.Intersect  <-  y.InRange & x.LeftOf
    
    Flip         <-  which(P.Intersect)   
    
    Inside[Flip] <-  !Inside[Flip]
    
    #-- Boundary Test
    
    x.Test       <- min(X.0, X.1) <=  P.x & P.x <= max(X.0, X.1)
    y.Test       <- min(Y.0, Y.1) <=  P.y & P.y <= max(Y.0, Y.1)
    
    # The line test fails on the hozinotal because any X may be vlaid - we only need the range tests
    if(Y.1 != Y.0){ Line.Test    <-  P.x == (X.0 + Slope.dx.dy * (P.y - Y.0))} else {Line.Test <- TRUE}
    
    
    Boundary     <-  Boundary | (Line.Test & x.Test & y.Test)
    
  }
  
  InBound        <- Inside | Boundary
  
  Output         <-  list("Inside"   = Inside, 
                          "Boundary" = Boundary, 
                          "InBound"  = InBound)
  
  return(Output)
  
}
  
  
  
  


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Plots
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Poly.x    <-  c(25,10, 25,75,75) / 100
# Poly.y    <-  c(25,50, 75,75,25) / 100


# P.x       <-  c(Poly.x, runif(1000,0,100) / 100)
# P.y       <-  c(Poly.y, runif(1000,0,100) / 100)

# Inside    <-  Lib.PointInPoly(P.x, P.y, Poly.x, Poly.y)$InBound
# Col       <-  rep('red' ,length(P.x))
# Col[which(Inside)]  <-  'blue'


# Lib.Plot.Blank()
# polygon(Poly.x, Poly.y)
# points(P.x, P.y, cex = 1.5, pch = 16, col = Col)






Lib.HeatLegend  <-  function(Names, Cols, X0, X1, Y0, Y1, Move.X, Move.Y,
                             Eps, Cex, TextCol){
  
  if(missing(X0)){ X0  <-  0}
  if(missing(X1)){ X1  <-  1}
  if(missing(Y0)){ Y0  <-  0}
  if(missing(Y1)){ Y1  <-  1}
  if(missing(Move.X)){ Move.X  <-  0}
  if(missing(Move.Y)){ Move.Y  <-  1}
  if(missing(Eps)){ Eps  <-  0.01}
  if(missing(Cex)){ Cex  <-  0.5}
  if(missing(TextCol)){ TextCol  <-  "white"}
     
  N  <-  length(Names)

  X.dA  <-  seq(0,N-1) * (X1 - X0) / N
  X.dB  <-  seq(1,N) * (X1 - X0) / N
  
  X.A  <-  X0 + X.dA * Move.X + (X1 - X0) * Eps
  X.B  <-  X0 + X.dB * Move.X - (X1 - X0) * Eps + (X1 - X0) * (1 - Move.X)
  
  Y.dA  <-  seq(0, N-1) * (Y1 - Y0) / N
  Y.dB  <-  seq(1, N)   * (Y1 - Y0) / N
  
  Y.A  <-  1 - Y0 - Y.dA * Move.Y - (Y1 - Y0) * Eps
  Y.B  <-  1 - Y0 - Y.dB * Move.Y + (Y1 - Y0) * Eps - (Y1 - Y0) * (1 - Move.Y)
  
  for(i in 1:N){
    
    X  <-  c(X.A[i], X.B[i], X.B[i], X.A[i])
    Y  <-  c(Y.A[i], Y.A[i], Y.B[i], Y.B[i])    
    
    polygon(X, Y, col="white", border=NA)
    polygon(X, Y, col=Cols[i], border=NA)
    
    # Text.X  <-  (mean(X) * Move.X) + (X.A[i] * !Move.X)
    # Text.Y  <-  (mean(Y) * Move.Y) + (Y.A[i] * !Move.Y)
    
    Text.X  <-  mean(X)
    Text.Y  <-  mean(Y)
    
    text(Text.X, Text.Y, labels=Names[i], cex=Cex, col=TextCol)
    
  }
  
}



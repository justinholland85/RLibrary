

Lib.TrafficLightPlot  <-  function(Index, X, Text, X.Lim, ZoneLeft, ZoneRight, X.Lab, Y.Gap, Main,
                                   Par.Mar, Text.Labels, Text.Cols, ZoneLeft.Label, Col.Lights, 
                                   Col.Plot, Text.Labels.Cols, Cex, ZoneLeft.Label.Cex){
   
  
  N  <-  nrow(Index)
  M  <-  ncol(Index) 
  
  if(missing(Par.Mar)){Par.Mar     <-  c(5.1, 2.1, 4.1, 1.1)}
  if(missing(X.Lim)){X.Lim         <-  Lib.Axis.PosNeg(X)$Lim}
  if(missing(ZoneLeft)){ZoneLeft   <-  .1}
  if(missing(ZoneRight)){ZoneRight <-  .1}
  if(missing(Text.Cols)){TextCols}
  if(missing(Text)){Text           <-  cbind(rep("", N))}
  if(missing(Cex)){Cex             <-  2}
  if(missing(ZoneLeft.Label.Cex)){ZoneLeft.Label.Cex <-  1}
  
  
  
  K  <-  ncol(Text) 
  
  if(missing(Text.Labels)){colnames(Text)}
  if(is.null(Text.Labels)){rep("", K)}
  
  if(missing(Main)){Main  <-  ""}
  if(missing(ZoneLeft.Label)){ZoneLeft.Label  <-  ""}

  if(missing(Col.Plot)){Col.Plot      <-  Lib.ColourScheme(24,5, V=.8, Plot = 0)[1,24]}
  if(missing(Col.Lights)){Col.Lights  <-  Lib.ColourScheme(N+1,5, V=.8, Plot = 0)[1,1:N]}
  if(missing(Text.Cols)){Text.Cols                  <-  rep("black", K)}
  if(missing(Text.Labels.Cols)){Text.Labels.Cols    <-  rep("black", K)}
  
  
  if(missing(Col.Lights)){Col.Lights  <-  c("red", "green")}
  if(missing(Col.Plot)){Col.Plot      <-  "blue"}

  
  # par(mar = c(5.1, 4.1, 4.1, 2.1))
  par(mar = Par.Mar)
  par(xpd=NA)


  X.ZoneLeft.0   <- 0
  X.ZoneLeft.1   <- ZoneLeft
  
  X.ZoneRight.0   <- 1 - ZoneRight
  X.ZoneRight.1   <- 1
  
  Y.Ends      <-  seq(0, 1, 1 / N)
  Y.Centres   <-  rev((Y.Ends[1:N] + Y.Ends[2:(N + 1)]) / 2 )
  
  X.ZoneLeft.Ends    <-  seq(X.ZoneLeft.0, X.ZoneLeft.1, (X.ZoneLeft.1 - X.ZoneLeft.0) / M) 
  X.ZoneLeft.Centres <-  (X.ZoneLeft.Ends[1:M] + X.ZoneLeft.Ends[2:(M + 1)]) / 2  
  
  X.ZoneRight.Ends    <-  seq(X.ZoneRight.0, X.ZoneRight.1, (X.ZoneRight.1 - X.ZoneRight.0) / K) 
  X.ZoneRight.Centres <-  (X.ZoneRight.Ends[1:K] + X.ZoneRight.Ends[2:(K + 1)]) / 2  
  
  

  plot(c(0,1), c(0,1), type='n', bty='n',xlab="",ylab="",xaxt='n' ,yaxt='n', main = Main)
  
  for(i in 1:N){
    for(j in 1:M){
      
      points(X.ZoneLeft.Centres[j], Y.Centres[i], pch = 21, bg = Col.Lights[Index[i,j] + 1], cex = Cex)
      
    }}
  
  
  X.Plot.0     <-  X.ZoneLeft.1
  X.Plot.1     <-  X.ZoneRight.0
  
  dX           <-  X.Lim[2] - X.Lim[1]
  dY           <-  (Y.Ends[2] - Y.Ends[1]) / 2 * (1 - Y.Gap)
  
  X.Scale      <-  (X.Plot.1  - X.Plot.0) / dX
  
  
  X.Origin     <-  X.ZoneLeft.1 + (0 - X.Lim[1]) * X.Scale  
  
  
  for(i in 1:N){
    
    X.1  <- X.ZoneLeft.1 + (X[i] - X.Lim[1]) * X.Scale
    
    Y.1 <- Y.Centres[i] + dY
    Y.0 <- Y.Centres[i] - dY
    
    polygon(c(X.Origin, X.Origin, X.1, X.1), c(Y.0, Y.1, Y.1,Y.0 ), col=Col.Plot)
    
  }
  
  
  X.Axis   <-  Lib.Axis.Range(X.Lim)  
  
  axis(1, at = (X.Plot.0 + (X.Axis$Axis - X.Lim[1]) * X.Scale ), labels = X.Axis$Axis)
  
  points(c(X.Origin, X.Origin), c(0,1), type = 'l', lwd = 2)
  
  
  for(i in 1:N){
    for(j in 1:K){
      
      text(X.ZoneRight.Centres[j],  Y.Centres[i], Text[i,j], col = Text.Cols[j])  
      
    }}
  
  for(j in 1:K){
    
    text(X.ZoneRight.Centres[j],  1.05 , Text.Labels[j], col = Text.Labels.Cols[j], font = 2, cex = 1)  
    
  }
  
  text((X.ZoneLeft.0 + X.ZoneLeft.1) / 2,  1.05 , ZoneLeft.Label, col = "black", font = 2, 
       cex = ZoneLeft.Label.Cex)  
  
  
  text(0.5,  -.175 , X.Lab, col = "black", font = 2, cex = 1)  
  
  
}



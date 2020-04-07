Lib.Legend  <-  function(Labels, X , Y, Gap, Cols, PointSize, TextSize, Pch, dY, 
                         BorderCol, BorderLwd, LineLty, Header, Type, Eps){
  
  N  <-  length(Labels)
  if(missing(PointSize)){PointSize  <-  rep(1.25,N) }
  if(missing(TextSize)){TextSize  <-  rep(1,N) }
  if(missing(Pch)){Pch  <-  rep(22,N) }
  if(missing(dY)){dY  <-  1}
  if(missing(BorderCol)){BorderCol  <-  rep("black", N) }
  if(missing(BorderLwd)){BorderLwd  <-  rep(1, N) }
  if(missing(Header)){DoHead  <- 0} else{ DoHead  <-  1}
  if(missing(LineLty)){LineLty  <-  rep(1, N)}
  
  if(length(BorderLwd) == 1){BorderLwd  <- rep(BorderLwd, N)}
  if(length(Y) == 1){Y  <-  Y + dY * seq(0,-(N-1), -1)}
  if(length(X) == 1){X  <- rep(X, N)}
  
  if(length(Pch) == 1){Pch  <-  rep(Pch, N) }
  if(length(TextSize) == 1){TextSize  <-  rep(TextSize, N) }
  if(length(PointSize) == 1){PointSize  <-  rep(PointSize, N) }
  
  if(!is.list(Pch)){Pch  <-  as.list(Pch)}
  
  if(missing(Type)){Type  <-  rep("p",N) }
  if(missing(Eps)){Eps  <- Gap / 2} 
  
  
  
  HeadDeltaY  <-  dY * DoHead
  
  Y  <-  Y - HeadDeltaY 
  
  if(DoHead == 1){
    
    text(X[1], Y[1] + HeadDeltaY , labels = Header, font = 2, pos=4, cex=TextSize[1])
    
  }
  
  
  for(i in 1:N){
    
    if(Type[i]  == "l"){
      
      XX   <-  c(X[i] - Eps, X[i] + Eps  )
      YY   <-  c(Y[i], Y[i])
      
    } else { 
      
      XX  <-  X[i]
      YY  <-  Y[i]
    }
    
    points(XX,YY,pch=Pch[[i]],bg=Cols[i], cex=PointSize[i], col = BorderCol[i], lwd = BorderLwd[i],
           type = Type[i], lty = LineLty[i])
    text(X[i] + Gap, Y[i], labels=Labels[i], pos=4, cex=TextSize[i])
    
    }
  
}

######################################################################################################
# N          <-  length(Labels)
# N.Horiz    <-  4
# X.Lim      <-  par()$xaxp[1:2]
# Y.Lim      <-  par()$yaxp[1:2]
# X0         <-  X.Lim[1]
# X1         <-  X.Lim[2]
# Y0         <-  -.1  * (Y.Lim[2] - Y.Lim[1])
# Y1         <-  -.15 * (Y.Lim[2] - Y.Lim[1])
# PointSize  <-  rep(1.25,N)
# TextSize   <-  rep(1,N)
# LineLty    <-  rep(1, N)
# Type       <-  rep("p",N)
# BorderCol  <-  rep("black", N)
# BorderLwd  <-  rep(1, N)
# LineLty    <-  rep(1, N)
# Eps        <- Gap / 2
# Gap        <-  .01 * (X.Lim[2] - X.Lim[1])


Lib.Legend.Horiz  <-  function(Labels, X0, X1 , Y0, Y1, Gap, Cols, PointSize, TextSize, Pch, dY, 
                               BorderCol, BorderLwd, LineLty, Type, Eps, X.Lim,
                               Y.Lim, N.Horiz){
  
  
  if(missing(N.Horiz)){N.Horiz  <-  4}
  
  if(missing(X.Lim)){X.Lim  <-  par()$xaxp[1:2]}
  if(missing(Y.Lim)){Y.Lim  <-  par()$yaxp[1:2]}
  
  
  if(missing(X0)){X0  <-  X.Lim[1]}
  if(missing(X0)){X1  <-  X.Lim[2]}
  if(missing(Y0)){Y0  <-  -.15  * (Y.Lim[2] - Y.Lim[1])}
  if(missing(Y1)){Y1  <-  -.35 * (Y.Lim[2] - Y.Lim[1])}
  
  N  <-  length(Labels)
  if(missing(PointSize)){PointSize  <-  rep(1.25,N)}
  if(missing(TextSize)){TextSize  <-  rep(1,N) }
  if(missing(Pch)){Pch  <-  rep(22,N) }
  if(missing(dY)){dY  <-  1}
  if(missing(BorderCol)){BorderCol  <-  rep("black", N) }
  if(missing(BorderLwd)){BorderLwd  <-  rep(1, N) }
  if(missing(LineLty)){LineLty  <-  rep(1, N)}
  
  if(length(BorderLwd) == 1){BorderLwd  <- rep(BorderLwd, N)}
  if(length(Y) == 1){Y  <-  Y + dY * seq(0,-(N-1), -1)}
  if(length(X) == 1){X  <- rep(X, N)}
  
  if(length(Pch) == 1){Pch  <-  rep(Pch, N) }
  if(length(TextSize) == 1){TextSize  <-  rep(TextSize, N) }
  if(length(PointSize) == 1){PointSize  <-  rep(PointSize, N) }
  
  if(!is.list(Pch)){Pch  <-  as.list(Pch)}
  
  if(missing(Type)){Type  <-  rep("p",N) }
  
  if(missing(Gap)){Gap <-  .01 * (X.Lim[2] - X.Lim[1])} 
  if(missing(Eps)){Eps  <- Gap / 2} 
  
  
  # Need to calculate the horizontal grid
  
  N.Vert   <-  ceiling(N / N.Horiz)  
  
  Grid   <-  Lib.Grid.Coords(N.Horiz, N.Vert , X0 = X0, X1 = X1, Y0 = Y0, Y1 = Y1)
  
  
  for(i in 1:N){
    
    i.x   <-  1 + (i - 1)  %% N.Horiz
    i.y   <-  ceiling(i / N.Horiz )
    
    X     <-  Grid$X[i.x]  
    Y     <-  rev(Grid$Y)[i.y]  
    
    if(Type[i]  == "l"){
      
      
      XX   <-  c(X - Eps, X + Eps  )
      YY   <-  c(Y, Y)
      
    } else { 
      
      XX  <-  X
      YY  <-  Y
    }
    
    points(XX,YY,pch=Pch[[i]],bg=Cols[i], cex=PointSize[i], col = BorderCol[i], lwd = BorderLwd[i],
           type = Type[i], lty = LineLty[i])
    
    text(X + Gap, Y, labels=Labels[i], pos=4, cex=TextSize[i])
    
  }
  
}







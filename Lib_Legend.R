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








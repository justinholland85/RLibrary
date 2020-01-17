
Lib.BarPlot.Stacked   <-  function(Data, Fill, Border, X.Gap = .1 , Plot.New, Border.dY, Plot.Y, Plot.X,
                                   Xlab, Ylab, Yaxt, Plot.Y.Second, Return, Main, Xlab.Cex,Border.Lwd,
                                   Xtext, Ytext, Xtext.Pos){
  
  par(xpd=NA)
  par(mar=c(3.1,4.1,4.1,3.1))
  
  N.X          <-  dim(Data)[2]
  N.Y          <-  dim(Data)[1]
  
  if(missing(Fill)){      Fill    <-  Lib.ColourScheme(ceiling(N.Y * 1.25), 5, V = .8, Alpha = .7)[2, 1:N.Y]}
  if(is.null(dim(Fill))){ Fill    <-  array(dim=c(N.Y, N.X), Fill)}
   
  
  if(missing(Border)){      Border    <-  Lib.ColourScheme(ceiling(N.Y * 1.25), 5, V = .8, Alpha = 1)[2,1:N.Y]}
  if(is.null(dim(Border))){ Border    <-  array(dim=c(N.Y, N.X), Border)}
  
  if(missing(Border.dY)){ Border.dY    <-  .002}
  
  if(missing(Plot.New)){Plot.New  <-  1}
  if(missing(Plot.X)){Plot.X  <-  1}
  if(missing(Plot.Y)){Plot.Y  <-  1}
  if(missing(Return)){Return  <-  0}
  if(missing(Main)){Main      <-  ""}
  if(missing(Xlab.Cex)){Xlab.Cex <- 1}
  if(missing(Border.Lwd)){Border.Lwd <- 2}
  if(missing(Xtext)){Xtext      <-  ""}
  if(missing(Ytext)){Ytext      <-  ""}
  if(missing(Xtext.Pos)){Xtext.Pos  <- -.1}

  
  
  
  X0             <-  array(dim(Data))  
  X1             <-  array(dim(Data))  
  
  Y0             <-  array(dim(Data), 0)  
  Y1             <-  array(dim(Data), 0)  
  
  dX             <-  (1 - X.Gap) / N.X
  x.Gap          <-  X.Gap / (N.X - 1)
  
  X0             <-  array(dim=N.X, 0)     
  X1             <-  array(dim=N.X, dX)     
    
  for(i in 2:N.X){
    
    X0[i]        <-   X0[i-1] + dX +  x.Gap
    X1[i]        <-   X0[i]   + dX
    
  }
  
  X0             <-  t(array(dim=dim(t(Data)), rep(X0, N.Y)))
  X1             <-  t(array(dim=dim(t(Data)), rep(X1, N.Y)))
  
  Y.Max          <-  max(colSums(Data)) 
   
  Y.CumSum       <-  array(dim = dim(Data))
  
  
  for(i in 1:N.X){
    
    Y.CumSum[,i] <-  cumsum(Data[,i]) 
    
  }

#----------------------------------------------------------------------------------------------------#
  
  Y.CumSum       <-  Y.CumSum / Y.Max  
  
  
  Y1   <-  Y.CumSum
  Y0   <-  array(dim = dim(Data))
  
  Y0[1,]      <-  0
  Y0[2:N.Y,]  <-  Y1[1:(N.Y-1), ]
  
  
  
#----------------------------------------------------------------------------------------------------#
if(Plot.New == 1){
  
plot(c(0,1), c(0,1), type='n', bty='n',ylab=Ytext, xlab = "", xaxt='n' ,yaxt='n', main = Main) 
  
  
  text(x = 0.5, y = Xtext.Pos, labels = Xtext)
  
}

#----------------------------------------------------------------------------------------------------#
  
if(Plot.X == 1){
    

  
  if(missing(Xlab)){Xlab   <-  colnames(Data)}
  if(is.null(Xlab)){Xlab   <-  seq(1,N.X)}
    
    
  X.Axt.At  <-  array(dim = N.X)
  
  for(i in 1:N.X){
    
    X.Axt.At[i]  <-  mean(c(X0[1,i], X1[1,i]))
     
  }
    
    
  text(X.Axt.At, y = -0.05, labels = Xlab, cex = Xlab.Cex)
    
    
}
  
  
#----------------------------------------------------------------------------------------------------#

if(Plot.Y == 1){
  
  
  Yaxt   <-  Lib.Axis(Y.Max)
  
  if(missing(Ylab)){ if(Y.Max == 1){ Ylab   <-  paste0(100 * Yaxt$Axis, "%")} else {Ylab   <-  Yaxt$Axis}}
  
  Lib.PlotLines.Y(Yaxt$Axis, Labels = Ylab, X.Min = 0, X.Max = 1, Col =  "transparent")
  
  axis(4,at = Yaxt$Axis, labels =Ylab  )
  
  
}
  
#----------------------------------------------------------------------------------------------------#
# Main Plot

  for(i in 1:N.Y){
  for(j in 1:N.X){
    
    X  <- c(X0[i,j], X0[i,j], X1[i,j], X1[i,j])  
    Y  <- c(Y0[i,j] + Border.dY, Y1[i,j] - Border.dY, Y1[i,j] - Border.dY, Y0[i,j] + Border.dY)  
    
    polygon(X, Y, col = Fill[i,j], border = Border[i,j], lwd=Border.Lwd)
    
    
  }}
  

#----------------------------------------------------------------------------------------------------#
  
Output   <-  list("X0" = X0,
                  "X1" = X1,
                  "Y0" = Y0,
                  "Y1" = Y1)
  
if(Return == 1){
return(Output)
}
  
}
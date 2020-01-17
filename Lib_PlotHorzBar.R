
#Y         <-  seq(1, 10)
#names(Y)  <-  LETTERS[Y]



Lib.PlotHorzBar      <-  function(Y, Cols, Names, X0, X1, Y0, Y1, Plot, Marge.X.L, 
                                  Marge.X.R, Marge.Y.T, Gap, Xlim, Border, Main, 
                                  Main.Cex, Text.Size ){
  
  N.Y    <-  length(Y)
  
  if(missing(Cols)){Cols             <-  rep("transparent", N.Y)}
  if(missing(Border)){Border         <-  rep("black", N.Y)}
  if(missing(Names)){Names           <-  names(Y)}
  if(missing(Cols)){Cols             <-  rep("transparent", N.Y)}
  if(missing(X0)){X0                 <-  0}
  if(missing(X1)){X1                 <-  1}
  if(missing(Y0)){Y0                 <-  0}
  if(missing(Y1)){Y1                 <-  1}
  if(missing(Plot)){Plot             <-  1}
  if(missing(Marge.X.L)){Marge.X.L   <- .1}
  if(missing(Marge.Y.T)){Marge.Y.T   <- .1}
  if(missing(Marge.X.R)){Marge.X.R   <- 0}
  if(missing(Gap)){Gap               <- .01}
  if(missing(Xlim)){Xlim             <- c(0,max(Y))}
  if(missing(Main)){Main             <-  ""}
  if(missing(Main.Cex)){Main.Cex     <-  1}
  if(missing(Text.Size)){Text.Size   <-  1}
  
  
  
  Breaks.Y         <-  rev(seq(0, 1, 1/N.Y))
  Delta.Y          <-  (Y1 - Y0) * (1 - Marge.Y.T)
  
  Breaks.Y         <-  Y0 + Breaks.Y * Delta.Y
  
  Delta.X          <-  Xlim[2] - Xlim[1]       
  
  PlotPoint.Min.X  <-  X0 + (X1 - X0) * Marge.X.L
  PlotPoint.Max.X  <-  X1 - (X1 - X0) * Marge.X.R
  
  PlotLength.X     <-  PlotPoint.Max.X - PlotPoint.Min.X
  
  Scale.X          <-  PlotLength.X / Delta.X
  
  
  Plot.X           <-      pmin(pmax( PlotPoint.Min.X + (Y -  Xlim[1]) * Scale.X, 
                                      PlotPoint.Min.X), PlotPoint.Max.X)
  
  
  if(Plot == 1){
    plot(c(0,1), c(0,1), type='n', bty='n',xlab="",ylab="",xaxt='n' ,yaxt='n')
  }

  
  for(i in 1:N.Y){
    
    t.X0  <-  PlotPoint.Min.X
    t.X1  <-  Plot.X[i]
    t.Y0  <-  Breaks.Y[i] - Gap / 2
    t.Y1  <-  Breaks.Y[i + 1] + Gap / 2
    
    Poly.X  <-  c(t.X0, t.X0, t.X1, t.X1)
    Poly.Y  <-  c(t.Y0, t.Y1, t.Y1, t.Y0)
    
    polygon(Poly.X, Poly.Y, col=Cols[i], border=Border[i])
  
    text(PlotPoint.Min.X, mean(Poly.Y), labels=Names[i], pos=2, cex= Text.Size )
    text(X1, mean(Poly.Y), labels=Y[i], pos=2, cex = Text.Size )
      
  }
  
  text(mean(c(X0, X1)), Y1 - Marge.Y.T * (Y1 - Y0) / 2 , labels = Main, cex = Main.Cex, font = 2)
  
}









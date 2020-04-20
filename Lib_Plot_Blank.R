Lib.Plot.Blank  <-  function(X.Lim, Y.Lim, X0, X1, Y0, Y1, X.Lab, Y.Lab, Main){
  
  if(missing(X.Lim)){X.Lim  <-  c(0,1)}
  if(missing(Y.Lim)){Y.Lim  <-  c(0,1)}
  
  if(missing(X0)){  X0      <-  X.Lim[1]}
  if(missing(X1)){  X1      <-  X.Lim[2]}
  if(missing(Y0)){  Y0      <-  Y.Lim[1]}
  if(missing(Y1)){  Y1      <-  Y.Lim[2]}
  if(missing(X.Lab)){X.Lab  <-  ""}
  if(missing(Y.Lab)){Y.Lab  <-  ""}
  if(missing(Main)){Main    <-  ""}
  
  plot(c(X0,X1), c(Y0,Y1), type='n', bty='n', xaxt ='n', yaxt='n', 
       xlab = X.Lab, ylab = Y.Lab, main = Main)
  
}
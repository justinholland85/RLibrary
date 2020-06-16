Data    <-  array(dim = c(3,10), runif(30,0,10))
colnames(Data)  <-  paste0("X_", seq(1, 10))
rownames(Data)  <-  paste0("N_", seq(1, 3))



Lib.BarPlot   <-  function(Data, Cols, Main){}


Data     <-  rbind(Data)
N        <-  nrow(Data)
M        <-  ncol(Data)

Gap.A     <-  .2
Gap.B     <-  .04

#----------------------------------------------------------------------------------------------------#

if(missing(Y.Lim)){Y.Lim                   <-  Lib.Axis.Smart(Data)$Lim}
if(missing(Main)){Main                     <- ""}
if(missing(Bord.Col)){Bord.Col             <- Lib.ColourScheme(N,5,.8)[1,]}
if(missing(Fill.Col)){Fill.Col             <- Lib.ColourScheme(N,5,.8)[3,]}
if(missing(Bord.Lwd)){Bord.Lwd             <- 1}
if(missing(Gap.A)){Gap.A                   <- .2}
if(missing(Gap.B)){Gap.B                   <- .04}
if(missing(Plot.Y)){Plot.Y                 <- 1}
if(missing(Plot.X)){Plot.X                 <- 1}
if(missing(Y.Axis)){Y.Axis                 <-  Lib.Axis.Smart(Data)$Axis}
if(missing(X.Labels)){X.Labels             <-  colnames(Data)}
if(missing(Leg.Labels)){Leg.Labels         <-  rownames(Data)}
if(missing(X.Ax.Y)){X.Ax.Y                 <-  -.05}
if(missing(X.Ax.Size)){X.Ax.Size           <- 1}
if(missing(X.Ax.Theta)){X.Ax.Theta         <- 0}
if(missing(Plot.Leg.Below)){Plot.Leg.Below <- 1}


if(is.null(X.Labels)){X.Labels      <-  rep("", M)}
if(is.null(Leg.Labels)){Leg.Labels  <-  rep("", N)}
if(!is.matrix(Bord.Col)){Bord.Col   <-  array(dim = dim(Data), Bord.Col)}
if(!is.matrix(Fill.Col)){Fill.Col   <-  array(dim = dim(Data), Fill.Col)}

#----------------------------------------------------------------------------------------------------#



dX.A   <-  1 / M

dX.B   <-  1 / N

X.A    <-  dX.A * seq(0, M )
X.B    <-  Gap.A / 2 + dX.B * (1 - dX.A) * seq(0, N )
X.C    <-  X.B + Gap.B / 2
X.D    <-  X.B - Gap.B / 2

X.0    <-  array(dim = dim(Data))
X.1    <-  array(dim = dim(Data))

 
for(j in 1:M){
  
  A  <-  X.A[j]  
  
for(i in 1:N){
  
  C   <- X.C[i]  
  D   <- X.D[i+1]  
  
  X.0[i,j]  <-  A + C * dX.A 
  X.1[i,j]  <-  A + D * dX.A 

}}

#----------------------------------------------------------------------------------------------------#


X.Lim   <-  c(0, 1)

Lib.Plot.Blank(X.Lim, Y.Lim, Main = Main)

#----------------------------------------------------------------------------------------------------#

if(Plot.Y == 1){  Lib.PlotLines.Y(Y.Axis, X.Lim)}

#----------------------------------------------------------------------------------------------------#



for(j in 1:M){
for(i in 1:N){
  
 Poly.X   <-  c(X.0[i,j], X.0[i,j], X.1[i,j], X.1[i,j])
 Poly.Y   <-  c(0, Data[i,j], Data[i,j], 0)  
  
 polygon(Poly.X, Poly.Y, border = Bord.Col[i,j], col = Fill.Col[i,j], lwd = Bord.Lwd, )
  
  
}}

#----------------------------------------------------------------------------------------------------#

par(xpd = NA)

if(Plot.X == 1){ 
  
  for(i in 1:M){
    
    Text.X   <-  (X.A[i] + X.A[i+1]) / 2 
    Text.Y   <-  X.Ax.Y * (Y.Lim[2] - Y.Lim[1])
    
    text(Text.X, Text.Y, labels = X.Labels[i], cex = X.Ax.Size, srt = X.Ax.Theta)
    
  }  
  }

#----------------------------------------------------------------------------------------------------#

if(Plot.Leg.Below){
  
  
  
  
}




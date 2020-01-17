Lib.HeatMap  <-  function(X0, X1, Y0, Y1, ColourRef, Colours, Xlim, Ylim){

N.Cols  <-  length(unique(ColourRef))
N  <-  length(X0)

if(missing(Xlim)){ Xlim  <-  c(min(X0,X1),max(X0,X1)) }
if(missing(Ylim)){ Ylim  <-  c(min(Y0,Y1),max(Y0,Y1)) }
if(missing(Colours)){ Colours  <-  c(Lib.ColourScheme(round(1.25 * N.Cols),1,Plot=0))}


plot(Xlim,Ylim, type='n')

for(i in 1:N){

X  <-  c(X0[i], X0[i], X1[i], X1[i])
Y  <-  c(Y0[i], Y1[i], Y1[i], Y0[i])

polygon(X,Y, col=Colours[ColourRef[i]], border=NA)
}

}

#########################################################################################################

Lib.HeatMap.DirCol  <-  function(X0, X1, Y0, Y1, Colours, Xlim, Ylim, Xlab, Ylab, Main, NewPlot){
  
  # Same as Lib.HeatMap with direct colour reference
  
  # N.Cols  <-  length(unique(ColourRef))
  N  <-  length(X0)
  
  if(missing(Xlim)){ Xlim  <-  c(min(X0,X1),max(X0,X1)) }
  if(missing(Ylim)){ Ylim  <-  c(min(Y0,Y1),max(Y0,Y1)) }
  if(missing(Xlab)){ Xlab  <- "" }
  if(missing(Ylab)){ Ylab  <- "" }
  if(missing(Main)){ Main  <- "" }
  if(missing(NewPlot)){ NewPlot  <- 1 }
 # if(missing(Colours)){ Colours  <-  c(Lib.ColourScheme(round(1.25 * N.Cols),1,Plot=0))}
  
  if(NewPlot == 1){
  plot(Xlim,Ylim, type='n', xaxt='n', yaxt='n', main = Main, xlab = Xlab, ylab= Ylab)
  }
  
  for(i in 1:N){
    
    X  <-  c(X0[i], X0[i], X1[i], X1[i])
    Y  <-  c(Y0[i], Y1[i], Y1[i], Y0[i])
    
    polygon(X,Y, col=Colours[i], border=NA)
  }
  
}







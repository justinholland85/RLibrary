
####################################################################################################

F.TreeMap.0  	<-  	function(A, R, X0, X1, Y0, Y1){
  
  if(missing(X0)){ X0  <-  0}
  if(missing(X1)){ X1  <-  1}
  if(missing(Y0)){ Y0  <-  0}
  if(missing(Y1)){ Y1  <-  1}
  if(missing(R)){ R  <-  1 - 2 / (1+5^.5)}
  
  N.A  <-  length(A)
  ID <-  seq(1,N.A)
  
  ID.0 <- ID
  A.0  <- A
  
  ID.Coords  <-  array(dim=c(4,N.A))
  rownames(ID.Coords)  <-  c("X0","X1","Y0","Y1")
  
  Points  <-  array(dim=c(2,2),c(X0,Y0,X1,Y1))
  
  repeat{
    
    Dims <-  Points[,2] -  Points[,1] 
    A.Sum  <-  sum(A)
    A.CumPct  <-  cumsum(A) / A.Sum
    A.Full  <-  prod(Dims)
    
    Dir  <-  match(max(Dims), Dims)
    Fixed   <-  c(1,2)[-Dir]
    
    Dir.Coords  <-  Points[Dir,]
    Fixed.Coords  <-  Points[Fixed,]
    
    
    Move.Implied  <-  Dir.Coords[1] + (Dir.Coords[2] - Dir.Coords[1]) * A.CumPct
    
    Length.Implied  <-  Move.Implied  -  Dir.Coords[1]
    Ratio.Implied  <- Length.Implied / (Fixed.Coords[2] - Fixed.Coords[1]) 
    Choose  <-  which(Ratio.Implied > R)[1]
    if(is.na(Choose)){Choose  <-  1}
    
    Fill.Dir  <- c(Dir.Coords[1] , Dir.Coords[1] + (Dir.Coords[2] - Dir.Coords[1]) * A.CumPct[Choose]) 
    
    ID.Fill  <-  ID[1:Choose]
    ID.Map  <-  match(ID.Fill, ID.0)
    
    Share.Rels  <-  cumsum(A[1:Choose] / sum(A[1:Choose]))
    Share.Fixed.Ends  <-  Fixed.Coords[1] + (Fixed.Coords[2] - Fixed.Coords[1]) * Share.Rels
    
    
    ID.Coords[2 * (Fixed - 1) + 1, ID.Map]  <-  c(Fixed.Coords[1],Share.Fixed.Ends)[1:Choose]  
    ID.Coords[2 * (Fixed - 1) + 2, ID.Map]  <-  Share.Fixed.Ends  
    
    ID.Coords[2 * (Dir - 1) + 1, ID.Map]  <-  Fill.Dir[1]
    ID.Coords[2 * (Dir - 1) + 2, ID.Map]  <-  Fill.Dir[2]  
    
    Points[Dir,1]  <-  Fill.Dir[2]
    
    A  <-  A[-seq(1,Choose)]
    ID  <-  ID[-seq(1,Choose)]
    
    if(length(A)==0){break}
  }
  
  return(ID.Coords)
}

####################################################################################################

F.TreeMap.1	<-	function(Table, R, X0, X1, Y0, Y1){
  
  if(missing(X0)){ X0  <-  0}
  if(missing(X1)){ X1  <-  1}
  if(missing(Y0)){ Y0  <-  0}
  if(missing(Y1)){ Y1  <-  1}
  if(missing(R)){ R  <-  1 - 2 / (1+5^.5)}
  
  N	<-	dim(Table)[1]
  M	<-	dim(Table)[2]
  
  RowSums	<-	rowSums(Table)  	
  
  Level.0 	<-	F.TreeMap.0(RowSums, R, X0, X1, Y0, Y1)
  
  Inputs.L1	<-	list()
  for(i in 1:N){
    Inputs.L1[[i]]	<-	Table[i,]
  }
  
  Level.1	<-	list()
  for(i in 1:N){
    
    X0  		<-  	Level.0[1,i] 
    X1  		<-  	Level.0[2,i]
    Y0  		<-  	Level.0[3,i]
    Y1  		<-  	Level.0[4,i]
    
    Level.1[[i]]<-	F.TreeMap.0(Inputs.L1[[i]], R, X0, X1, Y0, Y1)
  }
  
  Output		<-	list(Level.0, Level.1, Inputs.L1)
  names(Output)	<-	c("Level.0", "Level.1", "Inputs.L1")
  return(Output)
}

####################################################################################################

F.TreePlot		<-	function(Level.0, Level.1, Cols, BG, Lwd.0, Lwd.1, Main, PlotNew){
  
  N			<-	ncol(Level.0)
  M			<-	ncol(Level.1[[1]])
  
  if(missing(BG)){	BG	<-	"gray21" }
  if(missing(Cols)){ Cols  <-  Lib.ColourScheme(N, M, Plot = 0) }
  if(missing(Lwd.0)){  Lwd.0  <-  6 }
  if(missing(Lwd.1)){  Lwd.1  <-  2 }
  if(missing(Main)){  Main <-  ""}
  if(missing(PlotNew)){  PlotNew <-  1}
  
  if(PlotNew==1){
  par(bg=BG)
  plot(c(0,1),c(0,1), type='n', xaxt='n', yaxt='n',xlab="", ylab="",main=Main)
  }
  
  for(i in 1:N){
    X0  <-  Level.0[1, i]
    X1  <-  Level.0[2, i]
    Y0  <-  Level.0[3, i]
    Y1  <-  Level.0[4, i]
    
    polygon(c(X0,X0,X1,X1),c(Y0,Y1,Y1,Y0),col="white", lwd= Lwd.0, border=BG)
    
    
    for(j in 1:M){
      
      X0  <-  Level.1[[i]][1,j]
      X1  <-  Level.1[[i]][2,j]
      Y0  <-  Level.1[[i]][3,j]
      Y1  <-  Level.1[[i]][4,j]
      
      polygon(c(X0,X0,X1,X1), c(Y0,Y1,Y1,Y0), col=Cols[j,i], lwd= Lwd.1 , border=BG)
      
    }}}


####################################################################################################

F.TreeText.Detailed  <-	function(Level.0, Level.1, Names.0, Names.1, Col.0, Col.1, Col.2, Cex.0, 
                                 Labels){
  
  N			<-	ncol(Level.0)
  M			<-	ncol(Level.1[[1]])
  
  if(missing(Names.0)){Names.0  <-  seq(1,N)}
  if(missing(Names.1)){Names.1  <-  seq(1,M)}
  if(missing(Cex.0)){Cex.0  <-  rep(1.5,N)}
  if(missing(Labels)){Labels  <- array(dim=c(N,M))
    
  for(i in 1:N){
    for(j in 1:M){
      Labels[i,j]  <-  paste(c(Names.0[i], " & ", Names.1[j], " = ", Share,"%"), collapse="")  
    }}
    
  }
  
  
  
  Alpha	<-	.8
  BG	<-	par()$bg
  BG.0  <- col2rgb(BG) / 255
  BG.1	<-	rgb(BG.0[1], BG.0[2], BG.0[3], Alpha)
  
  if(missing(Col.0)){ Col.0  <-  BG.1}
  if(missing(Col.1)){ Col.1  <-  BG.1}
  if(missing(Col.2)){ Col.2  <-  "white"}
  
  Alpha	<-	.8
  BG.0  <- col2rgb(BG) / 255
  BG.1	<-	rgb(BG.0[1], BG.0[2], BG.0[3], Alpha)
  
  
  
  # text((X0 + X1) / 2, (Y0 + Y1) / 2, Text.0[i], col=BG.1, cex=3)
  
  for(i in 1:N){
    
    X0  <-  Level.0[1, i]
    X1  <-  Level.0[2, i]
    Y0  <-  Level.0[3, i]
    Y1  <-  Level.0[4, i]
    
   # text(X0, Y0 + .8 * (Y1-Y0), Names.0[i], col=BG.1, cex=Cex.0[i], pos=4)
    
    for(j in 1:M){
      
      X0  <-  Level.1[[i]][1,j]
      X1  <-  Level.1[[i]][2,j]
      Y0  <-  Level.1[[i]][3,j]
      Y1  <-  Level.1[[i]][4,j]
    
    Share  <-  round(100 * (X1 - X0) * (Y1 - Y0),1)   
    Text  <-  paste(c(Names.0[i], " & ", Names.1[j], " = ", Share,"%"), collapse="")  
      
      # text(X0, Y0 + .2 * (Y1-Y0),  Names.1[j], col=Col.2, cex=1, pos=4)
    text(X0, Y0 + .2 * (Y1-Y0),  Text, col=BG.1, cex=Cex.0, pos=4)
    }}
  
  
}



####################################################################################################

F.TreeText  <-	function(Level.0, Level.1, Names.0, Names.1, Col.0, Col.1, Col.2, Cex.0){
  
  N			<-	ncol(Level.0)
  M			<-	ncol(Level.1[[1]])
  
  if(missing(Names.0)){Names.0  <-  seq(1,N)}
  if(missing(Names.1)){Names.1  <-  seq(1,M)}
  if(missing(Cex.0)){Cex.0  <-  rep(1.5,N)}
  
  Alpha	<-	.8
  BG	<-	par()$bg
  BG.0  <- col2rgb(BG) / 255
  BG.1	<-	rgb(BG.0[1], BG.0[2], BG.0[3], Alpha)
  
  if(missing(Col.0)){ Col.0  <-  BG.1}
  if(missing(Col.1)){ Col.1  <-  BG.1}
  if(missing(Col.2)){ Col.2  <-  "white"}
  
  Alpha	<-	.8
  BG.0  <- col2rgb(BG) / 255
  BG.1	<-	rgb(BG.0[1], BG.0[2], BG.0[3], Alpha)
  
  for(i in 1:N){
    
    X0  <-  Level.0[1, i]
    X1  <-  Level.0[2, i]
    Y0  <-  Level.0[3, i]
    Y1  <-  Level.0[4, i]
    
    text(X0, Y0 + .8 * (Y1-Y0), Names.0[i], col=Col.1, cex=Cex.0[i], pos=4)
    
    for(j in 1:M){
      
      X0  <-  Level.1[[i]][1,j]
      X1  <-  Level.1[[i]][2,j]
      Y0  <-  Level.1[[i]][3,j]
      Y1  <-  Level.1[[i]][4,j]
      
      Share  <-  round(100 * (X1 - X0) * (Y1 - Y0),1)   
      Text  <-  paste(c(Names.0[i], " & ", Names.1[j], " = ", Share,"%"), collapse="")  
      
      text(X0, Y0 + .2 * (Y1-Y0),  Names.1[j], col=Col.2, cex=1, pos=4)
      #text(X0, Y0 + .2 * (Y1-Y0),  Text, col=BG.1, cex=1, pos=4)
    }}
  
  
}





















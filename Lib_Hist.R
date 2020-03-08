Lib.Hist.Centres  	<-  	function(X,N,Min,Max){

  if(missing(Min)){Min <-  min(X, na.rm=TRUE)}
  if(missing(Max)){Max <-  max(X, na.rm=TRUE)}
  

Delta  	<-  	(Max - Min) / (N-1)

Centres  	<-  	seq(Min,Max,Delta)
Lower  	<-  	Centres - Delta / 2
Upper  	<-  	Centres + Delta / 2 

Counts 	<-  	array(dim=N)
for(i in 1:N){
Counts[i]  	<-  	sum( Lower[i] <= X & X < Upper[i], na.rm=TRUE )
}

Freq  	<-  	Counts / sum(Counts)

Output  		<-  	list(Counts, Freq, Centres, Lower, Upper)
names(Output)  	<-	c("Counts", "Freq", "Centres", "Lower", "Upper")
return(Output)
}


##############################################################################

Lib.Hist.Breaks  	<-  	function(X, Breaks){
  
  N  <-  length(Breaks)

  Lower  	<-  	Breaks[1:(N-1)]
  Upper  	<-  	Breaks[2:N]
  Centres  	<-  (	Upper + Lower ) / 2
  
  Counts 	<-  	array(dim=(N-1))
  for(i in 1:(N-1)){
    Counts[i]  	<-  	sum(Lower[i] <= X & X < Upper[i], na.rm = TRUE)
  }
  
  Freq  	<-  	Counts / sum(Counts)
  
  Ranges  <-  Lib.RangeNames(Breaks)
  
  Output  		<-  	list(Counts, Freq, Centres, Lower, Upper, Ranges)
  names(Output)  	<-	c("Counts", "Freq", "Centres", "Lower", "Upper", "Ranges")
  return(Output)
}


##############################################################################
Lib.ColsToList	<-	function(X){
 
  List   <-  list()
  
  for(i in 1:ncol(X)){
    
    List[[i]]  <-  X[,i]
  }
  
  return(List)
}
  
##############################################################################






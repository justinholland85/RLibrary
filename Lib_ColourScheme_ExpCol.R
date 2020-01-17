
Lib.ColourScheme.ExpCol	<-	function(N,M,K,Plot,Cex,Alpha,BG,ExpCol){

if(missing(Cex)){ Cex	<-	2.5/log(N,3)}
if(missing(Plot)){ Plot  <-  1}
if(Plot != 0){Plot <- 1}
if(missing(Alpha)){Alpha  <-  1}
if(missing(BG)){BG  <-  "black"}
  

Theta0	<-	2 * pi * seq(0, 1, 1/N)
Theta		<-	2 * pi * seq(0, 1, 1/M)
Radius	<-	seq(0 , 1, 1/K)

Table		<-	array(dim=c(M,N))
Table.Exp.Col  <-  array(dim=c(K,M))
Par.bg 	<-	par()$bg
par(bg=BG)



if(Plot == 1){
plot(c(-1.05, 1.05), c(-1.05, 1.05) , type='n' ,xaxt='n', yaxt='n')
}

### Column Expansion


i <-  ExpCol
for(j in 1:M){
for(k in 1:K){

R0		<-	Radius[k]
R1		<-	Radius[k+1]

X		<-	c(R0 * cos(Theta[j]), R1 * cos(Theta[j]), R1 * cos(Theta[j+1]), R0 * cos(Theta[j+1]))
Y		<-	c(R0 * sin(Theta[j]), R1 * sin(Theta[j]), R1 * sin(Theta[j+1]), R0 * sin(Theta[j+1]))

Col		<-	hsv(Theta0[i]/(2 * pi), Theta[j]/(2 * pi), 1 - R1, Alpha)
Table.Exp.Col[K + 1 - k,j]	<-	Col
if(Plot == 1){
polygon(X, Y, col=Col)
}
}}


for(j in 1:M){

X		<-	1.05 * cos((Theta[j] + Theta[j+1])/2)	
Y		<-	1.05 * sin((Theta[j] + Theta[j+1])/2)
Col		<-	hsv(Theta0[i]/(2 * pi), Theta0[j]/(2 * pi), 0)	
if(Plot == 1){
text(X,Y,labels = j, cex=Cex, col=Col, srt=(180/pi*(Theta[j] + Theta[j+1])/2-90)) 
}
}

par(bg=Par.bg )
return(Table.Exp.Col)
}


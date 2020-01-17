
Lib.ColourScheme	<-	function(N,M,V,Plot,Cex,Alpha,BG){

if(missing(V)){	V	<-	1}
if(missing(Cex)){ Cex	<-	2.5/log(N,3)}
if(missing(Plot)){ Plot  <-  1}
if(Plot != 0){Plot <- 1}
if(missing(Alpha)){Alpha  <-  1}
if(missing(BG)){BG  <-  "black"}
  

Theta		<-	2 * pi * seq(0, 1, 1/N)
Radius	<-	seq(0 , 1 , 1/M)

Table		<-	array(dim=c(M,N))
Par.bg 	<-	par()$bg
par(bg=BG)

if(Plot == 1){
plot(c(-1.05, 1.05), c(-1.05, 1.05) , type='n' ,xaxt='n', yaxt='n')
}

for(j in 1:M){
for(i in 1:N){

R0		<-	Radius[j]
R1		<-	Radius[j+1]

X		<-	c(R0 * cos(Theta[i]), R1 * cos(Theta[i]), R1 * cos(Theta[i+1]), R0 * cos(Theta[i+1]))
Y		<-	c(R0 * sin(Theta[i]), R1 * sin(Theta[i]), R1 * sin(Theta[i+1]), R0 * sin(Theta[i+1]))

Col		<-	hsv(Theta[i]/(2 * pi), R1, V, Alpha)
Table[M + 1 - j,i]	<-	Col
if(Plot == 1){
polygon(X, Y, col=Col)
}
}}


for(i in 1:N){

X		<-	1.05 * cos((Theta[i] + Theta[i+1])/2)	
Y		<-	1.05 * sin((Theta[i] + Theta[i+1])/2)
Col		<-	hsv(Theta[i]/(2 * pi), 1, V)	
if(Plot == 1){
text(X,Y,labels = i, cex=Cex, col=Col, srt=(180/pi*(Theta[i] + Theta[i+1])/2-90)) 
}
}

par(bg=Par.bg )
return(Table)
}

####################################################################################################




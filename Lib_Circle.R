Lib.Circle  <-  function(X0,Y0,R,N){

if(missing(X0)){X0  <-  0}
if(missing(Y0)){Y0  <-  0}
if(missing(R)){R  <-  1}   
if(missing(N)){N  <-  100} 

Seq  <-  seq(0,N-1,1)

Theta  <-  2 * pi * Seq / N

X  <-  X0 + R * cos(Theta)
Y  <-	 Y0 + R * sin(Theta)

Output  <-  list(X,Y)
names(Output)  <-  c("X","Y")
return(Output)
}



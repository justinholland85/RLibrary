X  <-  c(0,0.5,1)
Y  <-  c(0,1,0)
N  <-  1000
P  <-  c(0.5,0.25,0.25)


Lib.Sierpinski  <-  function(N,X,Y,P,F){

if(missing(N)){N  <-  1000}
if(missing(X)){X  <-  c(0,0.5,1)}
if(missing(Y)){Y  <-  c(0,1,0)}
M  <-  length(X)
if(missing(P)){P  <-  rep(1/M,M)}
if(missing(F)){F  <-  2}




P  <-  P / sum(P)

XX  <-  array(dim=N)
YY  <-  array(dim=N)


XX[1]  <-  X[1] 
YY[1]  <-  Y[1] 

R  <-  runif(N)

CumSum.P  <-  c(0,cumsum(P))

K  <-  R * 0

for(i in 1:M){
K  <-  K + (R > CumSum.P[i])
}

K[1]  <-  1

for(i in 2:N){

Px  <-  X[K[i]]
Py  <-  Y[K[i]]

XX[i]  <-  (XX[i-1] + Px) / F
YY[i]  <-  (YY[i-1] + Py) / F

}

Output  <-  list(X = XX,
                 Y = YY, 
                 K = K)

return(Output)
}








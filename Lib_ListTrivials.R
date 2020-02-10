Lib.Take.Var  <-  function(X,Var){
  
  Col  <-  match(Var,colnames(X))
  return(X[,Col])
}

####

Lib.Take.Which  <-  function(A,B){
  
  return(A[B])
}

####

Lib.Take.UpTo  <-  function(A,N){
  
  if(N > 0){ Seq <-  seq(1,N)} else {Seq <- 0}
  
  return(A[Seq])
}



####

Lib.Take.One  <-  function(A,B){
  
  return(A[[B]])
}


####

Lib.Take.Which.Rows  <-  function(A,B){
  
  return(A[B,])
}

####

Lib.Take.Which.Cols  <-  function(A,B){
  
  if(is.null(dim(A)) | prod(dim(A)) == 0) {return(numeric(0))} else {
  
   return(A[,B]) } 
     
}

####

Lib.Take.A.From.B  <-  function(A,B){
  
  return(B[A])
}

####

Lib.Take.Col  <-  function(A,n){
  
  return(A[,n])
}

####

Lib.Take.Row  <-  function(A,n){
  
  return(A[n,])
}

####

Lib.B.in.A  <-  function(A,B){
  return(B %in% A)
}

####

Lib.Match.B.to.A  <-  function(A,B){
  return(match(B, A))
}

####

Lib.Kill.NULL  <-  function(X,R){
  
  if(is.null(X)){ X  <- R}
  return(X)
}

####


Lib.Kill.Empty  <-  function(X,R){
  
  if(length(X) == 0){ X  <- R }
  return(X)
}

####

Lib.BinaryOp <-  function(..., Op){
  
  return(Reduce(Op, list(...)))
  
}

####

Lib.Replace  <-  function(A,B,C){
  
  A[B]  <-  C
  
  return(A)
  
}

####


Lib.Quadrant  <-  function(A,B){
  
  X  <-  1 * (A == 1 & B == 0) +
    2 * (A == 0 & B == 1) +
    3 * (A == 1 & B == 1)
  
  return(X)
  
}

####

Lib.SumNA  <-  function(X){
  
  return(sum(is.na(X)))
         
}

####

Lib.SumNeg  <-  function(X){
  
  return(sum(X < 0, na.rm=TRUE))
  
}



####

Lib.SumZero  <-  function(X){
  
  return(sum(X == 0, na.rm=TRUE))
  
}

####

Lib.SumNegInf  <-  function(X){
  
  return(sum(X == -Inf, na.rm=TRUE))
  
}

####

Lib.SumInf  <-  function(X){
  
  return(sum(X == -Inf, na.rm=TRUE))
  
}

####
Lib.AnotB  <-  function(A,B){
  
  return(A & !B)
  
}

####

Lib.RowsToList  <-  function(X){
     N  <-  nrow(X)
     
     Y  <-  list()
     
     for(i in 1:N){
          
          Y[[i]]  <-  X[i,]
     }
     return(Y)
}

####

Lib.A.Minus.B  <-  function(A,B){
     return(A-B)
}


####

Lib.Names  <-  function(X,Names){
     
     names(X)  <-  Names
     return(X)
}

####

Lib.NumericNames   <-  function(X){
  return(as.numeric(names(X)))
}

####

Lib.NA.To.Zero   <-  function(X){
  
  X[which(is.na(X))]  <-  0
  
  return(X)
  
}


####

Lib.NA.To.Val   <-  function(X,v){
  
  X[which(is.na(X))]  <-  v
  
  return(X)
  
}

####

Lib.NAN.To.Zero   <-  function(X){
  
  X[which(is.nan(X))]  <-  0
  
  return(X)
  
}

####

Lib.Inf.To.Zero   <-  function(X){
  
  X[which(is.infinite(X))]  <-  0
  
  return(X)
  
}


####

Lib.KillWhich   <- function(X,k){
  
  X[k]  <-  NA
  return(X)
}

####
Lib.Shell  <-  function(X,r){

  if(missing(r)){r <- 1}
    
  Y        <-  X
  Y[]      <-  r
  return(Y)
  
}  

####

Lib.Between  <-  function(X, Min, Max, Closure){
  
  if(missing(Closure)){ Closure <- 'b'}
  
  if(Closure == 'b'){
    
        Y  <-  Mim <= X & X <= Max
    
  }
  
  if(Closure == 'l'){
    
        Y  <-  Mim <= X & X < Max
    
  }
  
  if(Closure == 'r'){
    
    Y  <-  Mim < X & X <= Max
    
  }
  
  return(Y)
  
}





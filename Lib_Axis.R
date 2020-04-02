



#####################################################################################################

Lib.AxisUpperBound  <-  function(X){
  
  
  D  <-   floor(log(X,10))
  
  if( X < 2 * 10^D){
    D  <-  D - 1
  }
  
  Y   <-  Lib.Mround(X, 10^D , 1)
  
  return(list("Upper" = Y,
              "D" = D))
}

#####################################################################################################

# Lib.Axis  <-  function(X,m){
#   
#   Temp   <-  Lib.AxisUpperBound(max(X))
#   
#   X.Upper  <-  Temp$Upper
#   D        <-  Temp$D
#   
#   if(is.na(X.Upper)){X.Upper  <-  1}
#   
#   
#   if(missing(m)){ m  <-  X.Upper / (10^D)  }
#   if(is.na(m)){m <- 5}
#   if(m < 4){m <- 4}
#   
#   Axis  <-  seq(0, X.Upper, X.Upper / m)
#   
#   Output  <-  list("Axis" = Axis,
#                    "UpperBound" = X.Upper)
#   
#   return(Output)
#   
# }

#####################################################################################################


Lib.Axis  <-  function(X,m, D, Shift){
  
  # m is the ideal number of marks for the axis, however, the procedure searches for a factor close
  # to m
  
  if(missing(Shift)){Shift <- 0}
  
  Problems    <-  c(11, 13, 17, 19)
  ProblemMap  <-  c(12, 15, 18, 20)   
  
  if(missing(m)){m = 5}
  
  Seq     <-  seq(1,10)
  
  Temp   <-  Lib.AxisUpperBound(max(X))
  
  X.Upper  <-  Temp$Upper
  if(missing(D)){ D        <-  Temp$D}
  
  Q        <-  X.Upper / 10^D    

  if(Q %in% Problems){
    
    Q  <-  ProblemMap[match(Q, Problems)]
    
    X.Upper   <-  Q * 10^D
    
  }
  
    
  Factors  <-  Q / Seq
  
  Integral <-   Factors - floor(Factors) == 0 
  Gap      <-  abs(seq(1, 10) - m) - seq(1,10) / 100
  Score    <-  Gap - Seq / 100 - 10 * Integral
  
  Min.Score  <- min(Score)
  
  Pick       <-  Seq[which(Score == Min.Score[1])] 
  
  Delta       <-   X.Upper / Pick

  Axis  <-  seq(0, X.Upper, Delta)
  
  Axis   <-  Axis + Shift
  
  X.Upper    <-  max(Axis)
  
  Lim        <-  c(min(Axis), max(Axis))
  
  
  Output  <-  list("Axis" = Axis,
                   "UpperBound" = X.Upper,
                   "Delta" = Delta,
                   "Lim" = Lim)
  
  return(Output)
  
}




#####################################################################################################

Lib.Axis.PosNeg  <-  function(X){
  
  Min       <-  min(X, na.rm = TRUE)
  Max       <-  max(X, na.rm = TRUE)
  
  if(Min > 0){ Min  <-  0}
  
  First     <-  Lib.Axis(abs(Min) + abs(Max))
  Delta     <-  First$Delta
  
  MinRound  <-   Lib.Mround(Min, Delta, Dir = 1)
  MaxRound  <-   Lib.Mround(Max, Delta, Dir = 1)
  
  Axis      <-  Lib.Axis(MaxRound - MinRound, Shift = MinRound)

  return(Axis)
  
}

#####################################################################################################

Lib.Axis.Range  <-  function(X){
  
  Min       <-  min(X, na.rm = TRUE)
  Max       <-  max(X, na.rm = TRUE)
  
  First     <-  Lib.Axis(abs(Min) + abs(Max))
  Delta     <-  First$Delta
  
  MinRound  <-   Lib.Mround(Min, Delta, Dir = 1)
  MaxRound  <-   Lib.Mround(Max, Delta, Dir = 1)
  
  Axis      <-  Lib.Axis(MaxRound - MinRound, Shift = MinRound)
  
  return(Axis)
  
}


#####################################################################################################
Lib.Axis.Neg  <-  function(X){
  
  Min       <-  min(X, na.rm = TRUE)
  Max       <-  max(0, na.rm = TRUE)
  
  First     <-  Lib.Axis(abs(Min) + abs(Max))
  Delta     <-  First$Delta

  MinRound  <-   Lib.Mround(Min, Delta, Dir = 1)
  MaxRound  <-   Lib.Mround(Max, Delta, Dir = 1)
  
  Axis      <-  Lib.Axis(MaxRound - MinRound, Shift = MinRound)
  
  return(Axis)
  
}

#####################################################################################################

Lib.Axis.Smart  <-  function(X){
  
  
  Min       <-  min(X)   
  Max       <-  max(X)
  
  Type.Pos    <-  Min >= 0 
  Type.Neg    <-  Max <= 0 
  Type.PosNeg <-  Min < 0 & Max > 0
  
  Type        <-  c("Pos", "Neg", "PosNeg")[which(c(Type.Pos, Type.Neg, Type.PosNeg))]
  
  #----
  
  if(Type == "Pos"){   Axis    <-  Lib.Axis(X) }
  if(Type == "Neg"){   Axis    <-  Lib.Axis.Neg(X) }
  if(Type == "PosNeg"){Axis    <-  Lib.Axis.Range(X) }
  
  return(Axis)
  
}








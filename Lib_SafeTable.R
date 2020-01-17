
# A1. Does table percs using row counts as cell base




Lib.SafeTable.A.1  <-  function(X, MinCell, RowTot, ColTot, Round){
  
  
  if(missing(MinCell)){MinCell  <-  10}
  if(missing(RowTot)){RowTot    <-  1}
  if(missing(ColTot)){ColTot    <-  1}
  if(missing(Round)){Round      <-  1}
  
  
  Safe    <-  !(0 < X & X < MinCell)  
  
  Xprime <- X * Safe    
  
  Nrow   <-  nrow(X)
  Ncol   <-  ncol(X)
  
  Y       <- X
  Yprime  <- Xprime  
  
  
  if(ColTot == 1){
    
    Y <-  rbind(Y, colSums(Y))
    rownames(Y)[Nrow + 1] <-  "Total"
    
    Yprime <-  rbind(Yprime, colSums(Yprime))
    rownames(Yprime)[Nrow + 1] <-  "Total"
    
  }
  
  RowSums.Y       <-  rowSums(Y)
  RowSums.Yprime  <-  rowSums(Yprime)
  
  
  if(RowTot == 1){
    
    Y <-  cbind(Y, rowSums(Y))
    colnames(Y)[Ncol + 1] <-  "Total"
    
    Yprime  <-  cbind(Yprime, rowSums(Yprime))
    colnames(Yprime)[Ncol + 1] <-  "Total"
    
  }
  
  
  W       <-  array(dim=dim(Y), RowSums.Y)
  Wprime  <-  array(dim=dim(Yprime), RowSums.Yprime)
  
  Z       <-  round(100 * Y / W, Round)
  Zprime  <-  round(100 * Yprime / Wprime, Round)
  
  Masked   <-  Yprime == 0 & Y != 0
  
  Yprime[which(Masked)]  <-  "*"
  Zprime[which(Masked)]  <-  "*"
  
  
  Output   <-  list("Y"      = Y,
                    "Yprime" = Yprime,
                    "Z"      = Z,
                    "Zprime" = Zprime
                    )
  
  return(Output)
  
}



######################################################################################################




Lib.SafeTable.A.2  <-  function(X, MinCell, RowTot, ColTot, Round){
  
  
  if(missing(MinCell)){MinCell  <-  10}
  if(missing(RowTot)){RowTot    <-  1}
  if(missing(ColTot)){ColTot    <-  1}
  if(missing(Round)){Round      <-  1}
  
  
  Safe    <-  !(0 < X & X < MinCell)  
  
  Xprime <- X * Safe    
  
  Nrow   <-  nrow(X)
  Ncol   <-  ncol(X)
  
  Y       <- X
  Yprime  <- Xprime  
  
  
  if(RowTot == 1){
    
    Y <-  cbind(Y, rowSums(Y))
    colnames(Y)[Ncol + 1] <-  "Total"
    
    Yprime <-  cbind(Yprime, rowSums(Yprime))
    colnames(Yprime)[Ncol + 1] <-  "Total"
    
  }
  
  ColSums.Y       <-  colSums(Y)
  ColSums.Yprime  <-  colSums(Yprime)
  
  
  if(ColTot == 1){
    
    Y <-  rbind(Y, colSums(Y))
    rownames(Y)[Nrow + 1] <-  "Total"
    
    Yprime <-  rbind(Yprime, colSums(Yprime))
    rownames(Yprime)[Nrow + 1] <-  "Total"
    
  }
  
  
  W       <-  t(array(dim=dim(t(Y)), ColSums.Y))
  Wprime  <-  t(array(dim=dim(t(Yprime)), ColSums.Yprime))
  
  Z       <-  round(100 * Y / W, Round)
  Zprime  <-  round(100 * Yprime / Wprime, Round)
  
  Masked   <-  Yprime == 0 & Y != 0
  
  Yprime[which(Masked)]  <-  "*"
  Zprime[which(Masked)]  <-  "*"
  
  
  Output   <-  list("Y"      = Y,
                    "Yprime" = Yprime,
                    "Z"      = Z,
                    "Zprime" = Zprime
  )
  
  return(Output)
  
}














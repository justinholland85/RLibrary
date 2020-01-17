Lib.MapListToSet  <-  function(L, s, Numeric){
  
  if(missing(Numeric)){Numeric  <-  1}
  
  if(Numeric == 1){
  if(missing(s)){s  <-  sort(unique(as.numeric(unlist(lapply(L,names)))))}   
  } else {
  if(missing(s)){s  <-  sort(unique(unlist(lapply(L,names))))}
  }
  
  s      <-  as.character(s)
  Names  <-  lapply(L, names)
  Match  <-  lapply(Names, Lib.Match.B.to.A, s)
  L      <-  mapply(Lib.Take.Which, L, Match, SIMPLIFY = FALSE)
  
  L      <-  lapply(L, Lib.GiveNames, s)
  
  return(L)
  
}

#####################################################################################################

Lib.MapListToSet.Rows  <-  function(L,s, Numeric){
  
  if(missing(Numeric)){Numeric  <-  1}
  
  if(Numeric == 1){
    if(missing(s)){s  <-  sort(unique(as.numeric(unlist(lapply(L,rownames)))))}   
  } else {
    if(missing(s)){s  <-  sort(unique(unlist(lapply(L,rownames))))}
  }
  
  
  s      <-  as.character(s)
  Names  <-  lapply(L, rownames)
  Match  <-  lapply(Names, Lib.Match.B.to.A, s)
  L      <-  mapply(Lib.Take.Which.Rows, L, Match, SIMPLIFY = FALSE)
  L      <-  lapply(L, Lib.GiveNames.Rows, s)
  
  return(L)
  
}

#####################################################################################################

Lib.MapListToSet.Cols  <-  function(L,s, Numeric){
  
  if(missing(Numeric)){Numeric  <-  1}
  
  if(Numeric == 1){
    if(missing(s)){s  <-  sort(unique(as.numeric(unlist(lapply(L,colnames)))))}   
  } else {
    if(missing(s)){s  <-  sort(unique(unlist(lapply(L,colnames))))}
  }
  
  
  s      <-  as.character(s)
  Names  <-  lapply(L, colnames)
  Match  <-  lapply(Names, Lib.Match.B.to.A, s)
  L      <-  mapply(Lib.Take.Which.Cols, L, Match, SIMPLIFY = FALSE)
  L      <-  lapply(L, Lib.GiveNames.Cols, s)
  
  
  return(L)
  
}

#####################################################################################################


Lib.MapVecToSet    <-  function(X,s){
  
  if(is.numeric(s)){  Names   <-   as.numeric(names(X))} else {
                      Names   <-   names(X)  }

Match   <-  match(s, Names)

Y  <-  X[Match]
names(Y)   <-  s
Y[which(is.na(Y))]  <-  0

return(Y)
}

#####################################################################################################
Lib.MapMatToSet.Cols    <-  function(X,s){
  
    if(is.numeric(s)){  Names   <-   as.numeric(colnames(X))} else {
    Names   <-   colnames(X)  }
  
  RowNames   <-  rownames(X)
  
  Match   <-  match(s, Names)
  
  Y  <-  rbind(X[,Match])
  colnames(Y)   <-  s
  rownames(Y)   <-  RowNames   
  Y[which(is.na(Y))]  <-  0
  
  return(Y)
}



#####################################################################################################
Lib.MapMatToSet.Rows    <-  function(X,s){
  
  if(is.numeric(s)){  Names   <-   as.numeric(rownames(X))} else {
    Names   <-   rownames(X)  }
  
  ColNames   <-  colnames(X)
  
  Match   <-  match(s, Names)
  
  Y  <-  cbind(X[Match,])
  rownames(Y)   <-  s
  colnames(Y)   <-  ColNames 
  Y[which(is.na(Y))]  <-  0
  
  return(Y)
}













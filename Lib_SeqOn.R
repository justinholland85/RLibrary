
Lib.GroupOn   <-  function(..., DF, Names, FullOut){
  
  if(missing(DF)){
      DT         <-  data.table(...)} else {
      DT         <-  data.table(DF) }
  
  if(ncol(DT) == 1){
    DT$Dummy <- 1
  }
  
  if(missing(Names)){Names          <-  names(DT)}
  if(missing(FullOut)){FullOut      <- 0}

  Tuple           <-  Lib.Tuple.Count(DT, Names = Names)
  
  Group            <-  Tuple$Index.Match
  
  
  if(FullOut == 1){
    
    Output              <-  Tuple
    Output$Group        <-  Group
    Output$IndexMatch   <-  Null
 
   } else {
  
    Output   <-  Group
    
  }
  
  return(Output)
  
}


######################################################################################################
  
Lib.SeqOn  <-  function(X){
  
  DT    <-  data.table(X = X)
  DT[, Seq := seq_len(.N), by = X]
  
  Seq    <-  DT$Seq
  
  return(Seq)
  
}
  

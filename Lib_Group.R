Lib.Group.Index <-  function(..., DF, Names){
  # Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  names(DT)    <-  Names.DT
  
  Index        <-  DT[ , .(Count = .N),
                       by = Names.DT]
  

  Index        <-  data.frame(Index)[ ,Names.DT]  
  
  Order        <-  do.call(order, as.list(Index))
  Index        <-  Index[Order,]

  
  return(Index)
  
}


######################################################################################################

Lib.Group.Sum   <-  function(..., DF, Var, Names, FullOut){
  
  if(missing(FullOut)){FullOut  <-  0}
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  names(DT)    <-  Names.DT
  
  Table        <-  DT[ , .(Sum = sum(Var)),
                       by = Names.DT]
  
  
  if(FullOut != 0){
    
    return( data.frame(Table))
    
    } else {
    
    return(Table$Sum)  
      
  } 
  
  
}


######################################################################################################










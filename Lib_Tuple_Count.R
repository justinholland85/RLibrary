Lib.Tuple.Count <-  function(..., DF, Names){
# Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  
  names(DT)    <-  Names.DT
  
  Index        <-  DT[ , .(Count = .N),
                        by = Names.DT]
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT]))
  Index        <-  Index[Order,]
  
  Count        <-  merge(DT, Index, sort=FALSE,
                        by =  Names.DT)
  
  Index.Count  <-  Index$Count
  Index        <-  data.frame(Index)[ ,Names.DT]  
  
  Index.Match  <-  Lib.Tuple.Match(DT, Index)
  
  Output       <-  list("Count"       = Count$Count,
                        "Index"       = Index,
                        "Index.Count" = Index.Count,
                        "Index.Match" = Index.Match) 
  
  return(Output)
  
}

######################################################################################################

Lib.Tuple.Sum <-  function(..., DF, Var, Names){
  # Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  names(DT)    <-  Names.DT
  
  DT.Orig       <-  DT
  
  DT           <-  data.table(DT, Var)
  
  
  Index        <-  DT[ , .(Sum = sum(Var)),
                       by = Names.DT]
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT]))
  Index        <-  Index[Order,]
  
  Sum          <-  merge(DT, Index, sort=FALSE,
                         by =  Names.DT)
  
  Index.Sum    <-  Index$Sum
  Index        <-  data.frame(Index)[ ,Names.DT]  
  
  Index.Match  <-  Lib.Tuple.Match(DT.Orig, Index)
  
  Output       <-  list("Sum"         = Sum$Sum,
                        "Index"       = Index,
                        "Index.Sum"   = Index.Sum,
                        "Index.Match" = Index.Match)
  
  return(Output)
  
}

######################################################################################################

Lib.Tuple.MinMax  <-  function(..., DF, Var, Names){
  # Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  names(DT)    <-  Names.DT
  
  DT.Orig       <-  DT
  
  DT           <-  data.table(DT, Var)
  
  
  Index        <-  DT[ , .(Min = min(Var),
                           Max = max(Var)),
                       by = Names.DT]
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT]))
  Index        <-  Index[Order,]
  
  MinMax       <-  merge(DT, Index, sort=FALSE,
                         by =  Names.DT)
  
  Index.Min    <-  Index$Min
  Index.Max    <-  Index$Max
  
  Index        <-  data.frame(Index)[ ,Names.DT]  
  
  Index.Match  <-  Lib.Tuple.Match(DT.Orig, Index)
  
  MinMax       <-  data.frame(MinMax)
  
  Output       <-  list("MinMax"      = MinMax,
                        "Index"       = Index,
                        "Index.Min"   = Index.Min,
                        "Index.Max"   = Index.Max,
                        "Index.Match" = Index.Match)
  
  return(Output)
  
}




######################################################################################################

Lib.Aggregate <-  function(..., DF, Var, Names){
  # Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
      DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  names(DT)    <-  Names.DT
  
  DT.Orig      <-  DT
  
  DT           <-  data.table(DT, Var)
  
  
  Index        <-  DT[ , .(Sum = sum(Var)),
                       by = Names.DT]
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT]))
  Index        <-  Index[Order,]
  
  Output       <-  list("Sum"         = Index$Sum,
                        "Index"       = Index[,..Names.DT],
                        "Full"        = Index)
  
  return(Output)
  
}









######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tuple.Count
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Lib.Tuple.Count <-  function(..., DF, Names){
# Requires data.table  
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
    DT         <-  data.table(DF) }
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  
  names(DT)    <-  Names.DT
  
  Index        <-  DT[ , .(Count = .N),
                        by = Names.DT]
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT, drop = FALSE]))
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
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tuple.Sum
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
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
  Index        <-  data.frame(Index)[ ,Names.DT, drop = FALSE]  
  
  Index.Match  <-  Lib.Tuple.Match(DT.Orig, Index)
  
  Output       <-  list("Sum"         = Sum$Sum,
                        "Index"       = Index,
                        "Index.Sum"   = Index.Sum,
                        "Index.Match" = Index.Match)
  
  return(Output)
  
}

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tuple.MinMax
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
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
  
  Index        <-  data.frame(Index)[ ,Names.DT, drop = FALSE]  
  
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
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Aggregate
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
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
                        "Index"       = Index[,..Names.DT, drop = FALSE],
                        "Full"        = Index)
  
  return(Output)
  
}


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tuple.Apply
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


Lib.Tuple.Apply <-  function(..., DF, Vars, Names, Func, FuncMap){
  
  if(missing(Func)){Func = length}
  
  if(missing(DF)){
    DT         <-  data.table(...)} else {
      DT         <-  data.table(DF) }
  
  DT.Orig      <-  DT
  
  if(is.null(dim(Vars))){Vars  <-  data.table(Var = Vars); SingleVar  <- 1} else {SingleVar <- 0}
  
  Names.Vars   <- names(Vars)
  
  
  if(missing(Names)){Names.DT     <-  names(DT)} else {Names.DT  <-  Names}
  
  DT           <-  data.table(DT, Vars)
  
  if(missing(FuncMap)){
    
    Index         <-  DT[ , lapply(.SD, Func), by = Names.DT]
    
  } else {
    
    Index        <- DT[, {
      Result <- Map(
        function(x, f) f(x),
        .SD,
        FuncMap
      )
      setNames(Result, names(FuncMap))
    }, by = Names.DT, .SDcols = names(FuncMap)]
    
    
  }
  
  
  Order        <-  do.call(order, as.list(Index[,..Names.DT]))
  Index        <-  Index[Order,]
  
  Result       <-  merge(DT.Orig, Index, sort=FALSE,
                         by =  Names.DT)
  
  
  Index.Result <-  Index[ ,..Names.Vars]
  Index        <-  data.frame(Index)[ ,Names.DT, drop = FALSE]  
  
  Index.Match  <-  Lib.Tuple.Match(DT.Orig, Index)
  
  if(SingleVar == 1){
    
    Result        <- Results[,1]
    Index.Result  <- Index.Result[,1]
    
  }
  
  
  Output       <-  list("Result"       = Result,
                        "Index"        = Index,
                        "Index.Result" = Index.Result,
                        "Index.Match"  = Index.Match)
  
  return(Output)
  
}





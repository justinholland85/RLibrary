Lib.QuantAssign         <-  function(X, N, Include){
  
  if(missing(Include)){Include <-  Lib.Shell(X,1)}
  
  Probs     <-  seq(1/N, 1, 1/N)
  
  
  
  W         <-  which(Include == 1)
  
  Q         <-  quantile(X[W], probs=Probs)
  
  Q[length(Q)]  <-  Inf
  
  # I want to allow for both a forced and unforced quantile assignment
  
  Unforced    <-  Lib.RangeMap(X,   c(-Inf, Q))
  Unforced[which(Include == 0)]  <-  0
  
  Temp.Probs   <-  Probs
  Temp.Probs[length(Probs)]  <-  Inf
  
  W.Order  <-  order(X[W]) / length(W)
  
  W.Forced     <- Lib.RangeMap(W.Order, Temp.Probs)
  
  Forced     <-  Lib.Shell(X,0)
  Forced[W]  <-  W.Forced
  
  Names        <-  Lib.RangeNames(paste0(100 * round(c(0,Probs),3),"%"))
  
  Output       <-  list("Unforced" = Unforced,
                        "Forced"   = Forced,
                        "Q"        = Q,
                        "Names"    = Names)
  
  return(Output)
  
  
}
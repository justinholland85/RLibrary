Lib.Rank  <-  function(X, decreasing = TRUE){
     
  Sort   <-  sort(X, decreasing )
  Rank   <-  match(X, Sort)
  
  return(Rank)

}

#----

Lib.Rank.Norm  <-  function(X, decreasing = TRUE){
  
  N.X       <-  length(X)
  
  Rank      <-  match(seq(1,N.X), order(X))
  
  NormRank  <-   (Rank - 1) / (N.X - 1)
  
  return(NormRank)
  
}


######################################################################################################

Lib.Rank.Order   <-  function(X,  decreasing = TRUE){
  
  
  N            <-  length(X)
  Seq          <-  seq(1,N)
  
  Order        <-  order(X, decreasing = decreasing)
  
  Rank.Order   <-  match(Seq, Order)
  return(Rank.Order)
  
  }
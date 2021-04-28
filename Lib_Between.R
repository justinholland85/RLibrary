# Open interval by default
# Can handle A > B, 
# Open is always <lower inequality, upper inequality>


Lib.Between    <-  function(X, A, B, Open, Range){
  
  # Open interval by default
  # Can handle A > B, 
  # Open is always <lower inequality, upper inequality>
  if(missing(A)){A      <-  Range[1]}
  if(missing(B)){B      <-  Range[2]}
  
  if(missing(Open)){Open      <-  c(1, 1)}
  if(length(Open) == 1){Open  <-  rep(Open, 2)}
  
  Check   <-  sum(A > B)
  
  if(Check > 0){
    
    A.prime <-  pmin(A, B)
    B.prime <-  pmax(A, B)
    
    A  <-  A.prime
    B  <-  B.prime
    
  }
  
  # if(sum(Check) == length(X)){Open <-  rev(Open)}
  
  Op.A    <-  c("<=", "<")[1 + Open[1]]
  Op.B    <-  c("<=", "<")[1 + Open[2]]
  
  Test.A  <-  do.call(Op.A, list(A, X))
  Test.B  <-  do.call(Op.B, list(X, B))
  
  return(Test.A & Test.B)
  
}

######################################################################################################

Lib.NotBetween    <-  function(X, A, B, Open, Range){
 
  # Open interval by default
  # Can handle A > B, 
  # Open is always <lower inequality, upper inequality>
  
  if(missing(Open)){Open      <-  c(1, 1)}
  if(length(Open) == 1){Open  <-  rep(Open, 2)}
  if(missing(A)){A      <-  Range[1]}
  if(missing(B)){B      <-  Range[2]}
  
  Check   <-  sum(A > B)
  
  if(Check > 0){
    
    A.prime <-  pmin(A, B)
    B.prime <-  pmax(A, B)
    
    A  <-  A.prime
    B  <-  B.prime
    
  }
  
 # if(sum(Check) == length(X)){Open <-  rev(Open)}
  
  Op.A    <-  c(">", ">=")[1 + Open[1]]
  Op.B    <-  c(">", ">=")[1 + Open[2]]
  
  Test.A  <-  do.call(Op.A, list(A, X))
  Test.B  <-  do.call(Op.B, list(X, B))
  
  return(Test.A | Test.B)
  
}

######################################################################################################
# Tests 

# X   <-  round(10 * runif(10))
# A   <-  3
# B   <-  7

# Lib.Between(X,A,B)
# Lib.NotBetween(X,A,B)
# table(Lib.Between(X,A,B) + Lib.NotBetween(X,A,B))

# X   <-  round(10 * runif(10))
# A   <-  round(10 * runif(10))
# B   <-  round(10 * runif(10))

# DF   <-  data.frame(X, 
#                    A,
#                    B,
#                    Between    = Lib.Between(X,A,B),
#                    NotBetween = Lib.NotBetween(X,A,B),
#                    Tautology  = Lib.Between(X,A,B) | Lib.NotBetween(X,A,B))

# DF   <-  data.frame(X, 
#                    A,
#                    B,
#                    Between    = Lib.Between(X,A,B, Open = c(0, 1)),
#                    NotBetween = Lib.NotBetween(X,A,B, Open = c(0, 1)),
#                    Tautology  = Lib.Between(X,A,B, Open = c(0, 1)) | Lib.NotBetween(X,A,B, Open = c(0, 1)))



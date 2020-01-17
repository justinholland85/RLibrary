

Lib.BigNumbers   <-  function(Numbers, D){


if(missing(D)){ D <- 2}

Powers  <-  c(0,3,6,9,12)
Strings  <- c("","k","m","b","t")


Order  <-  Lib.Mround(floor(log(abs(Numbers), 10)),3,Dir=-1)
Order[which(Order == -Inf)]  <-  0
Order[which(Order == Inf)]  <-  0


Divisor  <-  10^Order

#BasicNum  <- round(Numbers / Divisor ,  D)
BasicNum  <- signif(Numbers / Divisor ,  D)

Str  <-  Strings[match(Order, Powers)]
Str[which(is.na(Str))]  <-  ""
 
NumberStrings  <-  paste(BasicNum,Str, sep="")

return(NumberStrings)

}

######################################################################################################

Lib.Ordinals  <-  function(X){
  
  X.Mod10   <-  X %% 10
  
  X.Mod100  <-  X %% 100
  
  st        <-  X.Mod10 == 1 & X.Mod100 != 11
  nd        <-  X.Mod10 == 2 & X.Mod100 != 12
  rd        <-  X.Mod10 == 3 & X.Mod100 != 13
  
  th        <-  !st & !nd & !rd
  
  Ref       <-  1 * st + 2 * nd + 3 * rd + 4 * th 
  
  
  Y         <- paste0(X, c("st", "nd", "rd", "th")[Ref])
  
  return(Y)
  
}







Dir.Rlong   <-  "/data/secure/Fast/ALIFE/R_long/LongData"
setwd(Dir.Rlong)

load('sp_status_reported.Rdata')

table(sp_status_reported[, 14])




source("/data/secure/stata/Playground/Justin/Code/R Library/Start.R")

Colours  <-  Lib.ColourScheme(36,10,BG="transparent")[1,]

Lib.ColourScheme.ExpCol(36, 10, K = 10, ExpCol = 1, BG="transparent")

Lib.GreyScale  <-   function(Colours,Weights){
  if(missing(Weights)){Weights  <-  rep(1/3, 3)}
  
  RGB   <-  col2rgb(Colours)
  Grey  <-  colSums(RGB * Weights) 
  Grey  <-  rgb(Grey, Grey, Grey, maxColorValue = 255)
  
  return(Grey)
  
}
  



  
  
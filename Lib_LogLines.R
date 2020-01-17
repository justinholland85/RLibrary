
Lib.LogLines  <-  function(Min, Max){
  
  
  # Extending super min and super max will require extension of character vector
  SuperMin  <-  -12
  SuperMax  <-  12
  
  if(missing(Min)){ Min  <-  SuperMin}
  if(missing(Max)){ Max  <-  SuperMax}
  
  
  
  Lines  <- numeric(0)
  #for(i in (Xlim[1]+1):(Xlim[2]-1)){
  for(i in (SuperMin + 1):(SuperMax - 1)){
    
    Add <-  sign(i) * seq(1:9) * 10 ^ (abs(i))
    if(sign(i) == -1){Add <-  rev(Add)}
    Lines  <- c(Lines, Add)
  }
  
  
  Lines  <-  c(sign(SuperMin) * 10^abs(SuperMin), Lines, sign(SuperMax) * 10^abs(SuperMax))
  Lines  <-  setdiff(Lines, 0)
  
  Lines.AxtVal  <-  Lib.LogScale(Lines, 10)  

 # c(0.9,-0.9) %in% round(seq(SuperMin, SuperMax, .1),1)
  
  # Large Number Abbreviations http://crusaders-of-the-lost-idols.wikia.com/wiki/Large_Number_Abbreviations
  
  
  Labels.Seq  <-  seq(SuperMin, SuperMax)  
  Labels      <-  c("-1t","-100b","-10b","-1b", "-100m","-10m","-1m","-100k","-10k","-1k","-100","-10",0,
                       "10", "100", "1k", "10k", "100k", "1m","10m", "100m", "1b","10b","100b","1t")
  
  # 
  
  
  Axt.Lines.Val    <-  Lines[which(Min <= round(Lines.AxtVal,4) & 
                                   round(Lines.AxtVal,4) <= Max)]
  Axt.Lines.LogVal <-  Lib.LogScale(Axt.Lines.Val, 10)
  Axt.Tick.Val     <-  Labels.Seq[which(Min  <= Labels.Seq & Labels.Seq <= Max)]
  Axt.Tick.Label   <-  Labels[which(Min  <= Labels.Seq & Labels.Seq <= Max)]
  
  Output  <-  list("Axt.Lines.Val" = Axt.Lines.Val,
                   "Axt.Lines.LogVal" = Axt.Lines.LogVal,
                   "Axt.Tick.Val" = Axt.Tick.Val,
                   "Axt.Tick.Label" = Axt.Tick.Label)
  
  return(Output)
  }
  
  
  
    

  
  
  
  
  
  
  
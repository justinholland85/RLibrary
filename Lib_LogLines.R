
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
  Labels      <-  c("-1t","-100","-10b","-1b", "-100m","-10m","-1m","-100k","-10k","-1k","-100","-10",0,
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
  
  
######################################################################################################
# LogLines.True will do a proper logarithmic plot, all postive    
Lib.LogLines.True  <-  function(Min, Max){
  
  SuperMin  <-  -12
  SuperMax  <-  12
  
  
  Lines  <- numeric(0)

  for(i in (SuperMin + 1):(SuperMax - 1)){
    
    Add <-seq(1:9) * (10 ^ i)
    Lines  <- c(Lines, Add)
  }
  
  Labels.Seq  <-  seq(SuperMin, SuperMax)  

  Labels      <-  c("1p" ,"10p", "100p" ,"1n" ,"10n", "100n", "1mu" ,"10mu", "100mu", "1mi", "10mi","100mi",
                    "1","10", "100", "1k", "10k", "100k", "1M","10M", "100M", "1G","10G",
                    "100G","1T")
  
  
  Lines.AxtVal     <-  log(Lines, 10)  
  
  Axt.Lines.Val    <-  Lines[which(Min <= Lines.AxtVal & Lines.AxtVal <= Max)]
  Axt.Lines.LogVal <-  log(Axt.Lines.Val, 10)
  
  Axt.Tick.Val     <-  Labels.Seq[which(Min  <= Labels.Seq & Labels.Seq <= Max)]
  Axt.Tick.Label   <-  Labels[which(Min  <= Labels.Seq & Labels.Seq <= Max)]
  
  Output  <-  list("Axt.Lines.Val" = Axt.Lines.Val,
                   "Axt.Lines.LogVal" = Axt.Lines.LogVal,
                   "Axt.Tick.Val" = Axt.Tick.Val,
                   "Axt.Tick.Label" = Axt.Tick.Label)
  
  return(Output)

}

######################################################################################################  


Lib.LogBins   <-  function(Min, Max, Type, Max.Neg){
  
  # Types: -1 = Neg, 0 = PosNeg, 1 = Pos
  
  if(missing(Type)){Type        <-  1}
  if(missing(Max.Neg)){Max.Neg  <- Max}
  
  BinVals.Pos     <-  10^seq(Min, Max, .1)
  BinVals.Neg     <-  10^seq(Min, Max.Neg, .1)
  
  if(Type == 0){  BinVals   <-  c(rev(-BinVals.Neg), BinVals.Pos) }
  
  if(Type == -1){ BinVals   <-  c(rev(-BinVals.Neg))  }
  
  if(Type == 1){ BinVals   <-  c(BinVals.Pos)
    
  }
  
  return(BinVals)
  
}
######################################################################################################

Lib.LogHist  <-  function(X, Min, Max, Max.Neg){
  
  if(missing(Min)){Min           <-  0}
  if(missing(Max)){Max           <- ceiling(log(max(abs(X)), 10))}
  if(missing(Max.Neg)){Max.Neg   <- Max}
  
  Type.Pos    <-  min(X) >= 0 
  Type.Neg    <-  max(X) <= 0 
  Type.PosNeg <-  min(X) < 0 & max(X) > 0
  
  Type   <-  c(-1,0,1)[which(c(Type.Neg, Type.PosNeg, Type.Pos))]
  
  BinVals     <-  Lib.LogBins(Min, Max, Type, Max.Neg)
  
  Hist        <- Lib.Hist.Breaks(X, BinVals)
  
  Lib.LogLines.True(Min, Max)
  
  
  return(Hist)
  
}

######################################################################################################

Lib.LogPlotWindow.X  <-  function(MaxPos, MaxNeg, Min, Center.Gap, Y.Lim, Col, Lwd, Lty, 
                                  X.Lab, Y.Lab, Main, Srt, Cex, Y.Text.Par, Plot.New,
                                  Line.Lims.1 , Line.Lims.2){
  
if(missing(Col)){ 
Col <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6,
                                    BG= "white",  Plot = 0)[3,3]}
  
  
if(missing(Y.Lim)){Y.Lim <- c(0,1)}
if(missing(Y.Lab)){Y.Lab <- ""}
if(missing(X.Lab)){X.Lab <- ""}
if(missing(Main)){Main <- ""}
if(missing(Plot.New)){Plot.New <- 1}
  
if(missing(Y.Text.Par)){Y.Text.Par <- -.15 * (Y.Lim[2] - Y.Lim[1])}
  
Delta.Y  <-   (Y.Lim[2] - Y.Lim[1])
  
if(missing(Lwd)){Lwd <- 1}
if(missing(Lty)){Lty <- 2}
if(missing(Srt)){Srt <- 90}
if(missing(Cex)){Cex <- .75}
if(missing(Line.Lims.1)){Line.Lims.1  <-  Y.Lim}
if(missing(Line.Lims.2)){Line.Lims.2  <-  c(NA, NA)}
  
if(missing(Min)){Min <- 0}
if(missing(Center.Gap)){Center.Gap     <- .05}

Do.Neg          <-  MaxNeg > Min
Do.Pos          <-  MaxPos > Min
  
if(!Do.Neg){MaxNeg <-  -Inf}
if(!Do.Pos){MaxPos <-  -Inf}

PosSeq          <-  Lib.LogLines.True(Min, MaxPos)
NegSeq          <-  lapply(Lib.LogLines.True(Min, MaxNeg), rev)


MagPoints.Neg   <-  NegSeq$Axt.Tick.Val
MagPoints.Pos   <-  PosSeq$Axt.Tick.Val

MagPoints       <-  c(MagPoints.Neg, 0, MagPoints.Pos)  
N.Mags          <-  length(MagPoints) 
N.Mags.Neg      <-  length(MagPoints.Neg) 
N.Mags.Pos      <-  length(MagPoints.Pos) 

dN              <-  2 + (Do.Neg & Do.Pos)

MagScale        <-  (1 - Center.Gap) / (N.Mags - dN)

MapToScale.Neg  <-  (MagPoints.Neg[1] -  NegSeq$Axt.Lines.LogVal) * MagScale
MapToScale.Pos  <-  MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                    (PosSeq$Axt.Lines.LogVal - MagPoints.Pos[1]) * MagScale


N.LinesNeg      <-  length(MapToScale.Neg)
N.LinesPos      <-  length(MapToScale.Pos)

Axis.Neg.Ticks  <-   (MagPoints.Neg[1] - MagPoints.Neg) * MagScale
Axis.Neg.Labels <-   paste0("-", NegSeq$Axt.Tick.Label )

Axis.Pos.Ticks  <-    MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                      (MagPoints.Pos -  MagPoints.Pos[1]) * MagScale

Axis.Pos.Labels <-   PosSeq$Axt.Tick.Label

if(Plot.New != 0){
Lib.Plot.Blank(Y0 = Y.Lim[1], Y1 = Y.Lim[2], X.Lab = X.Lab, Y.Lab = Y.Lab, Main = Main)
}

par(xpd = NA)

if(Do.Neg){
axis(1, at = Axis.Neg.Ticks, labels = NA, cex.axis = .75)
text(Axis.Neg.Ticks, Y.Lim[1] + Y.Text.Par * Delta.Y, labels = Axis.Neg.Labels, srt = 90 )
}

if(Do.Pos){
axis(1, at = Axis.Pos.Ticks, labels = NA, cex.axis = .75)
text(Axis.Pos.Ticks, Y.Lim[1] + Y.Text.Par * Delta.Y, labels = Axis.Pos.Labels, srt = 90 )
}

par(xpd = FALSE)

#--- plot negative lines
if(Do.Neg){
for(i in 1:N.LinesNeg){
  
  points(c(MapToScale.Neg[i], MapToScale.Neg[i]), c(Line.Lims.1[1], Line.Lims.1[2]),
         type='l', lwd=Lwd, lty = Lty, col = Col)
  
  points(c(MapToScale.Neg[i], MapToScale.Neg[i]), c(Line.Lims.2[1], Line.Lims.2[2]),
         type='l', lwd=Lwd, lty = Lty, col = Col)
  
  
}}

#--- plot N.LinesPos lines
if(Do.Pos){
for(i in 1:N.LinesPos){
  points(c(MapToScale.Pos[i], MapToScale.Pos[i]), c(Line.Lims.1[1], Line.Lims.1[2]),
         type='l', lwd=Lwd, lty = Lty, col = Col)
  
  points(c(MapToScale.Pos[i], MapToScale.Pos[i]), c(Line.Lims.2[1], Line.Lims.2[2]),
         type='l', lwd=Lwd, lty = Lty, col = Col)
  
  
}}


}


######################################################################################################

Lib.LogPlotWindow.Y  <-  function(MaxPos, MaxNeg, Min, Center.Gap, X.Lim, Col, Lwd, Lty, 
                                  X.Lab, Y.Lab, Main, Srt, Cex, X.Text.Par, Plot.New, 
                                  Line.Lims.1, Line.Lims.2){
  
  if(missing(Col)){ 
    Col <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6,
                                    BG= "white",  Plot = 0)[3,3]}
  
  
  if(missing(X.Lim)){X.Lim <- c(0,1)}
  if(missing(Y.Lab)){Y.Lab <- ""}
  if(missing(X.Lab)){X.Lab <- ""}
  if(missing(Main)){Main <- ""}
  if(missing(Plot.New)){Plot.New <- 1}
  
  if(missing(X.Text.Par)){X.Text.Par <- -.1 }
  
  Delta.X  <-   (X.Lim[2] - X.Lim[1])
  
  if(missing(Lwd)){Lwd <- 1}
  if(missing(Lty)){Lty <- 2}
  if(missing(Srt)){Srt <- 90}
  if(missing(Cex)){Cex <- .75}
  
  if(missing(Line.Lims.1)){Line.Lims.1  <-  X.Lim}
  if(missing(Line.Lims.2)){Line.Lims.2  <-  X.Lim}
  
  if(missing(Min)){Min <- 0}
  if(missing(Center.Gap)){Center.Gap     <- .05}
  
  Do.Neg   <-  MaxNeg > Min
  Do.Pos   <-  MaxPos > Min
  
  if(!Do.Neg){MaxNeg <-  -Inf}
  if(!Do.Pos){MaxPos <-  -Inf}
  
  PosSeq   <-  Lib.LogLines.True(Min, MaxPos)
  NegSeq   <-  lapply(Lib.LogLines.True(Min, MaxNeg), rev)
  
  
  MagPoints.Neg  <-  NegSeq$Axt.Tick.Val
  MagPoints.Pos  <-  PosSeq$Axt.Tick.Val
  
  MagPoints  <-  c(MagPoints.Neg, 0, MagPoints.Pos)  
  N.Mags     <-  length(MagPoints) 
  N.Mags.Neg <-  length(MagPoints.Neg) 
  N.Mags.Pos <-  length(MagPoints.Pos) 
  
  dN         <-  2 + (Do.Neg & Do.Pos)
  
  MagScale   <-  (1 - Center.Gap) / (N.Mags - dN)
  
  MapToScale.Neg  <-  (MagPoints.Neg[1] -  NegSeq$Axt.Lines.LogVal) * MagScale
  MapToScale.Pos  <-  MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                       (PosSeq$Axt.Lines.LogVal - MagPoints.Pos[1]) * MagScale
  
  
  N.LinesNeg  <-  length(MapToScale.Neg)
  N.LinesPos  <-  length(MapToScale.Pos)
  
  Axis.Neg.Ticks  <-   (MagPoints.Neg[1] - MagPoints.Neg) * MagScale
  Axis.Neg.Labels <-   paste0("-", NegSeq$Axt.Tick.Label )
  
  Axis.Pos.Ticks  <-    MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                        (MagPoints.Pos -  MagPoints.Pos[1]) * MagScale
  
  Axis.Pos.Labels <-   PosSeq$Axt.Tick.Label
  
  if(Plot.New != 0){
  Lib.Plot.Blank(X0 = X.Lim[1], X1 = X.Lim[2], X.Lab = X.Lab, Y.Lab = Y.Lab, Main = Main)
  }
  
  par(xpd = NA)
  
  if(Do.Neg){
    axis(2, at = Axis.Neg.Ticks, labels = NA, cex.axis = .75)
    text(X.Lim[1] + X.Text.Par * Delta.X, Axis.Neg.Ticks, labels = Axis.Neg.Labels, srt = 0 )
  }
  
  if(Do.Pos){
    axis(2, at = Axis.Pos.Ticks, labels = NA, cex.axis = .75)
    text(X.Lim[1] + X.Text.Par * Delta.X, Axis.Pos.Ticks, labels = Axis.Pos.Labels, srt = 0 )
  }
  
  par(xpd = FALSE)
  
  #--- plot negative lines
  if(Do.Neg){
    for(i in 1:N.LinesNeg){
      
      points(c(Line.Lims.1[1], Line.Lims.1[2]), c(MapToScale.Neg[i], MapToScale.Neg[i]), 
             type='l', lwd=Lwd, lty = Lty, col = Col)
      
      points(c(Line.Lims.2[1], Line.Lims.2[2]), c(MapToScale.Neg[i], MapToScale.Neg[i]), 
             type='l', lwd=Lwd, lty = Lty, col = Col)
      
    }}
  
  #--- plot N.LinesPos lines
  if(Do.Pos){
    for(i in 1:N.LinesPos){
      points(c(Line.Lims.1[1], Line.Lims.1[2]), c(MapToScale.Pos[i], MapToScale.Pos[i]), 
             type='l', lwd=Lwd, lty = Lty, col = Col)
      
      points(c(Line.Lims.2[1], Line.Lims.2[2]), c(MapToScale.Pos[i], MapToScale.Pos[i]), 
             type='l', lwd=Lwd, lty = Lty, col = Col)
      
    }}
  
}



######################################################################################################
Lib.LogPlotWindow.Scale  <-  function(X, MaxPos, MaxNeg, Min, Center.Gap){
  
  # This fucntion goes directly with Lib.LogPlotWindow.X, changes to that fucntion
  # may require changes to this one
  if(missing(Min)){Min <- 0}
  if(missing(Center.Gap)){Center.Gap     <- .05}
  
  Do.Neg          <-  MaxNeg > Min
  Do.Pos          <-  MaxPos > Min
  
  PosSeq          <-  Lib.LogLines.True(Min, MaxPos)
  NegSeq          <-  lapply(Lib.LogLines.True(Min, MaxNeg), rev)
  
  
  MagPoints.Neg   <-  NegSeq$Axt.Tick.Val
  MagPoints.Pos   <-  PosSeq$Axt.Tick.Val
  
  MagPoints       <-  c(MagPoints.Neg, 0, MagPoints.Pos)  
  N.Mags          <-  length(MagPoints) 
  N.Mags.Neg      <-  length(MagPoints.Neg) 
  N.Mags.Pos      <-  length(MagPoints.Pos) 
  

  dN              <-  2 + (Do.Neg & Do.Pos)
  
  MagScale        <-  (1 - Center.Gap) / (N.Mags - dN)
  
  IsNeg           <-  X < 0
  IsPos           <-  X > 0
  IsZero          <-  X == 0
  
  MapToScale.Neg  <-  (MagPoints.Neg[1] - log(abs(X), 10)) * MagScale
  MapToScale.Pos  <-  MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                      (log(abs(X), 10) - MagPoints.Pos[1]) * MagScale
  
  MapToScale.Zero <-   MagScale * pmax((N.Mags.Neg - 1),0) + Do.Neg * Center.Gap / 2
  
  
  MapToScale  <-  rep(NA, length(X))
  MapToScale[which(IsNeg)]   <-  MapToScale.Neg[which(IsNeg)]
  MapToScale[which(IsPos)]   <-  MapToScale.Pos[which(IsPos)]
  MapToScale[which(IsZero)]  <-  MapToScale.Zero
  
  return(MapToScale)
  
}
######################################################################################################

Lib.LogPlotWindow.AltLines  <-  function(MaxPos, MaxNeg, Min, Center.Gap){
  
  if(missing(Min)){Min <- 0}
  if(missing(Center.Gap)){Center.Gap     <- .05}
  
  Do.Neg   <-  MaxNeg > Min
  Do.Pos   <-  MaxPos > Min
  
  if(!Do.Neg){MaxNeg <-  -Inf}
  if(!Do.Pos){MaxPos <-  -Inf}
  
  PosSeq          <-  Lib.LogLines.True(Min, MaxPos)
  NegSeq          <-  lapply(Lib.LogLines.True(Min, MaxNeg), rev)
  
  
  MagPoints.Neg   <-  NegSeq$Axt.Tick.Val
  MagPoints.Pos   <-  PosSeq$Axt.Tick.Val
  
  MagPoints       <-  c(MagPoints.Neg, 0, MagPoints.Pos)  
  N.Mags          <-  length(MagPoints) 
  N.Mags.Neg      <-  length(MagPoints.Neg) 
  N.Mags.Pos      <-  length(MagPoints.Pos) 
  
  dN              <-  2 + (Do.Neg & Do.Pos)
  
  MagScale        <-  (1 - Center.Gap) / (N.Mags - dN)
  
  Axis.Neg.Ticks  <-   (MagPoints.Neg[1] - MagPoints.Neg) * MagScale

  
  Axis.Pos.Ticks  <-    MagScale * pmax((N.Mags.Neg - 1),0) + Center.Gap + 
                        (MagPoints.Pos -  MagPoints.Pos[1]) * MagScale
  
  
  Lim.1    <-   c(Axis.Neg.Ticks[1], Axis.Neg.Ticks[max(length(Axis.Neg.Ticks), 1)])          
  Lim.2    <-   c(Axis.Pos.Ticks[1], Axis.Pos.Ticks[max(length(Axis.Pos.Ticks), 1)])       
  
  Output   <-  list("Lim.1" = Lim.1,
                    "Lim.2" = Lim.2)  
  
  return(Output)
  
  }


######################################################################################################
Lib.LogPlot.GoodMin   <-  function(X, r){
  
  if(missing(r)){r   <-  .001}
  
  Test.Orders   <-  seq(-12, 12)
  LogMap        <-  Lib.RangeMap(log(abs(X), 10), Test.Orders)
  Table         <-  table(LogMap)
  Table         <-  Table/ sum(Table)
  
  Choice        <-  Test.Orders[Lib.NumericNames(Table)[which(Table >= r)]][1]
  
}
######################################################################################################

Lib.Log.Mags   <-  function(X){

if(min(X) < 0){MaxNeg.X  <-   ceiling(log(abs(min(X)), 10)) } else {
  MaxNeg.X  <- X.Scale.Min }
  
if(max(X) > 0){MaxPos.X  <-   ceiling(log(abs(max(X)), 10)) } else {
  MaxPos.X  <- X.Scale.Min }
  
Output   <-  list("Pos" = MaxPos.X,
                  "Neg" = MaxNeg.X)
return(Output)
}



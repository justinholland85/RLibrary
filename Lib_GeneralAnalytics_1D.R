######################################################################################################
# Histogram of X

Lib.GA.1D.Hist.Basic  <-  function(X, N, Col, X.Name, Main, X.Lab, Y.Lab ){

  
if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}  
if(missing(X.Name)){X.Name  <- "X"}  
if(missing(Main)){Main  <-  "Histogram: X ~ Count" }
if(missing(X.Lab)){X.Lab  <-  paste0("Histogram: ", X.Name, " ~ Count") }
if(missing(Y.Lab)){Y.Lab  <-   "Count"} 

par(mar =  c(6.1, 4.1, 4.1, 2.1))
  
if(missing(N))  {N    <-  51}
if(missing(Col)){Col  <-  "blue"}



X.Axis    <-  Lib.Axis.Smart(X)
X.Lim     <-  X.Axis$Lim

Breaks    <-  signif(seq(X.Lim[1], X.Lim[2], (X.Lim[2] - X.Lim[1]) / (N - 1)), 2)
Hist      <-  Lib.Hist.Breaks(X, Breaks)


Y.Axis    <-  Lib.Axis(Hist$Counts) 


Lib.Plot.Blank(X.Lim  = X.Lim, 
               Y.Lim  = Y.Axis$Lim, 
               X.Lab  = X.Lab, 
               Y.Lab  = Y.Lab,
               Main   = Main
                )

Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = X.Lim)
Lib.PlotLines.X(X.Axis$Axis, Col = "transparent")

points(Hist$Centres, Hist$Counts, type = 'h' )
points(Hist$Centres, Hist$Counts, pch = 21, bg = Col )
}

######################################################################################################
# Histogram: Log(X) ~ Count

Lib.GA.1D.Hist.LogX  <-  function(X,LogMin, Col ){
 
  X.Name  <-  "X"
  
   if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}  
  par(mar =  c(6.1, 4.1, 4.1, 2.1))
  
if(missing(LogMin)){LogMin     <-  Lib.LogPlot.GoodMin(X)}
if(missing(Col)){Col  <-  "blue"}

  
Hist       <-  Lib.LogHist(X, Min = LogMin)
Centres    <-  Hist$Centres
Counts     <-  Hist$Counts

Min.X      <-  min(Centres)
Max.X      <-  max(Centres)


if(Min.X < 0){MaxNeg    <-   ceiling(log(abs(min(Centres)), 10)) } else {MaxNeg  <- LogMin }
if(Max.X > 0){MaxPos    <-   ceiling(log(abs(max(Centres)), 10)) } else {MaxPos  <- LogMin }


#MaxNeg     <-  max(ceiling(log(abs(min(Centres, Min)),10)), Min) 
#MaxPos     <-  max(ceiling(log(abs(max(Centres, Min)),10)), Min)   

Y.Axis     <-  Lib.Axis(Counts)
Y.Lim      <-  Y.Axis$Lim
Lims.X      <-  Lib.LogPlotWindow.AltLines(MaxPos, MaxNeg, LogMin)

Scales.Centres  <-  Lib.LogPlotWindow.Scale(Centres , MaxPos, MaxNeg, LogMin)

X.Lab                   <-  "Log(X) [Bin Centres] "
Y.Lab                   <-  "Count"
Main                    <-  "Histogram: Log(X) ~ Count"


# Need to fix the labels for this chart.

Lib.LogPlotWindow.X(MaxPos, MaxNeg, LogMin, Y.Lim = Y.Lim , X.Lab = X.Lab, 
                    Y.Lab =  Y.Lab, Main = Main)

Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = Lims.X[[1]])
Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = Lims.X[[2]], PlotAxis = 0 )

points(Scales.Centres, Counts, type = 'h' )
points(Scales.Centres, Counts, pch = 21, bg = Col )

}

######################################################################################################
# Histogram: X ~ Log(Count)

Lib.GA.1D.Hist.LogCount  <-  function(X, N, LogMin, Col ){
  
  X.Name  <-  "X"
  
 if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}  
 if(missing(Col)){Col  <-  "blue"}
  par(mar =  c(6.1, 4.1, 4.1, 2.1))
  if(missing(N))  {N    <-  51}
  
  
X.Axis      <-  Lib.Axis.Smart(X)
X.Lim       <-  X.Axis$Lim
Breaks      <-  signif(seq(X.Lim[1], X.Lim[2], (X.Lim[2] - X.Lim[1]) / (N - 1)), 2)

Hist        <-  Lib.Hist.Breaks(X, Breaks)
Centres     <-  Hist$Centres
Counts      <-  Hist$Counts

if(missing(LogMin)){LogMin         <-  Lib.LogPlot.GoodMin(Counts)}

Min.Y       <-  min(Counts)
Max.Y       <-  max(Counts)

if(Min.Y < 0){MaxNeg    <-   ceiling(log(abs(min(Counts)), 10)) } else {MaxNeg  <- LogMin }
if(Max.Y > 0){MaxPos    <-   ceiling(log(abs(max(Counts)), 10)) } else {MaxPos  <- LogMin }

Lims.Y                  <-  Lib.LogPlotWindow.AltLines(MaxPos, MaxNeg, LogMin)


Scales.Counts           <-  Lib.LogPlotWindow.Scale(Counts , MaxPos, MaxNeg, LogMin)

X.Lab                   <-  "X [Bin Centres] "
Y.Lab                   <-  "Log(Count)"
Main                    <-  "Histogram: X ~ Log(Count)"


Lib.LogPlotWindow.Y(MaxPos, MaxNeg, LogMin, X.Lim = X.Lim, Plot.New = 1,
                    Line.Lims.1 = X.Lim,  Line.Lims.2 = X.Lim, X.Lab = X.Lab, 
                    Y.Lab =  Y.Lab, Main = Main)

Lib.PlotLines.X(X.Axis$Axis, Y.Lim = Lims.Y[[1]])
Lib.PlotLines.X(X.Axis$Axis, Y.Lim = Lims.Y[[2]], PlotAxis = 0 )

points(Centres, Scales.Counts, type = 'h' )
points(Centres, Scales.Counts, pch = 21, bg = Col )
}

######################################################################################################
# Log Log Histogram 

Lib.GA.1D.Hist.LogLog  <-  function(X, LogMin.X, LogMin.Count, Col ){
  
  X.Name  <-  "X"
  
  par(mar =  c(6.1, 4.1, 4.1, 2.1))
  
if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}   
if(missing(Col)){Col  <-  "blue"}
if(missing(LogMin.X)){LogMin.X  <-  Lib.LogPlot.GoodMin(X)}
  
Hist         <-  Lib.LogHist(X, Min = LogMin.X)
Centres      <-  Hist$Centres
Counts       <-  Hist$Counts

if(missing(LogMin.Count)){LogMin.Count  <-  Lib.LogPlot.GoodMin(Counts)}

Min.X        <-  min(Centres)
Max.X        <-  max(Centres)

if(Min.X < 0){MaxNeg.X  <-   ceiling(log(abs(min(Centres)), 10)) } else {
                                                               MaxNeg.X  <- LogMin.X }
if(Max.X > 0){MaxPos.X  <-   ceiling(log(abs(max(Centres)), 10)) } else {
                                                               MaxPos.X  <- LogMin.X }

Min.Y        <-  min(Counts)
Max.Y        <-  max(Counts)

if(Min.Y < 0){MaxNeg.Y  <-   ceiling(log(abs(min(Counts)), 10)) } else {
                                                        MaxNeg.Y  <- LogMin.Count }

if(Max.Y > 0){MaxPos.Y  <-   ceiling(log(abs(max(Counts)), 10)) } else {
                                                        MaxPos.Y  <- LogMin.Count }


Lims.Y          <-  Lib.LogPlotWindow.AltLines(MaxPos.Y, MaxNeg.Y, LogMin.Count)
Lims.X          <-  Lib.LogPlotWindow.AltLines(MaxPos.X, MaxNeg.X, LogMin.X)

Scale.Centres   <-  Lib.LogPlotWindow.Scale(Centres, MaxPos.X, MaxNeg.X, LogMin.X)
Scale.Counts    <-  Lib.LogPlotWindow.Scale(Counts , MaxPos.Y, MaxNeg.Y, LogMin.Count)


X.Lab                   <-  "Log(X) [Bin Centres] "
Y.Lab                   <-  "Log(Count)"
Main                    <-  "Histogram: Log(X) ~ Log(Count)"



Lib.LogPlotWindow.X(MaxPos.X, MaxNeg.X, LogMin.X, 
                    Line.Lims.1 = Lims.Y[[1]],  Line.Lims.2 = Lims.Y[[2]],
                    X.Lab = X.Lab, Y.Lab =  Y.Lab, Main = Main)

Lib.LogPlotWindow.Y(MaxPos.Y, MaxNeg.Y, LogMin.Count, 
                    Line.Lims.1 = Lims.X[[1]],  Line.Lims.2 = Lims.X[[2]], Plot.New = 0 )



points(Scale.Centres, Scale.Counts, type = 'h' )
points(Scale.Centres, Scale.Counts, pch = 21, bg = Col)

}

######################################################################################################
# CumDists

Lib.GA.1D.CumDist.Basic  <-  function(X, Col ){
 
   X.Name  <-  "X"
if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}  
if(missing(Col)){Col  <-  "blue"}
  
  par(mar =  c(6.1, 4.1, 4.1, 2.1))
  
Y <-  seq(.1,.99, .01)

X.Quants   <-  quantile(X,Y)

X.Axis      <-  Lib.Axis.Smart(X.Quants)
X.Lim       <-  X.Axis$Lim

Y.Axis      <-  Lib.Axis.Smart(Y)
Y.Lim       <-  Y.Axis$Lim

Y.Lab       <-  "percentiles"
X.Lab       <-  X.Name 

Main        <-  paste0("Cumulative distribution of ", X.Name)



Lib.Plot.Blank(X.Lim,
               Y.Lim,
               X.Lab  = X.Lab, 
               Y.Lab  = Y.Lab,
               Main   = Main
)

Lib.PlotLines.X(X.Axis$Axis, Y.Lim = Y.Lim)
Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = X.Lim)

points(X.Quants, Y, type = 'h')
points(X.Quants, Y, pch=21, bg=Col)
}


######################################################################################################
# CumDist Log

Lib.GA.1D.CumDist.Log  <-  function(X, Col){

  X.Name  <-  "X"
  
if(missing(Col)){Col  <-  "blue"}  
if(missing(X)){X  <-  Lib.GA.1D.RandFunc()}  
  
  par(mar =  c(6.1, 4.1, 4.1, 2.1))
  
Y <-  seq(.1,.99, .01)

X.Quants     <-  quantile(X,Y)

LogMin      <-  Lib.Log.Mags(X)$Min 
MaxPos      <-  Lib.Log.Mags(X)$Pos
MaxNeg      <-  Lib.Log.Mags(X)$Neg

Lims.X          <-  Lib.LogPlotWindow.AltLines(MaxPos, MaxNeg, LogMin)

Y.Axis      <-  Lib.Axis.Smart(Y)
Y.Lim       <-  Y.Axis$Lim

Y.Lab       <-  "percentiles"
X.Lab       <-  X.Name 

Main        <-  paste0("Cumulative distribution of ", X.Name, " (logarithmic)")

X.Quants.Scaled  <-  Lib.LogPlotWindow.Scale(X.Quants,MaxPos,  MaxNeg, LogMin)

Lib.LogPlotWindow.X(MaxPos, MaxNeg, LogMin, Plot.New = 1,
                    Line.Lims.1 = Y.Lim,  Line.Lims.2 = Y.Lim, X.Lab = X.Lab, 
                    Y.Lab =  Y.Lab, Main = Main)

Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = Lims.X$Lim.1)
Lib.PlotLines.Y(Y.Axis$Axis, X.Lim = Lims.X$Lim.2, PlotAxis = 0)

points(X.Quants.Scaled, Y, type = 'h')
points(X.Quants.Scaled, Y, pch=21, bg=Col)

}


######################################################################################################










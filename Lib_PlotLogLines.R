Lib.PlotLogLines  <-  function(Min,Max, Ymin, Ymax, Lty, Lwd, Col){
  
  if(missing(Lty)){ Lty  <-  1}
  if(missing(Lwd)){ Lwd  <-  1}
  if(missing(Col)){ Col  <-  "black"}
  
  LogLines  <-  Lib.LogLines(Min, Max)
  
  LogVal    <-  LogLines$Axt.Lines.LogVal
  TickVal   <-  LogLines$Axt.Tick.Val
  
  
  N  <-  length(LogVal)
  
  
  for(i in 1:N){
  
   points(c(LogVal[i],LogVal[i]), c(Ymin,Ymax), type='l', lty=Lty, lwd=Lwd, col=Col)
    
  }
  
  axis(1, at=LogLines$Axt.Tick.Val, labels=LogLines$Axt.Tick.Label)  
  
  
}

######################################################################################################

Lib.PlotLogLines.Y  <-  function(PlotAxis,  X.Lim, Y.Min, Y.Max, X.Min, X.Max,
                                 Lwd.Big, Col.Big, Lty.Big, Lwd.Small, Col.Small, Lty.Small){
  
  if(missing(PlotAxis)){PlotAxis  <- 1}
  
  if(missing(Y.Min)){ Y.Min   <-  par()$yaxp[1]}
  if(missing(Y.Max)){ Y.Max   <-  par()$yaxp[2]}
  if(missing(X.Min)){ X.Min   <-  par()$xaxp[1]}
  if(missing(X.Max)){ X.Max   <-  par()$xaxp[2]}
  
  if(!missing(X.Lim)){X.Min  <-  X.Lim[1] 
                      X.Max  <-  X.Lim[2]}
  
  if(missing(Lwd.Big)){ Lwd.Big <- 2}
  if(missing(Lty.Big)){ Lty.Big <- 1}
  if(missing(Col.Big)){ 
    Col.Big <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6, Plot = 0)[3,3]}
  
  if(missing(Lwd.Small)){Lwd.Small  <-  Lwd.Big / 2}
  if(missing(Lty.Small)){Lty.Small  <-  2}
  if(missing(Col.Small)){Col.Small  <-  Col.Big }
  
  
  
  LogCalcs  <-  Lib.LogLines(Y.Min, Y.Max)
  
  if(PlotAxis == 1){
    
    axis(2, at=LogCalcs$Axt.Tick.Val, labels = LogCalcs$Axt.Tick.Label)
    
  }
  
  N.Lines   <-  length(LogCalcs$Axt.Lines.LogVal)
  
  for(i in 1:N.Lines){
    
    Y  <- LogCalcs$Axt.Lines.LogVal[i] 
    
    Big   <-  Y - floor(Y) == 0
    
    if(Big){
      
      points(c(X.Min, X.Max), c(Y, Y), type='l', lwd=Lwd.Big, lty = Lty.Big, col = Col.Big)
      
    } else {
      
      points(c(X.Min, X.Max), c(Y, Y), type='l', lwd=Lwd.Small, lty = Lty.Small, col = Col.Small)
      
    } # End if(Big)
    
  } # End for(i)
  
  
}
######################################################################################################

Lib.PlotLogLines.X  <-  function(PlotAxis, Y.Lim, X.Lim, X.Min, X.Max, Y.Min, Y.Max,
                                 Lwd.Big, Col.Big, Lty.Big, Lwd.Small, Col.Small, Lty.Small){
  
  if(missing(PlotAxis)){PlotAxis  <- 1}
  
  if(missing(Y.Min)){ Y.Min   <-  par()$yaxp[1]}
  if(missing(Y.Max)){ Y.Max   <-  par()$yaxp[2]}
  if(missing(X.Min)){ X.Min   <-  par()$xaxp[1]}
  if(missing(X.Max)){ X.Max   <-  par()$xaxp[2]}
  
  if(missing(Lwd.Big)){ Lwd.Big <- 2}
  if(missing(Lty.Big)){ Lty.Big <- 1}
  if(missing(Col.Big)){ 
    Col.Big <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6, Plot = 0)[3,3]}
  
  if(missing(Lwd.Small)){Lwd.Small  <-  Lwd.Big / 2}
  if(missing(Lty.Small)){Lty.Small  <-  2}
  if(missing(Col.Small)){Col.Small  <-  Col.Big }
  
  if(!missing(X.Lim)){X.Min  <-  X.Lim[1] 
                      X.Max  <-  X.Lim[2]}
  
  if(!missing(Y.Lim)){Y.Min   <-  Y.Lim[1] 
                      Y.Max   <-  Y.Lim[2]}
  
  
  
  LogCalcs  <-  Lib.LogLines(X.Min, X.Max)
  
  if(PlotAxis == 1){
    
    axis(1, at=LogCalcs$Axt.Tick.Val, labels = LogCalcs$Axt.Tick.Label)
    
  }
  
  N.Lines   <-  length(LogCalcs$Axt.Lines.LogVal)
  
  for(i in 1:N.Lines){
    
    X  <- LogCalcs$Axt.Lines.LogVal[i] 
    
    Big   <-  X - floor(X) == 0
    
    if(Big){
      
      points(c(X, X), c(Y.Min, Y.Max),  type='l', lwd=Lwd.Big, lty = Lty.Big, col = Col.Big)
      
    } else {
      
      points(c(X, X), c(Y.Min, Y.Max), type='l', lwd=Lwd.Small, lty = Lty.Small, col = Col.Small)
      
    } # End if(Big)
    
  } # End for(i)
  
  
}
######################################################################################################

Lib.PlotLines.Y  <-  function(Y.Axis, X.Lim, X.Min, X.Max,
                              Lwd, Col, Lty, PlotAxis, Labels, Cex){
  
  
  if(missing(PlotAxis)){PlotAxis  <- 1}
  if(missing(Labels)){Labels  <-  Lib.BigNumbers(Y.Axis)}
  if(missing(Cex)){Cex  <-  1}
  

  if(missing(X.Min)){ X.Min   <-  par()$xaxp[1]}
  if(missing(X.Max)){ X.Max   <-  par()$xaxp[2]}
  
  if(!missing(X.Lim)){X.Min  <-  X.Lim[1] 
                      X.Max  <-  X.Lim[2]}
  
  
  
  if(missing(Lwd)){Lwd <- 1}
  if(missing(Lty)){Lty <- 2}
  if(missing(Col)){ 
    Col <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6, Plot = 0)[3,3]}
  
  if(PlotAxis == 1){
    
    axis(2, at=Y.Axis, labels = Labels, cex.axis=Cex)
    
  }
  
  N.Lines  <-  length(Y.Axis)
    
  for(i in 1:N.Lines){
    
    Y  <-  Y.Axis[i]
    points(c(X.Min, X.Max), c(Y, Y), type='l', lwd=Lwd, lty = Lty, col = Col)
    
  }  
      
}

######################################################################################################

Lib.PlotLines.X  <-  function(X.Axis, Y.Lim, Y.Min, Y.Max,
                              Lwd, Col, Lty, PlotAxis, Labels, Cex){
  
  # Note that string rotation doesnt work in axis function
  
  if(missing(PlotAxis)){PlotAxis  <-  1}
  if(missing(Labels)){Labels      <-  Lib.BigNumbers(X.Axis)}
  if(missing(Cex)){Cex            <-  1}

  
  
  if(missing(Y.Min)){ Y.Min   <-  par()$yaxp[1]}
  if(missing(Y.Max)){ Y.Max   <-  par()$yaxp[2]}
  
  if(!missing(Y.Lim)){Y.Min   <-  Y.Lim[1] 
                      Y.Max   <-  Y.Lim[2]}
  
  
  
  if(missing(Lwd)){Lwd <- 1}
  if(missing(Lty)){Lty <- 2}
  if(missing(Col)){ 
  Col <-  Lib.ColourScheme.ExpCol(36, 5, Alpha = .7, ExpCol = 24, K = 6, Plot = 0)[3,3]}
  
  if(PlotAxis == 1){
    
    axis(1, at=X.Axis, labels = Labels, cex.axis=Cex)
    
  }
  
  N.Lines  <-  length(X.Axis)
  
  for(i in 1:N.Lines){
    
    X  <-  X.Axis[i]
    points(c(X, X), c(Y.Min, Y.Max), type='l', lwd=Lwd, lty = Lty, col = Col)
    
  }  
  
}










# 
# Data  <-   Lib.Markov(N= 5, M= 3)$V
# 
# Leg.Labels    <-  rownames(Data[[1]])
# ShapeAlpha        <-  1
# ShapeBeta          <-  ShapeAlpha
# Chord.Fills  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = .7,
#                                   Plot = 0 )[3,]

Lib.MultiAlluvial   <-  function(Data, 
                                 Chord.Fills, 
                                 Chord.Bords,
                                 Block.Fills,
                                 Block.Bords,
                                 Block.Lwd,
                                 Gap,
                                 Marge,
                                 Marge.Left,
                                 Marge.Right,
                                 Marge.Central,
                                 Plot.New,
                                 ShapeAlpha,
                                 ShapeBeta,
                                 Leg.X,
                                 Leg.Y,
                                 Leg.Labels,
                                 Plot.Leg,
                                 Leg.Cex.Text,
                                 Leg.Cex,
                                 Output){
 
  if(missing(Data)){Data  <-   Lib.Markov(N= 5, M= 3)$V}
  N  <-  nrow(Data[[1]])
  
  if(missing(Marge)){Marge                  <-  .05}
  if(missing(Marge.Left)){Marge.Left        <-  Marge}
  if(missing(Marge.Right)){Marge.Right      <-  Marge.Left}
  if(missing(Marge.Central)){Marge.Central  <-  Marge.Right * .25}
  if(missing(Leg.X)){Leg.X                  <-  1.05}
  if(missing(Leg.Y)){Leg.Y                  <-  1}
  if(missing(Plot.Leg)){Plot.Leg            <-  0}
  if(missing(Leg.Cex.Text)){Leg.Cex.Text    <-  1}
  if(missing(Leg.Cex)){Leg.Cex              <-  1}
  if(missing(Output)){Output                <-  0}
  if(missing(ShapeAlpha)){ShapeAlpha        <-  1}
  if(missing(ShapeBeta)){ShapeBeta          <-  ShapeAlpha}
  if(missing(Gap)){Gap                      <- .1}
  
  if(missing(Leg.Labels)){Leg.Labels    <-  rownames(Data[[1]])}
 
  
  if(missing(Chord.Fills)){
    Chord.Fills  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = .7,
                                      Plot = 0 )[3,]}
  
  if(missing(Chord.Bords)){
    Chord.Bords  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = 1 ,
                                      Plot = 0)[1,]}
  
  
  if(missing(Block.Fills)){
    Block.Fills  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = 1,
                                      Plot = 0 )[1,]}
  
  
  if(missing(Block.Bords)){
    Block.Bords  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = 1,
                                      Plot = 0 )[1,]}
  

  #====================================================================================================# 
  
  if(is.null(Leg.Labels)){Leg.Labels    <-  colnames(Data[[1]])}
  if(is.null(Leg.Labels)){Leg.Labels    <-  seq(1, N)}
  
  if(is.matrix(Data)){Data   <- list(Data)}
  
  M  <-   length(Data)
  
  Leg.Fills   <-  Block.Fills
  Leg.Bords   <-  Block.Bords
  
  par(xpd = NA)
  
#====================================================================================================# 
  
  
  Args   <-  list("Data"           = Data, 
                  "Chord.Fills"    = Chord.Fills, 
                  "Chord.Bords"    = Chord.Bords,
                  "Block.Fills"    = Block.Fills,
                  "Block.Bords"    = Block.Bords,
                  "Gap"            = Gap,
                  "Marge"          = Marge,
                  "Marge.Left"     = Marge.Left,
                  "Marge.Right"    = Marge.Right,
                  "Marge.Central"  = Marge.Central,
                  "ShapeAlpha"     = ShapeAlpha,
                  "ShapeBeta"      = ShapeBeta,
                  "Leg.X"          = Leg.X,
                  "Leg.Y"          = Leg.Y,
                  "Leg.Labels"     = Leg.Labels,
                  "Plot.Leg"       = Plot.Leg,
                  "Leg.Cex.Text"   = Leg.Cex.Text,
                  "Leg.Cex"        = Leg.Cex,
                  "Output"         = Output)
  
  
#---- Calculate the Xlim breaks
  
  X.Breaks  <-  seq(0, 1, 1/M)    
  
#====================================================================================================#
  
  SubOutput  <-  list()
  
  
  plot(c(0,1), c(0,1), type='n', bty ='n', xlab= "", ylab="", xaxt = 'n', yaxt='n')
  
  for(i in 1:M){
    
    # Xlim
    Xlim    <-  c(X.Breaks[i], X.Breaks[i+1])
    
    # Determine margines
    if(i == 1){i.Marge.Left  <- Marge.Left  } else {
      i.Marge.Left <- Marge.Central / 2}
    
    
    if(i == M){i.Marge.Right  <- Marge.Right} else {
    i.Marge.Right <- Marge.Central / 2}
    
    
    # Execute sub-plot
    i.Output  <-  Lib.Alluvial(Data[[i]], 
                               Xlim          = Xlim, 
                               Plot.New      = 0, 
                               Marge.Right   = i.Marge.Right,
                               Marge.Left    = i.Marge.Left,
                               ShapeAlpha    = ShapeAlpha,
                               ShapeBeta     = ShapeBeta,
                               Chord.Fills   = Chord.Fills,
                               Chord.Bords   = Chord.Bords,
                               Block.Fills   = Block.Fills,
                               Block.Bords   = Block.Bords,
                               Block.Lwd     = Block.Lwd,
                               Output        = 1,
                               Gap           = Gap     
                               )
    
    SubOutput[[i]]  <-  i.Output
    
    
  }
  
  #====================================================================================================#
  
  if(Plot.Leg == 1){
    
    Lib.Legend(Leg.Labels, X = Leg.X, Y = Leg.Y, Gap = .01, dY = .05, Cols = Leg.Fills,
               BorderCol = Leg.Bords, TextSize = Leg.Cex.Text, PointSize = Leg.Cex  )
  }
  
  
  
  
  if(Output == 1){
  
  Output  <-  list("Args"      = Args,
                   "SubOutput" = SubOutput)
  
  
  return(Output)
  
}}
  
  
  
  
  
  
  
  
  
  
  
  


# Table.1   <-  array(dim=c(4,5), 10 * runif(20), dimnames = list(1:4, LETTERS[1:5]))
# Table.2   <-  array(dim=c(5,3), 10 * runif(15), dimnames = list(LETTERS[1:5], 5:7 ))
# Table.3   <-  array(dim=c(3,6), 10 * runif(18), dimnames = list(LETTERS[1:3], seq(1,6)))
# 
# 
# 
# Data      <-   list(Table.1, Table.2, Table.3)

Lib.MultiAlluvial   <-  function(Data, 
                                 Chord.Fills.Map, 
                                 Chord.Bords.Map,
                                 Block.Fills.Map,
                                 Block.Bords.Map,
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
                                 Chord.Fills.Force, 
                                 Chord.Bords.Force,
                                 Block.Fills.Force,
                                 Block.Bords.Force,
                                 Output, 
                                 Text.OnChart,
                                 Text.OnChart.Cex){
 
  if(missing(Data)){Data  <-   Lib.Markov(N= 5, M= 3)$V}

  
  
  if(missing(Marge)){Marge                          <-  .05}
  if(missing(Marge.Left)){Marge.Left                <-  Marge}
  if(missing(Marge.Right)){Marge.Right              <-  Marge.Left}
  if(missing(Marge.Central)){Marge.Central          <-  Marge.Right * .25}
  if(missing(Leg.X)){Leg.X                          <-  1.05}
  if(missing(Leg.Y)){Leg.Y                          <-  1}
  if(missing(Plot.Leg)){Plot.Leg                    <-  0}
  if(missing(Leg.Cex.Text)){Leg.Cex.Text            <-  1}
  if(missing(Leg.Cex)){Leg.Cex                      <-  1}
  if(missing(Output)){Output                        <-  0}
  if(missing(ShapeAlpha)){ShapeAlpha                <-  1}
  if(missing(ShapeBeta)){ShapeBeta                  <-  ShapeAlpha}
  if(missing(Gap)){Gap                              <- .1}
  if(missing(Leg.Labels)){Leg.Labels                <-  rownames(Data[[1]])}
  if(missing(Chord.Fills.Map)){Chord.Fills.Map      <-  numeric(0)}
  if(missing(Chord.Bords.Map)){Chord.Bords.Map      <-  numeric(0)}
  if(missing(Block.Fills.Map)){Block.Fills.Map      <-  numeric(0)}
  if(missing(Block.Bords.Map)){Block.Bords.Map      <-  numeric(0)}
  if(missing(Block.Lwd)){Block.Lwd                  <-  1}
  if(missing(Text.OnChart)){Text.OnChart            <-  0}
  if(missing(Text.OnChart.Cex)){Text.OnChart.Cex    <-  1}
  
  
  
  
#====================================================================================================# 
  K  <-  length(Data)
  
  
  # Fix up dimnames if needed
  
  if(is.null(rownames(Data[[1]]))){ rownames(Data[[1]]) <-  seq(1,nrow(Data[[1]]))}
 
  if(is.null(colnames(Data[[1]]))){
    if(nrow(Data[[1]]) == ncol(Data[[1]])){colnames(Data[[1]])  <-  rownames(Data[[1]])} else {
      colnames(Data[[1]]) <-  seq(1,ncol(Data[[1]]))
    }
  }

  
  for(i in 2:K){

    if(is.null(rownames(Data[[i]]))){ 
     if(nrow(Data[[i]]) == ncol(Data[[i-1]])){rownames(Data[[i]])  <-  colnames(Data[[i-1]])} else {
      rownames(Data[[i]]) <-  seq(1,nrow(Data[[1]]))
     } 
    }
    
  if(is.null(colnames(Data[[i]]))){
    if(nrow(Data[[i]]) == ncol(Data[[i]])){colnames(Data[[i]])  <-  rownames(Data[[i]])} else {
      colnames(Data[[i]]) <-  seq(1,ncol(Data[[i]]))
    }
  }
}
  
AllNames     <-  unique(unlist(lapply(Data, dimnames)))
N.AllNames   <-  length(AllNames)  

#====================================================================================================# 
  
# Complete the maps:

Default.Chord.Fills   <-   Lib.ColourScheme(N.AllNames, 5, V = .75, BG = "white", Alpha = .7,
                                      Plot = 0 )[3,]
 
Default.Chord.Bords   <-   Lib.ColourScheme(N.AllNames, 5, V = .75, BG = "white", Alpha = 1 ,
                                      Plot = 0)[1,]

Default.Block.Fills   <-   Lib.ColourScheme(N.AllNames, 5, V = .75, BG = "white", Alpha = 1,
                                      Plot = 0 )[1,]

Default.Block.Bords   <-   Lib.ColourScheme(N.AllNames, 5, V = .75, BG = "white", Alpha = 1,
                                      Plot = 0 )[1,]

x.Chord.Fills.Map       <-  Chord.Fills.Map[match(AllNames, names(Chord.Fills.Map))]
x.Chord.Bords.Map       <-  Chord.Bords.Map[match(AllNames, names(Chord.Bords.Map))]
x.Block.Fills.Map       <-  Block.Fills.Map[match(AllNames, names(Block.Fills.Map))]
x.Block.Bords.Map       <-  Block.Bords.Map[match(AllNames, names(Block.Bords.Map))]


x.Chord.Fills.Map[which(is.na(x.Chord.Fills.Map))]  <-  Default.Chord.Fills[which(is.na(x.Chord.Fills.Map))]
x.Chord.Bords.Map[which(is.na(x.Chord.Bords.Map))]  <-  Default.Chord.Fills[which(is.na(x.Chord.Bords.Map))]
x.Block.Fills.Map[which(is.na(x.Block.Fills.Map))]  <-  Default.Chord.Fills[which(is.na(x.Block.Fills.Map))]
x.Block.Bords.Map[which(is.na(x.Block.Bords.Map))]  <-  Default.Chord.Fills[which(is.na(x.Block.Bords.Map))]


Chord.Fills  <-  list()
Chord.Bords  <-  list()
Block.Fills  <-  list()
Block.Bords  <-  list()


RowNames     <-  lapply(Data, rownames)
ColNames     <-  lapply(Data, colnames)
Dims         <-  lapply(Data, dim)

for(i in 1:K){
  
  Chord.Fills[[i]]  <-  array(dim=Dims[[i]], x.Chord.Fills.Map[match(RowNames[[i]], AllNames)])
  Chord.Bords[[i]]  <-  array(dim=Dims[[i]], x.Chord.Bords.Map[match(RowNames[[i]], AllNames)])
  Block.Fills[[i]]  <-  array(dim=Dims[[i]], x.Block.Fills.Map[match(RowNames[[i]], AllNames)])
  Block.Bords[[i]]  <-  array(dim=Dims[[i]], x.Block.Bords.Map[match(RowNames[[i]], AllNames)])
  
  
}


if(missing(Chord.Fills.Force)){Chord.Fills.Force  <-  Chord.Fills}
if(missing(Chord.Bords.Force)){Chord.Bords.Force  <-  Chord.Bords}
if(missing(Block.Fills.Force)){Block.Fills.Force  <-  Block.Fills}
if(missing(Block.Bords.Force)){Block.Bords.Force  <-  Block.Bords}



  #====================================================================================================# 
  
  if(is.null(Leg.Labels)){Leg.Labels    <-  colnames(Data[[1]])}
  if(is.null(Leg.Labels)){Leg.Labels    <-  seq(1, N)}
  
  if(is.matrix(Data)){Data   <- list(Data)}
  
  M  <-   length(Data)
  
  Leg.Fills   <-  Block.Fills
  Leg.Bords   <-  Block.Bords
  
  par(xpd = NA)
  
#====================================================================================================# 
  
  
  Args   <-  list("Data"              = Data, 
                  "Chord.Fills"       = Chord.Fills, 
                  "Chord.Bords"       = Chord.Bords,
                  "Block.Fills"       = Block.Fills,
                  "Block.Bords"       = Block.Bords,
                  "Gap"               = Gap,
                  "Marge"             = Marge,
                  "Marge.Left"        = Marge.Left,
                  "Marge.Right"       = Marge.Right,
                  "Marge.Central"     = Marge.Central,
                  "ShapeAlpha"        = ShapeAlpha,
                  "ShapeBeta"         = ShapeBeta,
                  "Leg.X"             = Leg.X,
                  "Leg.Y"             = Leg.Y,
                  "Leg.Labels"        = Leg.Labels,
                  "Plot.Leg"          = Plot.Leg,
                  "Leg.Cex.Text"      = Leg.Cex.Text,
                  "Leg.Cex"           = Leg.Cex,
                  "Chord.Fills.Force" = Chord.Fills.Force,
                  "Chord.Bords.Force" = Chord.Bords.Force,
                  "Block.Fills.Force" = Block.Fills.Force,
                  "Block.Bords.Force" = Block.Bords.Force,
                  "Output"            = Output)
  
  
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
                               Chord.Fills   = Chord.Fills.Force[[i]],
                               Chord.Bords   = Chord.Bords.Force[[i]],
                               Block.Fills   = Block.Fills.Force[[i]],
                               Block.Bords   = Block.Bords.Force[[i]],
                               Block.Lwd     = Block.Lwd,
                               Output        = 1,
                               Gap           = Gap     
                               )
    
    SubOutput[[i]]  <-  i.Output
    
    
  }
  
#====================================================================================================#
# Text on chart

if(Text.OnChart == 1){

  SubOutput[[M]]
  
Text.Left.X    <-    SubOutput[[1]]$Blocks.Left.X1
Text.Left.Y0   <-    apply(SubOutput[[1]]$Chords.Left.Y1 , 1, max)
Text.Left.Y1   <-    apply(SubOutput[[1]]$Chords.Left.Y0 , 1, min)


Text.Right.X    <-    SubOutput[[M]]$Blocks.Right.X0
Text.Right.Y0   <-    apply(SubOutput[[M]]$Chords.Right.Y1 , 2, max)
Text.Right.Y1   <-    apply(SubOutput[[M]]$Chords.Right.Y0 , 2, min)


text(Text.Left.X,   (Text.Left.Y0 + Text.Left.Y1) / 2, Leg.Labels, cex = Text.OnChart.Cex, pos = 4)
text(Text.Right.X, (Text.Right.Y0 + Text.Right.Y1) / 2, Leg.Labels, cex = Text.OnChart.Cex, pos = 2)

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
  
  
  
  
  
  
  
  
  
  
  
  

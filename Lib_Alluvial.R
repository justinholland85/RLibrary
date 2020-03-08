# N  <- nrow(Data)
# M  <- ncol(Data)
# R  <-  100
# 
# 
# Data         <-  array(dim = c(N,N), ceiling(R * runif(N * N)))
# Chord.Fills  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = .7, Plot = 0 )[3,]
# Chord.Bords  <-  Lib.ColourScheme(N, 5, V = .75, BG = "white", Alpha = 1,  Plot = 0)[1,]
# Block.Fills  <-  Lib.ColourScheme(M, 5, V = .75, BG = "white", Alpha = 1,  Plot = 0)[1,]
# Block.Bords  <-  Lib.ColourScheme(M, 5, V = .75, BG = "white", Alpha = 1,  Plot = 0 )[1,]
# Block.Lwd    <-  2
# 
# Gap          <-  .1
# Xlim         <-  c(0, 1)
# Ylim         <-  c(0, 1)
# Marge        <-  .1
# Marge.Left   <-  .1
# Marge.Right  <-  .1
# Plot.New     <-  1
# ShapeAlpha   <-  1
# ShapeBeta    <-  1
# Leg.X        <-  1.05
# Leg.Y        <-  1
# Leg.Labels   <-  seq(1, N)
# Plot.Leg     <-  1
# Leg.Cex.Text <-  1
# Leg.Cex      <-  2
# Chord.Lwd    <-  1


Lib.Alluvial  <-  function(Data, 
                           Chord.Fills, 
                           Chord.Bords,
                           Chord.Lwd,
                           Block.Fills,
                           Block.Bords,
                           Block.Lwd,
                           Xlim,
                           Ylim,
                           Gap,
                           Marge,
                           Marge.Left,
                           Marge.Right,
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


if(missing(Data)){Data  <-  Lib.Markov(N = 5)$V}
     
N            <-  nrow(Data)
M            <-  ncol(Data)

Max.NM       <-  max(N,M)

AllNames     <-  unique(unlist(lapply(Data, dimnames)))
N.AllNames   <-  length(AllNames)  



if(missing(Chord.Fills)){
     Chord.Fills  <-  Lib.ColourScheme(Max.NM, 5, V = .75, BG = "white", Alpha = .7,
                                       Plot = 0 )[3,]}

if(missing(Chord.Bords)){
     Chord.Bords  <-  Lib.ColourScheme(Max.NM, 5, V = .75, BG = "white", Alpha = 1 ,
                                       Plot = 0)[1,]}


if(missing(Block.Fills)){
     Block.Fills  <-  Lib.ColourScheme(Max.NM, 5, V = .75, BG = "white", Alpha = 1,
                                       Plot = 0 )[1,]}


if(missing(Block.Bords)){
     Block.Bords  <-  Lib.ColourScheme(Max.NM, 5, V = .75, BG = "white", Alpha = 1,
                                       Plot = 0 )[1,]}

if(missing(Marge)){Marge              <-  .05}
if(missing(Marge.Left)){Marge.Left    <-  Marge}
if(missing(Marge.Right)){Marge.Right  <-  Marge.Left}
if(missing(Plot.New)){Plot.New        <-  1}
if(missing(Xlim)){Xlim                <-  c(0,1)}
if(missing(Ylim)){Ylim                <-  c(0,1)}
if(missing(Leg.X)){Leg.X              <-  1.05}
if(missing(Leg.Y)){Leg.Y              <-  1}
if(missing(Plot.Leg)){Plot.Leg        <-  0}
if(missing(Leg.Cex.Text)){Leg.Cex.Text<-  1}
if(missing(Leg.Cex)){Leg.Cex          <-  1}
if(missing(Block.Lwd)){Block.Lwd      <-  2}
if(missing(Output)){Output            <-  0}
if(missing(Gap)){Gap                  <- .1}
if(missing(ShapeAlpha)){ShapeAlpha    <- 1}
if(missing(ShapeBeta)){ShapeBeta      <- ShapeAlpha}
if(missing(Chord.Lwd)){Chord.Lwd      <- 1}

if(missing(Leg.Labels)){Leg.Labels    <-  rownames(Data)}
if(is.null(Leg.Labels)){Leg.Labels    <-  colnames(Data)}
if(is.null(Leg.Labels)){Leg.Labels    <-  seq(1, N)}


Leg.Fills   <-  Block.Fills
Leg.Bords   <-  Block.Bords

par(xpd = NA)


if(is.vector(Chord.Fills)){Chord.Fills  <-  array(dim=c(N,M), Chord.Fills[1:N])}
if(is.vector(Chord.Bords)){Chord.Bords  <-  array(dim=c(N,M), Chord.Bords[1:N])}


Args   <-  list("Data"         = Data, 
                "Chord.Fills"  = Chord.Fills, 
                "Chord.Bords"  = Chord.Bords,
                "Block.Fills"  = Block.Fills,
                "Block.Bords"  = Block.Bords,
                "Block.Lwd"    = Block.Lwd,
                "Xlim"         = Xlim,
                "Ylim"         = Ylim,
                "Gap"          = Gap,
                "Marge"        = Marge,
                "Marge.Left"   = Marge.Left,
                "Marge.Right"  = Marge.Right,
                "Plot.New"     = Plot.New,
                "ShapeAlpha"   = ShapeAlpha,
                "ShapeBeta"    = ShapeBeta,
                "Leg.X"        = Leg.X,
                "Leg.Y"        = Leg.Y,
                "Leg.Labels"   = Leg.Labels,
                "Plot.Leg"     = Plot.Leg,
                "Leg.Cex.Text" = Leg.Cex.Text,
                "Leg.Cex"      = Leg.Cex,
                "Output"       = Output)


#---- Determine proportions

A.Props     <-  rowSums(Data) / sum(Data) * (1 - Gap)
B.Props     <-  colSums(Data) / sum(Data) * (1 - Gap)
Props       <-  Data / sum(Data) * (1 - Gap)


#---- Calculate Y Outer Vals

A.Y.1       <-  rep(1, N)
A.Y.0       <-  rep(0, N)

B.Y.1       <-  rep(1, M)
B.Y.0       <-  rep(0, M)
 
Delta.A     <-  Gap / (N - 1)
Delta.B     <-  Gap / (M - 1)


for(i in 2:N){
  A.Y.1[i]  <-  A.Y.1[i - 1] - A.Props[i - 1] - Delta.A
}

for(i in 2:M){
  B.Y.1[i]  <-  B.Y.1[i - 1] - B.Props[i - 1] - Delta.B   
}


for(i in 1:N){
  A.Y.0[i]  <-  A.Y.1[i] - A.Props[i]
 }


for(i in 1:M){
   B.Y.0[i]  <-  B.Y.1[i] - B.Props[i]   
}


#---- Calculate Y Inner Vals

a.Y.1   <-  array(dim=c(N,M), 1)
a.Y.0   <-  array(dim=c(N,M), 1)

b.Y.1   <-  array(dim=c(N,M), 1)
b.Y.0   <-  array(dim=c(N,M), 1)


a.Y.1[,1]  <-  A.Y.1
b.Y.1[1,]  <-  B.Y.1

for(i in 2:M){ a.Y.1[,i]  <- a.Y.1[,i - 1]  - Props[ ,i- 1]}
for(i in 1:M){ a.Y.0[,i]  <- a.Y.1[,i] - Props[ ,i]}

for(i in 2:N){ b.Y.1[i,]  <- b.Y.1[i - 1, ]  - Props[i- 1, ]}
for(i in 1:N){ b.Y.0[i,]  <- b.Y.1[i,] - Props[i, ]}

#---- Calculate X Vals

A.X.0  <-  0
A.X.1  <-  Marge    

B.X.0  <-  1 - Marge.Right
B.X.1  <-  1    

#====================================================================================================#
# Now scale all X and Y values into the Xlim Ylim window

X0     <-  Xlim[1]
dX     <-  Xlim[2] - Xlim[1]

A.X.0  <-  X0 + (A.X.0 - 0) * dX 
A.X.1  <-  X0 + (A.X.1 - 0) * dX 

B.X.0  <-  X0 + (B.X.0 - 0) * dX 
B.X.1  <-  X0 + (B.X.1 - 0) * dX 

Leg.X  <-  X0 + (Leg.X - 0) * dX 

#----

Y0     <-  Ylim[1]
dY     <-  Ylim[2] - Ylim[1]

A.Y.0  <-  Y0 + (A.Y.0 - 0) * dY 
A.Y.1  <-  Y0 + (A.Y.1 - 0) * dY 

B.Y.0  <-  Y0 + (B.Y.0 - 0) * dY 
B.Y.1  <-  Y0 + (B.Y.1 - 0) * dY 

a.Y.0  <-  Y0 + (a.Y.0 - 0) * dY 
a.Y.1  <-  Y0 + (a.Y.1 - 0) * dY 

b.Y.0  <-  Y0 + (b.Y.0 - 0) * dY 
b.Y.1  <-  Y0 + (b.Y.1 - 0) * dY 

Leg.Y  <-  Y0 + (Leg.Y - 0) * dX 



#====================================================================================================#
# Calculate the Chords

Omega     <-  seq(0 , 1 , 1 / 50 )
Theta     <-  pi * pbeta(Omega, ShapeAlpha, ShapeBeta)

Chords.Y   <-  list()

for(i in 1:N){
for(j in 1:M){     
  
   k    <-  i + N * (j-1)
   
   dY   <-  -(b.Y.0[i,j] - a.Y.0[i,j])
   
   Y0   <-  a.Y.0[i,j] + (dY / 2) * (cos(Theta) - 1)
   Y1   <-  a.Y.1[i,j] + (dY / 2) * (cos(Theta) - 1)
   
   Chords.Y[[k]]  <-  c(Y0, rev(Y1))
     
}}

X0  <-  A.X.1 + (B.X.0 - A.X.1) * Omega
X1  <-  rev(X0)

Chords.X  <-  c(X0, X1)

#====================================================================================================#

if(Plot.New == 1){
     plot(c(0,1), c(0,1), type='n', bty ='n', xlab= "", ylab="", xaxt = 'n', yaxt='n')
}

#---- Draw the Chords


for(i in 1:N){
     for(j in 1:M){
          
          k    <-  i + N * (j-1)   
          
          polygon(Chords.X, Chords.Y[[k]], col = Chord.Fills[i, j], 
                  border = Chord.Bords[i, j], lwd =  Chord.Lwd)
          
          
     }}


#---- Draw the blocks


for(i in 1:N){
     
     X   <-  c(A.X.0, A.X.0, A.X.1, A.X.1)
     Y   <-  c(A.Y.0[i], A.Y.1[i], A.Y.1[i], A.Y.0[i]) 
     
     polygon(X, Y, col = Block.Fills[i], border = Block.Bords[i], lwd = Block.Lwd)
}
 
for(i in 1:M){    
     
     X   <-  c(B.X.0, B.X.0, B.X.1, B.X.1)
     Y   <-  c(B.Y.0[i], B.Y.1[i], B.Y.1[i], B.Y.0[i]) 
     
     polygon(X, Y, col = Block.Fills[i], border = Block.Bords[i],  lwd = Block.Lwd)
     
}

#---- 

if(Plot.Leg == 1){

Lib.Legend(Leg.Labels, X = Leg.X, Y = Leg.Y, Gap = .01, dY = .05, Cols = Leg.Fills,
           BorderCol = Leg.Bords, TextSize = Leg.Cex.Text, PointSize = Leg.Cex  )
}



#====================================================================================================#
# Collect output

if(Output == 1){
 
Output    <-  list("Blocks.Left.X0"    = A.X.0,
                   "Blocks.Left.X1"    = A.X.1,
                   "Blocks.Right.X0"   = B.X.0,
                   "Blocks.Right.X1"   = B.X.1,
                   "Chords.Left.Y0"    = a.Y.0, 
                   "Chords.Left.Y1"    = a.Y.1,
                   "Chords.Right.Y0"   = b.Y.0,
                   "Chords.Right.Y1"   = b.Y.1,
                   "Blocks.Left.Y0"    = A.Y.0,
                   "Blocks.Left.Y1"    = A.Y.1,
                   "Blocks.Right.Y0"   = B.Y.0,
                   "Blocks.Right.Y1"   = B.Y.1,
                   "Blocks.Left.Ymid"  = (A.Y.0 + A.Y.1) / 2,
                   "Blocks.Right.Ymid" = (B.Y.0 + B.Y.1) / 2,
                   "Chord.Fills"       = Chord.Fills,
                   "Chord.Bords"       = Chord.Bords,
                   "Args"              = Args)

return(Output)
}


}






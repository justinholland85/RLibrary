# N.X	      <-	24
# N.Y	      <-	5
# N.Z	      <-	2
# 
# Plot      <-  1
# Alpha     <-  1
# BG        <-  "white"
# 
# Min.X     <- 0
# Max.X     <- 1
# Min.Y     <- 0
# Max.Y     <- 1
# Min.Z     <- 1 / N.Z
# Max.Z     <- 1
# Order.HSV <- c("H", "S", "V")
# Round     <- 2
# 
# Rev.X     <- 0
# Rev.Y     <- 1
# Rev.Z     <- 0
# 
# Mfrow.Cols <-  ceiling(N.Plot^.5)
# Mfrow.Rows <-  ceiling(N.Plot / Mfrow.Cols)
# Mfrow      <-  c(Mfrow.Rows, Mfrow.Cols)
# 


######################################################################################################



######################################################################################################

Lib.ColourScheme3D	<-	function(N.X,N.Y,N.Z,Plot,Cex,Alpha,BG,
                                 Min.X, Max.X, Min.Y, Max.Y, Min.Z, Max.Z,
                                 Order.HSV, Round, Mfrow, Mfrow.Cols, Mfrow.Rows,
                                 Rev.X, Rev.Y, Rev.Z){

if(missing(N.X)){     	N.X	      <-	24}  
if(missing(N.Y)){  	    N.Y	      <-	5}
if(missing(N.Z)){  	    N.Z	      <-	1}
if(missing(Plot)){      Plot      <-  1}
if(Plot != 0){          Plot      <-  1}
if(missing(Alpha)){     Alpha     <-  1}
if(missing(BG)){        BG        <-  "white"}
if(missing(Min.X)){     Min.X     <- 0}
if(missing(Max.X)){     Max.X     <- 1}
if(missing(Min.Y)){     Min.Y     <- 0}
if(missing(Max.Y)){     Max.Y     <- 1}
if(missing(Min.Z)){     Min.Z     <- 1 / N.Z}
if(missing(Max.Z)){     Max.Z     <- 1}
if(missing(Order.HSV)){ Order.HSV <- c("H", "S", "V")}
if(missing(Round)){     Round     <- 2}

if(missing(Rev.X)){     Rev.X     <- 0}
if(missing(Rev.Y)){     Rev.Y     <- 1}
if(missing(Rev.Z)){     Rev.Z     <- 0}
  
  
Match     <-  match(c("H", "S", "V"), Order.HSV)

N.Plot    <-  N.Z         


# Set the Mfrow according to specification approach
if(missing(Mfrow.Cols) & missing(Mfrow.Rows)){
  Mfrow.Cols <- ceiling(N.Plot^.5)
  Mfrow.Rows <- ceiling(N.Plot / Mfrow.Cols)}
if(missing(Mfrow.Cols)){Mfrow.Cols<- ceiling(N.Plot / Mfrow.Rows)}
if(missing(Mfrow.Rows)){Mfrow.Rows<- ceiling(N.Plot / Mfrow.Cols)}
if(missing(Mfrow)){     Mfrow     <-  c(Mfrow.Rows, Mfrow.Cols)}



  
# Set plots per page  
par(mfrow = Mfrow)
  
# HSV sequence values
HSV.X   <-  (Min.X + seq(0, N.X) * (Max.X - Min.X) / N.X)[-(N.X + 1)]
HSV.Y   <-  (Min.Y + seq(0, N.Y) * (Max.Y - Min.Y) / N.Y)[-(N.Y + 1)]
HSV.Z   <-  (Min.Z + seq(0, N.Z) * (Max.Z - Min.Z) / N.Z)[-(N.Z + 1)]

if(Rev.X == 1){HSV.X  <-  rev(HSV.X)}
if(Rev.Y == 1){HSV.Y  <-  rev(HSV.Y)}
if(Rev.Z == 1){HSV.Z  <-  rev(HSV.Z)}


Cube     <-  array(dim=c(N.X, N.Y, N.Z))
Cube.HSV <-  array(dim=c(N.X, N.Y, N.Z))
Cube.RGB <-  array(dim=c(N.X, N.Y, N.Z))

  

    for(i in 1:N.X){
    for(j in 1:N.Y){
    for(k in 1:N.Z){  
      
      Vals.XYZ  <-  c(HSV.X[i], HSV.Y[j], HSV.Z[k])
      Vals.HSV  <-  Vals.XYZ[Match]
      
      
      t.H  <-  Vals.HSV[1]
      t.S  <-  Vals.HSV[2] 
      t.V  <-  Vals.HSV[3]
      
      
      Cube[i,j,k]     <- hsv(t.H, t.S, t.V, Alpha)
      Cube.HSV[i,j,k] <- paste0("H:",  round(t.H,Round), 
                                " S:", round(t.S,Round),
                                " V:", round(t.V,Round))
      
      RGB  <-  col2rgb(Cube[i,j,k]) / 255
      
      Cube.RGB[i,j,k] <- paste0("R:",  round(RGB[1],Round), 
                                " G:", round(RGB[2],Round),
                                " B:", round(RGB[3],Round))
  
      
      
    }}}

Temp      <- list()
Temp.HSV  <- list()
Temp.RGB  <- list()

for(i in 1:N.Z){
  
  Temp[[i]]      <- t(Cube[,,i])
  Temp.HSV[[i]]  <- t(Cube.HSV[,,i])
  Temp.RGB[[i]]  <- t(Cube.RGB[,,i])
  
}

Cube      <-  Temp
Cube.HSV  <-  Cube.HSV
Cube.RGB  <-  Cube.RGB


if(Plot == 1){
  
  Lib.Plot.ColouredGrid.3D(Cube, BG, Mfrow)
  
}

Output  <-  list("Col" = Cube,
                 "HSV" = Cube.HSV,
                 "RGB" = Cube.RGB,
                 "H"   = HSV.List[[Match[1]]],
                 "S"   = HSV.List[[Match[2]]],
                 "V"   = HSV.List[[Match[3]]] )

return(Output)
}


######################################################################################################
  
  
Lib.Plot.ColouredGrid.3D  <-  function(Cube, BG, Mfrow,  Mfrow.Cols, Mfrow.Rows){
  
  if(missing(BG)){          BG      <-  "white"}
  if(class(Cube) !=  "list"){Cube   <- list(Cube)}
  
  N  <- length(Cube)
  
  # Set the Mfrow accoring to specification approach
  if(missing(Mfrow.Cols) & missing(Mfrow.Rows)){
    Mfrow.Cols <- ceiling(N^.5)
    Mfrow.Rows <- ceiling(N / Mfrow.Cols)}
  if(missing(Mfrow.Cols)){Mfrow.Cols<- ceiling(N / Mfrow.Rows)}
  if(missing(Mfrow.Rows)){Mfrow.Rows<- ceiling(N / Mfrow.Cols)}
  if(missing(Mfrow)){     Mfrow     <-  c(Mfrow.Rows, Mfrow.Cols)}
  

  
  par(mfrow = Mfrow )
  par(mar = c(1,1,3,1))
 

  
  for(i in 1:N){
    
    Rect      <-  Cube[[i]]
    
    Nx        <-  ncol(Rect)
    Ny        <-  nrow(Rect)
    
    Labels.X  <-  seq(1, Nx)
    Labels.Y  <-  seq(1, Ny)
    
    Grid   <-  Lib.Grid.Coords(Nx, Ny )
    
    Lib.Grid.Draw(X = Grid$X,Y =Grid$Y, Fill = Rect,
                  Labels.X = Labels.X, Labels.Y = Labels.Y, Labels.Quad = c(1,1,1,1), 
                  Labels.Cex.Y = .8, Main = i   )
    
  }
  
}


  



######################################################################################################








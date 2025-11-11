
#====================================================================================================#
# Lib.Grid.Coords
#====================================================================================================#


Lib.Grid.Coords  <-  function(Nx, Ny, X0, X1, Y0, Y1){
  
  if(missing(X0)){X0  <-  0}
  if(missing(X1)){X1  <-  1}
  if(missing(Y0)){Y0  <-  0}
  if(missing(Y1)){Y1  <-  1}
  
  X   <-  X0 + seq(0, Nx) * (X1 - X0) / Nx
  Y   <-  Y1 - seq(0, Ny) * (Y1 - Y0) / Ny
  
  Output  <-  list("X" = X,
                   "Y" = Y)
  
  return(Output)
  
}  




#====================================================================================================#
# Lib.Gridify
#====================================================================================================#

Lib.Gridify  <-  function(Grid, X, Y){
  
  if(!missing(Grid)){X   <-  Grid$X ; Y  <-  Grid$Y}

  N.X     <-  length(X)
  N.Y     <-  length(Y)

  Mat.X   <-  t(array(dim = c(N.X, N.Y), X))
  Mat.Y   <-    array(dim = c(N.Y, N.X), Y)  
  
  Output  <-  list("Mat.X" = Mat.X,
                   "Mat.Y" = Mat.Y)
  
  return(Output)
  
}  





######################################################################################################

Lib.Grid.Draw  <-  function(X, Y, Fill, Border, New, Labels.X, Labels.Y, Labels.Gap,
                            Labels.Quad, Labels.Col, Labels.Cex, Labels.Cex.X, 
                            Labels.Cex.Y, Main){
  
  Nx   <-  length(X) - 1  
  Ny   <-  length(Y) - 1
  
  if(missing(Fill))  {       Fill          <-  array(dim=c(Ny,Nx), "transparent")}
  if(missing(Border)){       Border        <-  array(dim=c(Ny,Nx), "black")}
  if(missing(New)){          New           <-  1}
  if(missing(Labels.X)){     Labels.X      <-  rep("", Nx)}
  if(missing(Labels.Y)){     Labels.Y      <-  rep("", Ny)}
  if(missing(Labels.Gap)){   Labels.Gap    <- .02}
  if(missing(Labels.Quad)){  Labels.Quad   <- c(1,1,0,0)}
  if(missing(Labels.Col)){   Labels.Col    <- "black"}
  if(missing(Labels.Cex)){   Labels.Cex    <- 1}
  if(missing(Labels.Cex.X)){ Labels.Cex.X  <- Labels.Cex}
  if(missing(Labels.Cex.Y)){ Labels.Cex.Y  <- Labels.Cex}
  if(missing(Main)){         Main          <-  ""}
  
  
  par(xpd = NA)
  
  #----------------------------------------------------------------------------------------------------#
  # New blank plot iff required
  
  X0    <-  min(X)
  X1    <-  max(X)
  Y0    <-  min(Y)
  Y1    <-  max(Y)
  
  dX    <-  X1 - X0
  dY    <-  Y1 - Y0
  
  if(New == 1){Lib.Plot.Blank(X0 = X0, X1 = X1, Y0 = Y0 , Y1 = Y1, Main = Main)}
  
  
  #----------------------------------------------------------------------------------------------------#
  # Draw the grid
  
  for(i in 1:Ny){
    for(j in 1:Nx){
      
      polygon(c(X[j], X[j],  X[j+1], X[j+1]),
              c(Y[i], Y[i+1],Y[i+1], Y[i]),
              col=Fill[i,j], border = Border[i,j])
      
    }}
  
  
  #----------------------------------------------------------------------------------------------------#
  # Labels
  
  Labels.X1  <-  (X[1:Nx] + X[2:(Nx+1)]) / 2
  Labels.Y1  <- rep(Y0 - dY * Labels.Gap, Nx)
  
  Labels.X2  <-  rep(X0 - dX * Labels.Gap, Ny)
  Labels.Y2  <-  (Y[1:Ny] + Y[2:(Ny+1)]) / 2
  
  Labels.X3  <-  (X[1:Nx] + X[2:(Nx+1)]) / 2
  Labels.Y3  <-  rep(Y1  + dY * Labels.Gap, Nx)
  
  Labels.X4  <-  rep(X1 + dX * Labels.Gap, Ny)
  Labels.Y4  <-  (Y[1:Ny] + Y[2:(Ny+1)]) / 2
  
  if(Labels.Quad[1] == 1){
    text(Labels.X1, Labels.Y1, labels = Labels.X, 
         cex=Labels.Cex.X, col=Labels.Col, pos = 1, offset = 0)
  }
  
  if(Labels.Quad[2] == 1){
    text(Labels.X2, Labels.Y2, labels = Labels.Y, 
         cex=Labels.Cex.Y, col=Labels.Col, pos = 2, offset = 0)
  }
  
  if(Labels.Quad[3] == 1){
    text(Labels.X3, Labels.Y3, labels = Labels.X, 
         cex=Labels.Cex.X, col=Labels.Col, pos = 3, offset = 0)
  }
  
  if(Labels.Quad[4] == 1){
    text(Labels.X4, Labels.Y4, labels = Labels.Y, 
         cex=Labels.Cex.Y, col=Labels.Col, pos = 4, offset = 0)
  }
  
}






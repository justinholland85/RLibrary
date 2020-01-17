Lib.PlotRegions   <-  function(){
  
  
  Fin   <-  par()$fin
  Pin   <-  par()$pin
  Din   <-  par()$din
  Usr   <-  par()$usr
  Plt   <-  par()$plt
  Fig   <-  par()$fig
  Omi   <-  par()$omi
  
  # First objective is to find coordinates of the figure window
  
  dX  <-  (Usr[2] - Usr[1]) / (Plt[2] - Plt[1])
  dY  <-  (Usr[4] - Usr[3]) / (Plt[4] - Plt[3])
  
  Fig.X0  <-  Usr[1] - Plt[1] * dX
  Fig.X1  <-  Fig.X0 + dX
  
  Fig.Y0  <-  Usr[3] - Plt[3] * dY
  Fig.Y1  <-  Fig.Y0 + dY
  
  Figure   <-  c(Fig.X0, Fig.X1, Fig.Y0, Fig.Y1)
  
  # Second obejective is to find the device coordinates
  
  FigureProp   <-  Fin / Din
  
  dX   <-  (Figure[2] - Figure[1]) / Fin[1]
  dY   <-  (Figure[4] - Figure[3]) / Fin[2]
  
  Dev.X0  <- Fig.X0 - Fig[1] * Din[1] * dX   
  Dev.X1  <- Dev.X0 + Din[1] * dX 
  
  Dev.Y0  <- Fig.Y0 - Fig[3] * Din[2] * dY   
  Dev.Y1  <- Dev.Y0 + Din[2] * dY 
  
  Dev     <-  c(Dev.X0, Dev.X1, Dev.Y0, Dev.Y1)
  
  # Third obejective is to find the coordinates of the full page
  
  Full.X0  <-  Dev.X0 - Omi[2] * dX
  Full.X1  <-  Dev.X1 + Omi[4] * dX
  Full.Y0  <-  Dev.Y0 - Omi[1] * dY
  Full.Y1  <-  Dev.Y1 + Omi[3] * dY
  
  Full     <-  c(Full.X0, Full.X1, Full.Y0, Full.Y1)
  
  
  # Calculate lengths
  
  Full.dX      <- Full.X1 - Full.X0 
  Full.dY      <- Full.Y1 - Full.Y0 
  Fig.dX       <- Fig.X1  - Fig.X0 
  Fig.dY       <- Fig.Y1  - Fig.Y0 
  Dev.dX       <- Dev.X1  - Dev.X0 
  Dev.dY       <- Dev.Y1  - Dev.Y0 
  
  dX.dInch     <-  dX
  dY.dInch     <-  dY
  
  
  Output   <-  list("Figure"   = Figure,
                    "Dev"      = Dev,
                    "Full"     = Full,
                    "Fig.X0"   = Fig.X0,
                    "Fig.X1"   = Fig.X1,
                    "Fig.Y0"   = Fig.Y0,
                    "Fig.Y1"   = Fig.Y1,
                    "Dev.X0"   = Dev.X0,
                    "Dev.X1"   = Dev.X1,
                    "Dev.Y0"   = Dev.Y0,
                    "Dev.Y1"   = Dev.Y1,
                    "Full.X0"  = Full.X0,
                    "Full.X1"  = Full.X1,
                    "Full.Y0"  = Full.Y0,
                    "Full.Y1"  = Full.Y1,
                    "Full.dX"  = Full.dX,
                    "Full.dY"  = Full.dY,
                    "Fig.dX"   = Fig.dX,
                    "Fig.dY"   = Fig.dY,
                    "Dev.dX"   = Dev.dX,
                    "Dev.dY"   = Dev.dY,
                    "dX.dInch" = dX.dInch,
                    "dY.dInch" = dY.dInch)
  
  return(Output)
}




#polygon(c(Usr[1],Usr[1], Usr[2], Usr[2]), c(Usr[3],Usr[4], Usr[4], Usr[3]), col = "red")

#polygon(c(Dev.X0, Dev.X0, Dev.X1 , Dev.X1), c(Dev.Y0, Dev.Y1, , Dev.Y0), col = "red", lwd=5)
#polygon(c(Full.X0, Full.X0, Full.X1 , Full.X1), c(Full.Y0, Full.Y1, Full.Y1, Full.Y0), col = "green", lwd=5)


#points(Dev.X0, Dev.Y0, pch="*", cex = 21, col = "green")
#points(Dev.X0, Dev.Y1, pch="*", cex = 21, col = "green")
#points(Dev.X1, Dev.Y1, pch="*", cex = 21, col = "green")
#points(Dev.X1, Dev.Y0, pch="*", cex = 21, col = "green")

#points(Fig.X0, Fig.Y0, pch="*", cex = 11, col = "red")
#points(Fig.X0, Fig.Y1, pch="*", cex = 11, col = "red")
#points(Fig.X1, Fig.Y1, pch="*", cex = 11, col = "red")
#points(Fig.X1, Fig.Y0, pch="*", cex = 11, col = "red")

#points(Full.X0, Full.Y0, pch="*", cex = 21, col = "blue")
#points(Full.X0, Full.Y1, pch="*", cex = 21, col = "blue")
#points(Full.X1, Full.Y1, pch="*", cex = 21, col = "blue")
#points(Full.X1, Full.Y0, pch="*", cex = 21, col = "blue")

#polygon(c(Regions$Dev.X0, Regions$Dev.X0, Regions$Dev.X1 , Regions$Dev.X1), 
#        c(Regions$Dev.Y0, Regions$Dev.Y1, Regions$Dev.Y1, Regions$Dev.Y0), 
#        col = "yellow", lwd=5)





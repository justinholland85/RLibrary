#====================================================================================================#
# Lib.ColourScheme.AlphaMod   
#====================================================================================================#

Lib.ColourScheme.AlphaMod    <-  function(Base, Alpha){
  
  RGB        <-   data.frame(t(col2rgb(Base)) / 255)
  
  RGB$alpha  <-  Alpha
  
  New         <-  do.call(rgb, RGB)
  
  return(New)
  
}


#====================================================================================================#
# Lib.ColourScheme.AlphaGet
#====================================================================================================#

Lib.ColourScheme.AlphaGet   <-  function(Base){
  
  alpha_hex <- substr(Base, 8, 9)
  alpha     <- strtoi(alpha_hex, 16L) / 255  # convert hex → decimal → 0–1 scale
  
  return(alpha)   
  
}

#====================================================================================================#
# Lib.ColourScheme.VMod
#====================================================================================================#

Lib.ColourScheme.VMod  <-  function(Base, Delta){
  
  
  RGB            <-  data.frame(t(col2rgb(Base)))
  RGB$alpha      <-  Lib.ColourScheme.AlphaGet(Base)
  
  HSV            <-  data.frame(t(rgb2hsv(r = RGB$red, g = RGB$green, b = RGB$blue )))
  HSV$alpha      <-  RGB$alpha
  rownames(HSV)  <-  rownames(RGB)
  
  HSV$v          <-  pmin(pmax(HSV$v + Delta, 0), 1)
  
  Out            <-  do.call(hsv, HSV)
  
  return(Out)
  
}

#====================================================================================================#
# Lib.ColourScheme.SMod
#====================================================================================================#

Lib.ColourScheme.SMod  <-  function(Base, Delta){
  
  
  RGB            <-  data.frame(t(col2rgb(Base)))
  RGB$alpha      <-  Lib.ColourScheme.AlphaGet(Base)
  
  HSV            <-  data.frame(t(rgb2hsv(r = RGB$red, g = RGB$green, b = RGB$blue )))
  HSV$alpha      <-  RGB$alpha
  rownames(HSV)  <-  rownames(RGB)
  
  HSV$s          <-  pmin(pmax(HSV$s + Delta, 0), 1)
  
  Out            <-  do.call(hsv, HSV)
  
  return(Out)
  
}

#====================================================================================================#
# Lib.ColourScheme.HRot
#====================================================================================================#

Lib.ColourScheme.HRot  <-  function(Base, Delta){
  
  
  RGB            <-  data.frame(t(col2rgb(Base)))
  RGB$alpha      <-  Lib.ColourScheme.AlphaGet(Base)
  
  HSV            <-  data.frame(t(rgb2hsv(r = RGB$red, g = RGB$green, b = RGB$blue )))
  HSV$alpha      <-  RGB$alpha
  rownames(HSV)  <-  rownames(RGB)
  
  HSV$h          <-  (HSV$h + Delta) %% 1 
  
  Out            <-  do.call(hsv, HSV)
  
  return(Out)
  
}

#====================================================================================================#
# Lib.ColourScheme.Values   
#====================================================================================================#

Lib.ColourScheme.Values  <-  function(Base){
  
  RGB            <-  data.frame(t(col2rgb(Base)))
  names(RGB)     <-  c("r", "g", "b")
  
  alpha         <-  Lib.ColourScheme.AlphaGet(Base)
  
  HSV            <-  data.frame(t(rgb2hsv(r = RGB$r, g = RGB$g, b = RGB$b )))
  
  DF             <-  data.frame("Colour" = Base, 
                                RGB, 
                                HSV, 
                                alpha = alpha)
  
  return(DF)
  
}



#====================================================================================================#
# Lib.Plot.Colours   
#====================================================================================================#

Lib.Plot.Colours   <- function(Colours,
                               Plot.New = TRUE,
                               X0 = 0, X1 = 1, Y0 = 0, Y1 = 1){
  
  
  if(Plot.New == 1){Lib.Plot.Blank(X0 = X0, X1 = X1, Y0 = Y0, Y1 = Y1)}
  
  N.y      <-  nrow(Colours)
  N.x      <-  ncol(Colours)
  
  Grid      <-  Lib.Grid.Coords(N.x, N.y, X0 = X0, X1 = X1, Y0 = Y0, Y1 = Y1)
  
  
  for(i in 1:N.y){
    for(j in 1:N.x){
      
      
      x0    <-  Grid$X[j]
      x1    <-  Grid$X[j+1]
      y0    <-  Grid$Y[i+1]
      y1    <-  Grid$Y[i]
      
      
      polygon(x = c(x0, x0, x1, x1), 
              y = c(y0, y1, y1, y0), col = Colours[i,j])
      
    } }
  
}

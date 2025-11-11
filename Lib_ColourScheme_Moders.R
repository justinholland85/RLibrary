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
  
  return(Alpha)   
  
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

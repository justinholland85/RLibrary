#Tab.Text   <-  Text
#Y0        <-  Y.Lim[2]
#X0        <-  X.Lim[2]

#dY         <-  .05 * Y.Lim[2]
#dX         <-  .1 * (X.Lim[2] - X.Lim[1])      

Lib.TextTable    <-  function(Tab.Text, Tab.Colour, Tab.Font, Tab.Cex, 
                              Tab.Pos, Y0, dY, X0, dX ){
  
  Dim    <-  dim(Tab.Text)
  
  if(missing(Tab.Colour)){Tab.Colour        <-  array(dim = Dim, "black")}
  if(missing(Tab.Font)){Tab.Font            <-  array(dim = Dim, 1)}
  if(missing(Tab.Cex)){Tab.Cex              <-  array(dim = Dim, .6)}
  if(missing(Tab.Pos)){Tab.Pos              <-  array(dim = Dim, 4)}
  if(length(dX) == 1){dX                    <-  rep(dX, Dim[2])}
  
  if(length(Tab.Font) == 1){Tab.Font        <-  array(dim = Dim, Tab.Font)}
  if(length(Tab.Cex) == 1){Tab.Cex          <-  array(dim = Dim, Tab.Cex)}
  if(length(Tab.Colour) == 1){Tab.Colour    <-  array(dim = Dim, Tab.Colour)}
  if(length(Tab.Pos) == 1){Tab.Pos          <-  array(dim = Dim, Tab.Pos)}
  
  if(length(Tab.Pos) == Dim[2]){Tab.Pos     <-  t(array(dim = rev(Dim), Tab.Pos))}
  
  
  Y     <-  array(dim = Dim, Y0  - dY * seq(0, Dim[1] -1 ))
  X     <-  t(array(dim = rev(Dim), c(X0, X0 + cumsum(dX)[-length(dX)])))
  
  for(i in 1:Dim[1]){
  for(j in 1:Dim[2]){

    text(x = X[i,j], y = Y[i,j], labels = Tab.Text[i,j], font = Tab.Font[i,j], pos = Tab.Pos, 
         col = Tab.Colour[i,j],
         cex = Tab.Cex[i,j] )
    
      
  }}
  
  
}
  
  
  
  
  

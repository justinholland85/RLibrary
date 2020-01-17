Lib.PrintList   <-  function(List, 
                             X, 
                             Y0, 
                             Y1, 
                             Plot,
                             Col.Text,
                             Col.Names,
                             Col.Header,
                             Size.Text,
                             Size.Names,
                             Size.Header,
                             Names,
                             Header,
                             Header.Marge,
                             dX){

  
  if(missing(Header)){      Header         <-  ""}
  if(missing(Header.Marge)){Header.Marge  <-  .1}
  if(missing(Names)){       Names         <-  names(List)}
  if(missing(X)){           X             <-  .1}
  if(missing(Y0)){          Y0            <-  0}
  if(missing(Y1)){          Y1            <-  1}
  if(missing(Col.Text)){    Col.Text      <-  "black"}  
  if(missing(Col.Names)){   Col.Names     <-  "black"}  
  if(missing(Col.Header)){  Col.Header    <-  "black"}  
  if(missing(Size.Text)){   Size.Text     <-  1}  
  if(missing(Size.Names)){  Size.Names    <-  1} 
  if(missing(Size.Header)){ Size.Header   <-  1} 
  if(missing(Plot)){        Plot          <-  1}  
  if(missing(dX)){          dX            <-  0}  
  
  N.Y   <-  length(List)
  
  Y.Breaks  <-  rev(seq(0,1,1/N.Y))
  
  if(Plot == 1){
    plot(c(0,1), c(0,1), type='n', bty='n',xlab="",ylab="",xaxt='n' ,yaxt='n')
  }
  
  dY  <-  Y1 - Y0
  
  for(i in 1:N.Y){
    
    Y   <-  Y0 + dY * (1 - Header.Marge) * Y.Breaks[i]
    text(X, Y, Names[i], col=Col.Names, cex=Size.Names, font= 2, pos = 2)
    
    Text   <-  paste(List[[i]], collapse = ",")
    text(X + dX, Y, Text, col=Col.Text, cex=Size.Text, font= 2, pos = 4)
    
  }
  
  
  #--- Header
  text(X + dX, Y1, Header, col=Col.Text, cex=Size.Header, font= 2, pos = 4)
  
}

######################################################################################################

Lib.PrintList2   <-  function(List.A,
                              List.B,
                              X.A,
                              X.B,
                              Y0, 
                              Y1, 
                              Plot,
                              Col.Text.A,
                              Col.Text.B,
                              Col.Names,
                              Size.Text,
                              Size.Names,
                              Names,
                              Header.A,
                              Header.B,
                              Header.Marge,
                              Col.Header,
                              Size.Header,
                              dX,
                              Diff.Border,
                              Diff.Fill,
                              Diff.Lwd,
                              Diff.X0,
                              Diff.X1){
  
  
  if(missing(Header.A)){    Header.A     <-  ""}
  if(missing(Header.B)){    Header.B      <-  ""}
  if(missing(Header.Marge)){Header.Marge  <-  .1}
  if(missing(Names)){       Names         <-  names(List.A)}
  if(missing(X.A)){         X.A           <-  .1}
  if(missing(X.B)){         X.B           <-  .2}
  if(missing(Y0)){          Y0            <-  0}
  if(missing(Y1)){          Y1            <-  1}
  if(missing(Col.Text.A)){  Col.Text.A    <-  "black"}  
  if(missing(Col.Text.B)){  Col.Text.B    <-  "black"}  
  if(missing(Col.Names)){   Col.Names     <-  "black"}  
  if(missing(Col.Header)){  Col.Header    <-  "black"}  
  if(missing(Size.Text)){   Size.Text     <-  1}  
  if(missing(Size.Names)){  Size.Names    <-  1} 
  if(missing(Size.Header)){ Size.Header   <-  1} 
  if(missing(Plot)){        Plot          <-  1}  
  if(missing(dX)){          dX            <-  0}  
  if(missing(Diff.Border)){ Diff.Border   <-  "black"}
  if(missing(Diff.Fill)){   Diff.Fill <-  "transparent"}
  if(missing(Diff.Lwd)){    Diff.Lwd         <-  1}
  
  if(missing(Diff.X0)){        Diff.X0         <-  X.A - (X.B - X.A)  }
  if(missing(Diff.X1)){        Diff.X1         <-  X.B + (X.B - X.A)  }
  
  
  N.Y   <-  length(List.A)
  
  Y.Breaks  <-  rev(seq(0,1,1/N.Y))
  
  if(Plot == 1){
    plot(c(0,1), c(0,1), type='n', bty='n',xlab="",ylab="",xaxt='n' ,yaxt='n')
  }
  
  
  Different   <-  !mapply("identical", List.A, List.B)
  
  dY  <-  Y1 - Y0
  Poly.dY  <-   dY / (2 * N.Y) 
  Poly.Y.Shift  <-  .0025
  
  for(i in 1:N.Y){
    
    Y        <-  Y0 + dY * (1 - Header.Marge) * Y.Breaks[i]

    
    
    if(Different[i] == 1){
      
      Poly.X  <-  c(Diff.X0, Diff.X0, Diff.X1, Diff.X1)
      Poly.Y  <-  Y + c(-Poly.dY+Poly.Y.Shift, Poly.dY, Poly.dY, -Poly.dY+Poly.Y.Shift)
      polygon(Poly.X, Poly.Y, col=Diff.Fill, border = Diff.Border, lwd = Diff.Lwd)
      
    }
    
    Y   <-  Y0 + dY * (1 - Header.Marge) * Y.Breaks[i]
    text(X.A, Y, Names[i], col=Col.Names, cex=Size.Names, font= 2, pos = 2)
    
    Text   <-  paste(List.A[[i]], collapse = ",")
    text(X.A + dX, Y, Text, col=Col.Text.A, cex=Size.Text, font= 2, pos = 4)
    
    Text   <-  paste(List.B[[i]], collapse = ",")
    text(X.B , Y, Text, col=Col.Text.B, cex=Size.Text, font= 2, pos = 4)
    
  
    
  }
  
  
  #--- Header
  text(X.A + dX, Y1, Header.A, col=Col.Text.A, cex=Size.Header, font= 2, pos = 4)
  text(X.B     , Y1, Header.B, col=Col.Text.B, cex=Size.Header, font= 2, pos = 4)
  
}



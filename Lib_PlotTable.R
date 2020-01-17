Lib.PlotTable  <-  function(Table,
                            X0,
                            X1,
                            Y0,
                            Y1,
                            RowMarge,
                            ColMarge,
                            RowNames,
                            ColNames,
                            Col.BG,
                            Col.Text,
                            Col.Grid,
                            Size.Text,
                            Pch,
                            Pch.Col,
                            Pch.Border,
                            Pch.Size,
                            Col.BG.Rows,
                            Col.Text.Rows,
                            Col.Grid.Rows,
                            Size.Text.Rows,
                            Col.BG.Cols,
                            Col.Text.Cols,
                            Theta.Text.Cols,
                            Col.Grid.Cols,
                            Size.Text.Cols,
                            Plot,
                            Plot.Pch,
                            Title.Text,
                            Title.Col,
                            Title.BG,
                            Title.Border,
                            Title.Size,
                            Plot.Title,
                            Plot.Head.Cols,
                            Head.Prop.Cols,
                            Head.TextCol.Cols,
                            Head.Size.Cols,
                            Head.Border.Cols,
                            Head.BG.Cols,
                            Head.Label.Cols,
                            Plot.Head.Rows,
                            Head.Prop.Rows,
                            Head.TextCol.Rows,
                            Head.Size.Rows,
                            Head.Border.Rows,
                            Head.BG.Rows,
                            Head.Label.Rows,
                            Text.Pos,
                            Text.Pos.Method,
                            Text.Pos.Rows){
  
  
  Dim  <-  dim(Table)
  N.Col  <-  ncol(Table)
  N.Row  <-  nrow(Table)
  
  #### Defaults
  
  if(missing(X0)){  X0  <-  0}
  if(missing(X1)){  X1  <-  1}
  if(missing(Y0)){  Y0  <-  1}
  if(missing(Y1)){  Y1  <-  0}
  
  if(missing(RowMarge)){ RowMarge   <-  .1}
  if(missing(ColMarge)){ ColMarge   <-  .1}
  
  if(missing(Plot)){Plot  <- 1}
  
  if(missing(RowNames)){  RowNames   <-  rownames(Table)}
  if(missing(ColNames)){  ColNames   <-  colnames(Table)}
  
  if(missing(Col.BG)){    Col.BG     <-  array(dim=Dim, "white")}
  if(missing(Col.Text)){  Col.Text   <-  array(dim=Dim, "black")}
  if(missing(Col.Grid)){  Col.Grid   <-  array(dim=Dim, "black")}
  if(missing(Size.Text)){ Size.Text  <-  array(dim=Dim, 1)}
  
  if(!identical(dim(Size.Text), Dim)){ Size.Text        <-  array(dim=Dim, Size.Text[1])}
  if(!identical(dim(Col.BG), Dim)){ Col.BG        <-  array(dim=Dim, Col.BG[1])}
  if(!identical(dim(Col.Text), Dim)){ Col.Text    <-  array(dim=Dim, Col.Text[1])}
  if(!identical(dim(Col.Grid), Dim)){ Col.Grid    <-  array(dim=Dim, Col.Grid[1])}
 
  if(missing(Plot.Pch)){  Plot.Pch   <-  0}
  if(missing(Pch)){       Pch        <-  array(dim=Dim, 21)}
  if(missing(Pch.Col)){   Pch.Col    <-  array(dim=Dim, "white")}
  if(missing(Pch.Border)){Pch.Border <-  array(dim=Dim, "black")}
  if(missing(Pch.Size)){  Pch.Size   <-  array(dim=Dim, 1)}
  
  if(!identical(dim(Pch), Dim)){ Pch  <-  array(dim=Dim, Pch[1])}
  if(!identical(dim(Pch.Col), Dim)){ Pch.Col  <-  array(dim=Dim, Pch.Col[1])}
  if(!identical(dim(Pch.Border), Dim)){ Pch.Border  <-  array(dim=Dim, Pch.Border[1])}
  if(!identical(dim(Pch.Size), Dim)){ Pch.Size  <-  array(dim=Dim, Pch.Size[1])}

  if(missing(Col.BG.Rows)){   Col.BG.Rows      <-  array(dim=N.Row, "black")}
  if(missing(Col.Text.Rows)){ Col.Text.Rows    <-  array(dim=N.Row, "white")}
  if(missing(Col.Grid.Rows)){ Col.Grid.Rows    <-  array(dim=N.Row, "white")}
  if(missing(Size.Text.Rows)){Size.Text.Rows   <-  array(dim=N.Row, 1)}
  if(missing(Theta.Text.Cols)){Theta.Text.Cols   <-  0}
  
  
  
  if(!identical(length(Col.BG.Rows), N.Row)){ Col.BG.Rows  <-  array(dim=N.Row, Col.BG.Rows[1])}
  if(!identical(length(Col.Text.Rows), N.Row)){ Col.Text.Rows  <-  array(dim=N.Row, Col.Text.Rows[1])}
  if(!identical(length(Col.Grid.Rows), N.Row)){ Col.Grid.Rows  <-  array(dim=N.Row, Col.Grid.Rows[1])}
  if(!identical(length(Size.Text.Rows), N.Row)){ Size.Text.Rows  <-  array(dim=N.Row, Size.Text.Rows[1])}
  
  if(missing(Col.BG.Cols)){  Col.BG.Cols   <-  array(dim=length(ColNames), "black")}
  if(missing(Col.Text.Cols)){Col.Text.Cols <-  array(dim=length(ColNames), "white")}
  if(missing(Col.Grid.Cols)){Col.Grid.Cols <-  array(dim=length(ColNames), "white")}
  if(missing(Size.Text.Cols)){Size.Text.Cols  <-  array(dim=length(ColNames),1)}
  
  if(!identical(length(Col.BG.Cols), N.Col)){ Col.BG.Cols  <-  array(dim=N.Col, Col.BG.Cols[1])}
  if(!identical(length(Col.Text.Cols), N.Col)){ Col.Text.Cols  <-  array(dim=N.Col, Col.Text.Cols[1])}
  if(!identical(length(Col.Grid.Cols), N.Col)){ Col.Grid.Cols  <-  array(dim=N.Col, Col.Grid.Cols[1])}
  if(!identical(length(Size.Text.Cols), N.Col)){ Size.Text.Cols  <-  array(dim=N.Col, Size.Text.Cols[1])}

  if(missing(Plot.Title)){Plot.Title = 1}
  if(missing(Title.Text)){Title.Text = ""}
  if(missing(Title.Col)){Title.Col = "white"}
  if(missing(Title.Border)){Title.Border = "transparent"}
  if(missing(Title.Size)){Title.Size = 1}
  if(missing(Title.BG)){Title.BG = "black"}

  # Heads

  if(missing(Plot.Head.Cols)){Plot.Head.Cols <- 0}
  if(missing(Head.Prop.Cols)){Head.Prop.Cols <- 0}
  if(missing(Head.TextCol.Cols)){Head.TextCol.Cols <- "white"}
  if(missing(Head.Size.Cols)){Head.Size.Cols <- 1}
  if(missing(Head.Border.Cols)){Head.Border.Cols  <-  "white"}
  if(missing(Head.BG.Cols)){Head.BG.Cols  <-  "black"}
  if(missing(Head.Label.Cols)){Head.Label.Cols  <-  ""}
  
  if(missing(Plot.Head.Rows)){Plot.Head.Rows <- 0}
  if(missing(Head.Prop.Rows)){Head.Prop.Rows <- 0}
  if(missing(Head.TextCol.Rows)){Head.TextCol.Rows <- "white"}
  if(missing(Head.Size.Rows)){Head.Size.Rows <- 1}
  if(missing(Head.Border.Rows)){Head.Border.Rows  <-  "white"}
  if(missing(Head.BG.Rows)){Head.BG.Rows  <-  "black"}
  if(missing(Head.Label.Rows)){Head.Label.Rows  <-  ""}
  
  if(missing(Text.Pos)){Text.Pos  <- NULL }
  if(missing(Text.Pos.Method)){Text.Pos.Method  <- "C" }
  if(missing(Text.Pos.Rows)){Text.Pos.Rows  <- NULL }
  
  

  #### Key calculated pars
  
  dX   <-  X1 - X0
  dY   <-  Y1 - Y0
  

  #### Find grid coords
  
  T.X    <-  seq(X0 + RowMarge * dX, X1, dX * (1 - RowMarge) / N.Col)
  T.Y    <-  seq(Y0 + ColMarge * dY, Y1, dY * (1 - ColMarge) / N.Row)
  
  T.X0   <-  t(array(dim=dim(t(Table)), T.X[1:N.Col]))
  T.X1   <-  t(array(dim=dim(t(Table)), T.X[2:(N.Col + 1)]))
  
  T.Y0   <-  array(dim=dim(Table), T.Y[1:N.Row])
  T.Y1   <-  array(dim=dim(Table), T.Y[2:(N.Row + 1)])
  
  C.X0   <-  T.X[1:N.Col]
  C.X1   <-  T.X[2:(N.Col + 1)]
  
  C.Y0   <-  rep(Y0 + ColMarge * dY * Head.Prop.Cols, N.Col)
  C.Y1   <-  rep(Y0 + ColMarge * dY, N.Col)
  
  R.X0   <-  rep(X0 + RowMarge * dX * Head.Prop.Rows, N.Row)
  R.X1   <-  rep(X0 + RowMarge * dX, N.Row)
  
  R.Y0   <-  T.Y[1:N.Row]
  R.Y1   <-  T.Y[2:(N.Row + 1)]
  
  H.C.X0 <-  C.X0[1]
  H.C.X1 <-  C.X1[N.Col]
 
  H.C.Y0 <-  Y0
  H.C.Y1 <-  Y0 + ColMarge * dY * Head.Prop.Cols
  
  H.R.X0 <-  X0
  H.R.X1 <-  X0 + RowMarge * dX * Head.Prop.Rows
  
  H.R.Y0 <-  R.Y0[1]
  H.R.Y1 <-  R.Y1[N.Row]
  
  B.X0   <-  X0
  B.X1   <-  X0 + RowMarge * dX 
  
  B.Y0   <-  Y0
  B.Y1   <-  Y0 + ColMarge * dY
  
  
  #### Base plot (if required)
  
  if(Plot == 1){
  plot(c(0,1), c(0,1), type='n', xaxt='n', yaxt='n', xlab="", ylab="", main="", bty="n")
  }
  
  #### Plot the polygons
  
  for(i in 1:N.Row){
    for(j in 1:N.Col){
      
      polygon(c(T.X0[i,j], T.X1[i,j], T.X1[i,j], T.X0[i,j]),
              c(T.Y0[i,j], T.Y0[i,j], T.Y1[i,j], T.Y1[i,j]),
              col=Col.BG[i,j], border= Col.Grid[i,j])
    }
  }
  
  for(i in 1:N.Row){
    polygon(c(R.X0[i], R.X1[i], R.X1[i], R.X0[i]),
            c(R.Y0[i], R.Y0[i], R.Y1[i], R.Y1[i]),
            col=Col.BG.Rows[i], border= Col.Grid.Rows[i])
  }
  
  for(j in 1:N.Col){
    polygon(c(C.X0[j], C.X1[j], C.X1[j], C.X0[j]),
            c(C.Y0[j], C.Y0[j], C.Y1[j], C.Y1[j]),
            col=Col.BG.Cols[j], border= Col.Grid.Cols[j])
  }
  
  #### Draw the pch 
  
  if(Plot.Pch == 1){
  
  for(i in 1:N.Row){
    for(j in 1:N.Col){
      
      points(mean(c(T.X0[i,j], T.X1[i,j])),
             mean(c(T.Y0[i,j], T.Y1[i,j])),
             pch  =  Pch[i,j],
             cex = Pch.Size[i,j], bg= Pch.Col[i,j], col=Pch.Border)
    }
  }
  }
  
  #### Write the text 
  
  for(i in 1:N.Row){
    for(j in 1:N.Col){
      
      if(Text.Pos.Method == "C"){ X.Pos  <-  mean(c(T.X0[i,j], T.X1[i,j]))}
      if(Text.Pos.Method == "L"){ X.Pos  <-  T.X0[i,j]   }              
        
      text(X.Pos,
           mean(c(T.Y0[i,j], T.Y1[i,j])),
           labels  <-  Table[i,j],
           cex=Size.Text[i,j], col= Col.Text[i,j], pos= Text.Pos)
    }
  }
  
  for(i in 1:N.Row){
    
    if(is.null(Text.Pos.Rows)){ X.Pos  <-  mean(c(R.X0[i], R.X1[i]))} else {
    X.Pos  <-  R.X0[i]   }      
    
    text(X.Pos,
         mean(c(R.Y0[i], R.Y1[i])),
         labels  <-  RowNames[i],
         cex=Size.Text.Rows[i], col= Col.Text.Rows[i], pos=Text.Pos.Rows)
  }
  
  for(j in 1:N.Col){
    text(mean(c(C.X0[j], C.X1[j])),
         mean(c(C.Y0[j], C.Y1[j])),
         labels  <-  ColNames[j],
         cex=Size.Text.Cols[j], col= Col.Text.Cols[j], srt=Theta.Text.Cols)
  }
  
  
  #### Title 
  
  if(Plot.Title == 1){
    
    polygon(c(B.X0, B.X1, B.X1, B.X0),
            c(B.Y0, B.Y0,  B.Y1, B.Y1),
            col = Title.BG,
            border = Title.Border)
   
    
    text(mean(c(B.X0, B.X1)),
         mean(c(B.Y0, B.Y1)),
         labels = Title.Text, col=Title.Col, cex=Title.Size) 
    }
  
  #### Headers
  
  if(Plot.Head.Cols){
    
    
    polygon(c(H.C.X0, H.C.X1, H.C.X1, H.C.X0),
            c(H.C.Y0, H.C.Y0,  H.C.Y1, H.C.Y1),
            col = Head.BG.Cols,
            border = Head.Border.Cols)
    
    text(mean(c(H.C.X0, H.C.X1)),
         mean(c(H.C.Y0, H.C.Y1)),
         labels = Head.Label.Cols, col=Head.TextCol.Cols, cex=Head.Size.Cols) 
    
    
  }
  
  
  if(Plot.Head.Rows){
    
    polygon(c(H.R.X0, H.R.X1, H.R.X1, H.R.X0),
            c(H.R.Y0, H.R.Y0,  H.R.Y1, H.R.Y1),
            col = Head.BG.Cols,
            border = Head.Border.Cols)
    
    text(mean(c(H.R.X0, H.R.X1)),
         mean(c(H.R.Y0, H.R.Y1)),
         labels = Head.Label.Rows, col=Head.TextCol.Rows, cex=Head.Size.Rows, srt=90) 
    
    
  }
  
  
  
}
  
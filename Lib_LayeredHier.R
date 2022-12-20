
#Eps  <-  0
#Min.Plot <-  0.001
#Min.Text <-  0.005
#Text.Cex <-  1
#Text.Cex  <-  c(Text.Cex,rep(Text.Cex[length(Text.Cex)],N + 2 - length(Text.Cex)))
#BG	<-	"gray21"
#Ref  <-  sort(unique(c(Cats)))
#Colours  <-  Lib.ColourScheme(length(Ref),4, V=1, Plot = 0)[3,]
#BorderCols  <-  rep("black", length(Ref))
#FlipAx  <-  0
#Main  <-  ""
#Plot.New  <-  1
#Xmin  <-  0
#Xmax  <-  1
#Ymin  <-  0
#Ymax  <-  1
#Text.FirstOnly  <-  1









Lib.LayeredHier  <-  function(Cats, Values, Ref, Colours, BorderCols,
					Min.Plot, Min.Text, Text.Cex, BG, Eps,
					FlipAx, Main, Plot.New, Xmin, Xmax, Ymin, Ymax, Text.FirstOnly){

# Cats is a text matrix, columns are levels
# Values is a vector of case weights equivalent length to rows in Cats
# Ref is a text vector linking category labels to colours and should match
#	length of Colours and Border Colours


N  <-  ncol(Cats)
Ypoints  <-  seq(0,1,1/N)
Eps.Text  <-  .1 * 1/N

if(missing(Eps)){Eps  <-  0}
if(missing(Min.Plot)){Min.Plot <-  0.001}
if(missing(Min.Text)){Min.Text <-  0.005}
if(missing(Text.Cex)){Text.Cex <-  1}
if(length(Text.Cex) < N + 1){Text.Cex  <-  c(Text.Cex,rep(Text.Cex[length(Text.Cex)],N + 2 - length(Text.Cex)))}
if(missing(BG)){BG	<-	"gray21"}
if(missing(Ref)){Ref  <-  sort(unique(c(Cats)))}
if(missing(Colours)){ Colours  <-  Lib.ColourScheme(length(Ref),4, V=1, Plot = 0)[3,] }
if(missing(BorderCols)){ BorderCols  <-  rep("black", length(Ref)) }
if(missing(FlipAx)){ FlipAx  <-  0}
if(missing(Main)){ Main  <-  ""}
if(missing(Plot.New)){ Plot.New  <-  1}
if(missing(Xmin)){ Xmin  <-  0}
if(missing(Xmax)){ Xmax  <-  1}
if(missing(Ymin)){ Ymin  <-  0}
if(missing(Ymax)){ Ymax  <-  1}
if(missing(Text.FirstOnly)){ Text.FirstOnly  <-  1}



BG0  <-  par()$bg

par(bg=BG)

Divide  <- 1000
ID   <-  seq(1,nrow(Cats))

# open plot window
if(Plot.New==1){
plot(c(0,1), c(0,1), type='n', xaxt='n', yaxt='n', xlab="", ylab="", bty='n', main = Main)
}

# Create shells for plot data
Plot.Xpoints  	<-  list()
Plot.Colours  	<-  list()
Plot.BorderCol  	<-  list()
Plot.Text  		<-  list()

# Initialise at base level

Tapply  		<-  tapply(Values,as.character(Cats[,1]),sum)
Group 		<-  match(as.character(Cats[,1]), names(Tapply))
Group.Order 	<-  match(as.character(Cats[,1]),as.character(Ref))
Group.String	<-  as.character(Cats[,1])
Tapply.Order	<-  order(Group.Order[match(names(Tapply),Group.String)])
Tapply		<-  Tapply[Tapply.Order]

Proportions  	<-  Tapply / sum(Tapply)
X  			<-  cumsum(Proportions)
Plot.Xpoints[[1]] <-  c(0,X)


Match.Colour 	 	<-  match(names(Tapply), as.character(Ref))
Plot.Colours[[1]]  	<-  Colours[Match.Colour]
Plot.BorderCol[[1]]	<-  BorderCols[Match.Colour]
Plot.Text[[1]]		<-  Ref[Match.Colour]

###### 
# Loop over higher levels

for(i in 2:N){

SubGroup.String   <-  paste(Group , Cats[,i], sep="$") 
Tapply  		<-  tapply(Values, SubGroup.String, sum)
SubGroup.Ord      <-  match(as.character(Cats[,i]),as.character(Ref))
Group.Ord.Score   <-  Group.Order + SubGroup.Ord  / Divide
Sort			<-  sort(Group.Ord.Score)	
Group.Order		<-  match(Group.Ord.Score,Sort)
Tapply.Order	<-  order(Group.Ord.Score[match(names(Tapply),SubGroup.String)])
Tapply  		<-  Tapply[Tapply.Order]

###
Proportions  	<-  Tapply / sum(Tapply)
X  			<-  cumsum(Proportions)
Plot.Xpoints[[i]] <-  c(0,X)
Group  		<-  match(SubGroup.String , names(Tapply))


Colours.Dummy  	<-  Colours[match(as.character(Cats[,i]),  as.character(Ref))]
BorderCols.Dummy  <-  BorderCols[match(as.character(Cats[,i]),  as.character(Ref))]

Match.Colour  		<-  match(names(Tapply), SubGroup.String)
Plot.Colours[[i]]  	<-  Colours.Dummy[Match.Colour]
Plot.BorderCol[[i]]  	<-  BorderCols.Dummy[Match.Colour]
Plot.Text[[i]]		<-  Cats[Match.Colour,i]

}

###### 
# Plot the boxes 


for(i in 1:N){

Temp.Xpoints  	<-  Plot.Xpoints[[i]]
Temp.Colours  	<-  Plot.Colours[[i]]
Temp.BorderCol  	<-  Plot.BorderCol[[i]]
Temp.Text 		<-  Plot.Text[[i]]

M  			<-  length(Temp.Colours)


for(j in 1:M){

X0  		<-  Temp.Xpoints[j]
X1  		<-  Temp.Xpoints[j+1] - Eps  
Y0  		<-  Ypoints[i]
Y1  		<-  Ypoints[i+1] - Eps   

X.Share  	<-  X1 - X0

# re-map X and Y into plot reduced plot window

X0      <-  Xmin + X0 * (Xmax - Xmin)
X1      <-  Xmin + X1 * (Xmax - Xmin)
Y0      <-  Ymin + Y0 * (Ymax - Ymin)
Y1      <-  Ymin + Y1 * (Ymax - Ymin)


Text.X 	<-  (X0 + X1) / 2 	
Text.Y	<-  Y0 + Eps.Text

 


Col  		<-  Temp.Colours[j]
Border  	<-  Temp.BorderCol[j]
Text		<-  Temp.Text[j]
Plot.Text.Cex 	<-  Text.Cex[i]

if(X.Share > Min.Plot){
if(FlipAx == 0){
polygon(c(X0,X0,X1,X1),c(Y0,Y1,Y1,Y0),col=Col,border=Col)
} else {
polygon(c(Y0,Y1,Y1,Y0),c(X0,X0,X1,X1),col=Col,border=Col)
}
}

if(Text.FirstOnly == 0 | i == 1){
if(X.Share > Min.Text){
if(FlipAx ==0){
text(Text.X, Text.Y, labels=Text, col= Border, cex = Plot.Text.Cex)
} else {
text(Text.Y, Text.X, labels=Text, col= Border, cex = Plot.Text.Cex)
}}}



}}

par(bg=BG0)
}








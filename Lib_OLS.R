
Lib.OLS  			<-  	function(X,y){
     
     X  			<-  	cbind(1,X) 
     Design.Mat     <-   solve(t(X) %*% X)
     Beta  		<-  	solve(t(X) %*% X) %*% t(X) %*% y
     Beta.Mat  		<-	t(array(dim=dim(t(X)),Beta))
     Est			<-	rowSums(Beta.Mat * X)
     U			<-	(y - Est)
     Mean.y		<-	mean(y)
     
     # Calculate correlation coefficient
     R2			<-	1 - sum((Est - y)^2) / sum( (y - Mean.y)^2) 
     
     # Calculate standard errors 
     N  			<-	length(U)
     Var.Hat.U		<-	sum(U^2) / (N - ncol(X)) # Almost
     Mu.X			<-	t(array( dim= dim( t(X)), apply(X, 2, mean)))
     Sum.x.2		<-	colSums((X - Mu.X)^2) 	
     SE.U   		<-	Var.Hat.U^.5
     SE             <-   (diag(Design.Mat) * Var.Hat.U) ^ .5 
     
     # Adjusted R^2
     N  			<-	nrow(X)
     P			<-	ncol(X) - 1
     Adj.R2  		<-	1 - (1 - R2) * (N - 1) / (N - P - 1)
     
     # t
     t			<-	Beta / SE 
     P.t			<-	2 * (1 - pt(abs(t), N - P - 1 ))
     
     Output		<-	list(Est, U, Beta, SE, t, P.t, SE.U, R2, Adj.R2)
     names(Output)  <-  	c("Est", "U", "Beta", "SE", "t", "P.t", "SE.U", 
                           "R2", "Adj.R2")
     return(Output)
} 

# x = (X - Mu.X)
# Beta  		<-  	Design.Mat %*% t(X) %*% y
# SE			<-	(Var.Hat.U / Sum.x.2)^.5
# Var.Hat.U		<-	sum(U^2) / (N - 2)
# N-2 in Gurjarati was for 2 DF
# SE: Gujarati, pg 72
# SE Multiple: https://stats.stackexchange.com/questions/27916/standard-errors-for-multiple-regression-coefficients
# https://en.wikipedia.org/wiki/Coefficient_of_determination
##############################################################################


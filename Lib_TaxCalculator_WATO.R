######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tax.Parameters.WATO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Years   <- 2025

Lib.Tax.Parameters.WATO   <-  function(Years,...){
  
  
  
  
  FilePath         <-  paste0(Lib.Dir, "/LibData_TaxCode.csv")
  LibData.TaxCode  <-  read.csv(FilePath, header=TRUE)
  
  if(missing(Years)){Years   <-   LibData.TaxCode$income_year}
  Years   <-  c(Years, c(...))
  
  Max         <-  LibData.TaxCode$wato_max
  
  Rates.Res   <-  cbind(0,
                        LibData.TaxCode$first_rate,
                        LibData.TaxCode$second_rate,
                        LibData.TaxCode$third_rate,
                        LibData.TaxCode$fourth_rate,
                        LibData.TaxCode$fifth_rate)
  Thold.Res   <-  cbind(0,
                        LibData.TaxCode$tax_free_threshold,
                        LibData.TaxCode$second_threshold,
                        LibData.TaxCode$third_threshold,
                        LibData.TaxCode$fourth_threshold,
                        LibData.TaxCode$fifth_threshold)
  
  rownames(Rates.Res)  <-  LibData.TaxCode$income_year
  rownames(Thold.Res)  <-  LibData.TaxCode$income_year
  
  
  
  Rates.Res[which(is.na(Rates.Res))]    <- 100
  Rates.Res  <-  Rates.Res / 100
  
  Thold.Res[which(is.na(Thold.Res))]    <- Inf
  Max[which(is.na(Max))]                <- 0
  
  Rates.Res  <-  Rates.Res[match(Years, LibData.TaxCode$income_year),]
  Thold.Res  <-  Thold.Res[match(Years, LibData.TaxCode$income_year),]
  Max        <-  Max[      match(Years, LibData.TaxCode$income_year)]
  
  Output  <-  list("Rates.Res" = Rates.Res,
                   "Thold.Res" = Thold.Res,
                   "Max"       = Max,
                   "Years"     = Years
  )
  
  
  return(Output)
  
}

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tax.WATO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################


#Max             <-  Lib.Tax.Parameters.WATO(2028)$Max
#Rates           <-  Lib.Tax.Parameters.WATO(2028)$Rates
#Thresholds      <-  Lib.Tax.Parameters.WATO(2028)$Thold
#X               <-  seq(0, 130000, 1000)


Lib.Tax.WATO         <-  function(X, Rates, Thresholds, Max){
  
  
  N             <-  length(X)
  Dim.Rates     <-  dim(Rates)
  Dim.Tholds    <-  dim(Thresholds)
  
  if(is.null(Dim.Rates)){
    
    N.Rates     <-  length(Rates)
    Rates       <-  t(array(dim=c(N.Rates, N), Rates))
    
  } else { N.Rates   <-  Dim.Rates[2]}
  
  if(is.null(Dim.Tholds)){
    
    N.Tholds    <-  length(Thresholds)
    Thresholds  <-  t(array(dim=c(N.Rates, N), Thresholds  ))
    
  } else { N.Rates   <-  Dim.Tholds[2]}
  
  
  # Initialisations:
  R.Applied     <-  0 * X
  Tax           <-  0 * X
  
  # Loop over rates and thresholds
  for(i in 1:N.Rates){
    Base        <-  pmax(X - Thresholds[,i], 0)
    AddTax      <-  Base * (Rates[,i] - R.Applied)
    Tax         <-  Tax + AddTax
    R.Applied   <-  Rates[,i]
  }
  
  Offest        <-  pmin(Tax, Max)
  
  return(Offest)
  
} 


######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.TaxCalculator.WATO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Lib.TaxCalculator.WATO  <-  function(X, Year){
  
  Parameters         <-  Lib.Tax.Parameters.WATO(Year)
  
  if(length(Year) == 1){Year <-  rep(Year, length(X))}
  if(length(X) == 1 & length(Year) > 1){X  <-  rep(X, length(Year))}
  
  MatchYear   <-  match(Year, Parameters$Years)
  
  Max         <-  Parameters$Max
  Rates       <-  Parameters$Rates
  Tholds      <-  Parameters$Thold
  
  if(is.null(dim(Rates))){Rates    <-  rbind(Rates)}
  if(is.null(dim(Tholds))){Tholds  <-  rbind(Tholds)}
  
  Max          <-   Max[MatchYear] 
  Rates        <-   Rates[MatchYear,]
  Tholds       <-   Tholds[MatchYear,]
  
  Offset       <-  as.numeric(Lib.Tax.WATO(X, Rates, Tholds, Max))
  
  return(Offset)
  
  
}











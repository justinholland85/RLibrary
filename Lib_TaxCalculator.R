

Lib.Tax.Parameters   <-  function(Years,...){
  
  
  FilePath         <-  paste0(Lib.Dir, "/LibData_TaxCode.csv")
  LibData.TaxCode  <-  read.csv(FilePath, header=TRUE)

  if(missing(Years)){Years   <-   LibData.TaxCode$income_year}
  Years   <-  c(Years, c(...))
  
  
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
  
  
  
  Rates.Res[which(is.na(Rates.Res))]    <-  100
  Rates.Res  <-  Rates.Res / 100
  
  Thold.Res[which(is.na(Thold.Res))]    <-  Inf
  
  
  Rates.Res  <-  Rates.Res[match(Years, LibData.TaxCode$income_year),]
  Thold.Res  <-  Thold.Res[match(Years, LibData.TaxCode$income_year),]
  
  
  Output  <-  list("Rates.Res" = Rates.Res,
                   "Thold.Res" = Thold.Res,
                   "Years"     = Years
                   )
  
  
  return(Output)
  
}


######################################################################################################

Lib.Tax         <-  function(X, Rates, Thresholds){
  
  
  N             <-  length(X)
  Dim.Rates     <-  dim(Rates)
  Dim.Tholds    <-  dim(Thresholds)
  
  if(is.null(Dim.Rates)){
    
    N.Rates   <-  length(Rates)
    Rates     <-  t(array(dim=c(N.Rates, N), Rates))
    
  } else { N.Rates   <-  Dim.Rates[2]}
  
  if(is.null(Dim.Tholds)){
    
    N.Tholds    <-  length(Thresholds)
    Thresholds  <-  t(array(dim=c(N.Rates, N), Thresholds  ))
    
  } else { N.Rates   <-  Dim.Tholds[2]}
  
  
  # Initialisations:
  R.Applied    <-  0 * X
  Tax          <-  0 * X
  
  # Loop over rates and thresholds
  for(i in 1:N.Rates){
    Base       <-  pmax(X - Thresholds[,i], 0)
    AddTax     <-  Base * (Rates[,i] - R.Applied)
    Tax        <-  Tax + AddTax
    R.Applied  <-  Rates[,i]
  }
  
  return(Tax)
  
} 

######################################################################################################

Lib.Tax.Basic  <-  function(X, Year){
  
  
  Parameters         <-  Lib.Tax.Parameters(Year)

  
  if(length(Year) == 1){Year <-  rep(Year, length(X))}
  if(length(X) == 1 & length(Year) > 1){X  <-  rep(X, length(Year))}
  
  
  MatchYear   <-  match(Year, Parameters$Years)
  
  Rates   <-  Parameters$Rates.Res
  Tholds  <-  Parameters$Thold.Res
  
  if(is.null(dim(Rates))){Rates  <-  rbind(Rates)}
  if(is.null(dim(Tholds))){Tholds  <-  rbind(Tholds)}
  
  Rates        <-   Rates[MatchYear,]
  Tholds       <-   Tholds[MatchYear,]
    
    
  Tax <-  as.numeric(Lib.Tax(X, Rates,Tholds))
  
  return(Tax)
  
  
}
















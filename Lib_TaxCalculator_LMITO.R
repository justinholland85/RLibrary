######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tax.Parameters.LMITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################



Lib.Tax.Parameters.LMITO   <-  function(Years,...){
  
  
  FilePath         <-  paste0(Lib.Dir, "/LibData_TaxCode.csv")
  LibData.TaxCode  <-  read.csv(FilePath, header=TRUE)
  
  if(missing(Years)){Years   <-   LibData.TaxCode$income_year}
  Years            <-  c(Years, c(...))
  
  Base             <-  LibData.TaxCode$lmito_base
  
  Rates            <-  cbind(0,
                             LibData.TaxCode$lmito_rate_1,
                             LibData.TaxCode$lmito_rate_2, 
                             LibData.TaxCode$lmito_rate_3, 
                             LibData.TaxCode$lmito_rate_4) / 100
  
  Thold            <-  cbind(0,
                             LibData.TaxCode$lmito_threshold_1,
                             LibData.TaxCode$lmito_threshold_2, 
                             LibData.TaxCode$lmito_threshold_3,
                             LibData.TaxCode$lmito_threshold_4)
  
  rownames(Rates)  <-  LibData.TaxCode$income_year
  rownames(Thold)  <-  LibData.TaxCode$income_year
  
  
  Base[which(is.na(Base))]              <-  0
  Rates[which(is.na(Rates))]            <-  0
  
  Thold[which(is.na(Thold))]            <-  Inf
  
  Base   <-  Base[match(Years, LibData.TaxCode$income_year)]
  
  Rates  <-  Rates[match(Years, LibData.TaxCode$income_year),]
  Thold  <-  Thold[match(Years, LibData.TaxCode$income_year),]
  
  
  Output  <-  list("Base"  = Base,
                   "Rates" = Rates,
                   "Thold" = Thold,
                   "Years" = Years
  )
  
  
  return(Output)
  
}



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tax.LMITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Base           <-  Lib.Tax.Parameters.LMITO(2021)$Base
# Rates          <-  Lib.Tax.Parameters.LMITO(2021)$Rates
# Thresholds     <-  Lib.Tax.Parameters.LMITO(2021)$Thold
# X              <-  seq(0, 130000, 1000)


Lib.Tax.LMITO         <-  function(X, Base, Rates, Thresholds){
  
  
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
    
  } else { N.Tholds   <-  Dim.Tholds[2]}
  
  if(length(Base) == 1){Base   <-  rep(Base, N)}
  
  Thresholds          <-  cbind(Thresholds, Inf)
  
  # Initialisations:
  
  Offset              <-  Base
  
  # Loop over rates and thresholds
  
  for(i in 1:N.Rates){
    Amt        <-  pmin(pmax(X - Thresholds[,i], 0), Lib.NAN.To.Val(Thresholds[,i+1] - Thresholds[,i], Inf))
    AddOffset  <-  Amt * (Rates[,i])
    Offset     <-  pmax(Offset + AddOffset, 0)
  }
  
  return(Offset)
  
} 


# cbind(X, Lib.Tax.LMITO(X, Base, Rates, Thresholds))




######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.TaxCalculator.LMITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Lib.TaxCalculator.LMITO  <-  function(X, Year){
  
  Parameters         <-  Lib.Tax.Parameters.LMITO(Year)
  
  if(length(Year) == 1){Year <-  rep(Year, length(X))}
  if(length(X) == 1 & length(Year) > 1){X  <-  rep(X, length(Year))}
  
  MatchYear   <-  match(Year, Parameters$Years)
  
  Base        <-  Parameters$Base
  Rates       <-  Parameters$Rates
  Tholds      <-  Parameters$Thold
  
  if(is.null(dim(Rates))){Rates    <-  rbind(Rates)}
  if(is.null(dim(Tholds))){Tholds  <-  rbind(Tholds)}
  
  Base         <-   Base[MatchYear] 
  Rates        <-   Rates[MatchYear,]
  Tholds       <-   Tholds[MatchYear,]
  
  Offset       <-  as.numeric(Lib.Tax.LMITO(X, Base, Rates, Tholds))
  
  return(Offset)
  
}













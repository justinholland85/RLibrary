

# Years  <-  c(2017,2021)

######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.Tax.Parameters.LITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Lib.Tax.Parameters.LITO   <-  function(Years,...){
  
  
  FilePath         <-  paste0(Lib.Dir, "/LibData_TaxCode.csv")
  LibData.TaxCode  <-  read.csv(FilePath, header=TRUE)
  
  if(missing(Years)){Years   <-   LibData.TaxCode$income_year}
  Years            <-  c(Years, c(...))
  
  Base             <-  LibData.TaxCode$lito_max
  
  Rates            <-  cbind(0,
                             LibData.TaxCode$lito_rate_1,
                             LibData.TaxCode$lito_rate_2) / 100
  
  Thold            <-  cbind(0,
                             LibData.TaxCode$lito_threshold_1,
                             LibData.TaxCode$lito_threshold_2)
  
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
# Lib.Tax.LITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

# Base           <-  Lib.Tax.Parameters.LITO(2021)$Base
# Rates          <-  Lib.Tax.Parameters.LITO(2021)$Rates
# Thresholds     <-  Lib.Tax.Parameters.LITO(2021)$Thold
# X              <-  seq(37000, 68000, 1000)


Lib.Tax.LITO         <-  function(X, Base, Rates, Thresholds){
  
  
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

#cbind(X, Lib.Tax.LITO(X, Base, Rates, Thresholds))



######################################################################################################
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
# Lib.TaxCalculator.LITO
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
######################################################################################################

Lib.TaxCalculator.LITO  <-  function(X, Year){
  
  Parameters         <-  Lib.Tax.Parameters.LITO(Year)
  
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
  
  Offset       <-  as.numeric(Lib.Tax.LITO(X, Base, Rates, Tholds))
  
  return(Offset)
  
  
}






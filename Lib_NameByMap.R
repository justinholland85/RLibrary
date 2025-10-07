
Lib.NameByMap    <-  function(Data, NameMap){
  
  Names.Base                          <-  names(Data)
  
  Match                               <-  match(Names.Base, names(NameMap))
  
  Names.New                           <-  NameMap[Match]
  
  Which                               <-  which(is.na(Names.New))
  
  Names.New[Which]                    <-  Names.Base[Which]
  
  return(as.character(Names.New))
  
}




Lib.ALifeFileChecker    <-  function(Dir, Write){
  
  Dir.Write     <-  "/data/rab3/Playground/Justin/FileCheckerOutput"
  
  if(missing(Dir)){Dir        <-  getwd()}
  if(missing(Write)){Write    <-  1}
  
  
  
  Files    <- list.files(path = Dir)
  N.Files  <- length(Files)  
  
  Vars      <-  list()
  Dims      <-  list()
  Years     <-  list()
  Size_MB   <-  list()
  Made      <-  list()
  
  

  
  for(i in 1:N.Files){
    
    Size_MB[[i]]   <-  round(file.info(Files[[i]])$size / 1024^2)
    Made[[i]]      <-  file.info(Files[[i]])$mtime
    
    i.Data         <-  data.frame(fread(Files[[i]], integer64 = "numeric"))
    
    Dims[[i]]      <-  dim(i.Data)
    Vars[[i]]      <-  names(i.Data)
    
    YearVars       <-  c("z_year", "income_year")
    
    Which.YearVar      <-  which(YearVars %in% names(i.Data))
    
    if(length(Which.YearVar) > 0 ){
      
      VarName    <-  YearVars[Which.YearVar]
      
      Years[[i]] <-  table(i.Data[[VarName]])
      
    } else {  Years[[i]] <- "None"}
    
  }
  
  names(Vars)      <-  Files
  names(Dims)      <-  Files
  names(Years)     <-  Files
  names(Size_MB)   <-  Files
  names(Made)      <-  Files
  
  
  Size_MB          <- cbind(unlist(Size_MB))
  Made             <- cbind(unlist(lapply(Made, as.character)))
  
  Dims             <-  do.call(rbind, Dims)

  Rows             <-  Dims[,1]
  Cols             <-  Dims[,2]
  Rows.Simp        <-  Lib.BigNumbers(Rows)
  
  Dims             <-  data.frame("Rows"      = Rows,
                                  "Cols"      = Cols,
                                  "Rows.Simp" = Rows.Simp)
            
  
  Time             <-  Sys.time()
  

  
  Header           <-  list("Dir"   = Dir, 
                            "Time"  = Time, 
                            "Files" = Files
                            )
  
  Output           <- list("Header"  = Header, 
                           "Dims"    = Dims,
                           "Size_MB" = Size_MB,
                           "Made"    = Made,
                           "Years"   = Years,
                           "Vars"    = Vars)
  
  if(Write == 1){
  
  TimeStamp    <-  substr(Sys.time(), 1 , 19)
  TimeStamp    <-  gsub("-", "", TimeStamp)
  TimeStamp    <-  gsub(":", "",  TimeStamp)
  TimeStamp    <-  gsub(" ", "_", TimeStamp)
  
  
  Split            <-  strsplit(Dir, split = "/")[[1]]
  Split            <-  Split[length(Split)]
  
  
  Write.Name      <-  paste0(Dir.Write, "/", TimeStamp, "_", Split,  ".Rdata")  
  save(Output, file = Write.Name)      

  }
  
  return(Output)
  
  
}
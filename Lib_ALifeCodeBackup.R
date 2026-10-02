
Lib.ALifeCodeBackup   <-  function(){


  Dir.Backup   <-  "/data/ALife_Code_Backup/"
  
  DateStamp    <-  substr(Sys.time(), 1 , 10)
  DateStamp    <-  gsub("-", "", DateStamp)
  
  FileName     <-  paste0("Backup_", DateStamp)
  
  CheckFile    <-  paste0(Dir.Backup, FileName, ".zip")
  
  if(!file.exists(CheckFile)){
   
    GetWd               <-  getwd()
    
    Dir.List            <-  c(rab1 = "/data/rab1", rab2 = "/data/rab2", rab3 = "/data/rab3")
    
    N.Dir.List          <-  length(Dir.List)
    
    FileList            <-  as.character(unlist(sapply(Dir.List, list.files, recursive = TRUE, full.names = TRUE)))
    
    Extensions          <-  c("R", "r", "py", "sql", "Rmd", "do")
    
    FileList.Ext        <-  tools::file_ext(FileList)
    
    Selection           <-  FileList[FileList.Ext %in% Extensions]
    
    Selection.SizeMB    <-  unlist(lapply(lapply(Selection, file.info), Lib.Take.One, "size")) / 1024 ^ 2
    
    Selection.Target    <-  sub("^/data/", paste0(Dir.Backup, FileName, "/") , Selection)
    
    Create              <- sapply(unique(dirname(Selection.Target)), dir.create, recursive = TRUE, showWarnings = FALSE)
    
    Copy                <- file.copy(from = Selection, to = Selection.Target)
    
    setwd(Dir.Backup)
    
    Zip <- system2("zip", c("-r", paste0(FileName, ".zip"), FileName), stdout = FALSE, stderr = FALSE)
    
    unlink(FileName, recursive = TRUE)
    
    setwd(GetWd)
  }
  

  
}


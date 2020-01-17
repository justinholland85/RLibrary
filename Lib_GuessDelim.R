Lib.GuessDelim  <-  function(FileName){
  
  Candidates  <-  c("=",
                    "^",
                    ",",
                    "~",
                    "%",
                    "$",
                    "!",
                    "\t")
  
  
  FirstRow  <-  as.character(read.csv(file=FileName, header=TRUE, sep="`", nrows = 1)[1,1])
  
  
  Chars  <-  substring(FirstRow, seq(1, nchar(FirstRow)), seq(1, nchar(FirstRow)))
  Table  <-  sort(table(Chars), decreasing = TRUE)
  Names  <-  names(Table)
  Guess  <-  Names[which(Names %in% Candidates)][1]
  
  return(Guess)
}


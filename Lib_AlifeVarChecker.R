

Lib.AlifeVarChecker  <-  function(Directory){

  Getwd  <-  getwd()

if(missing(Directory)){Directory   <-  Getwd}
  
setwd(Directory)
  
Files    <-  list.files()
Files    <-  union(grep(pattern = ".csv",  Files, value = TRUE), 
                   grep(pattern = ".txt",  Files, value = TRUE))
                   
N.Files  <-  length(Files)


VarNames   <-  list()

for(i in 1:N.Files){
  
  Temp  <-  data.frame(fread(file = Files[i], header=TRUE, nrows=5))
  VarNames[[i]]  <- names(Temp)
  
}

Counts  <-  table(tolower(unlist(VarNames)))
Names   <-  names(Counts)

Identifiers  <-  c("clnt_intrnl_id",
                   "Clnt_Id",
                   "Clnt_Intrnl_Id",
                   "c_id_from",
                   "c_id_to",
                   "c_id",
                   "c_id_uhc",
                   "c_id_csg_head",
                   "tfn",
                   "c_tfn_uhc",
                   "c_tfn_csg_head",
                   "alife_id_001_from",
                   "alife_id_001_to",
                   "alife_id_001",
                   "alife_id_002",
                   "alife_id_003",
                   "alife_id_004",
                   "alife_id_005",
                   "alife_id_006",
                   "alife_id_007",
                   "alife_id_008",
                   "alife_id_009",
                   "alife_id_010",
                   "Reported_TFN",
                   "Matched_TFN",
                   "Ult_Hldg_Coy",
                   "Consolidated_Head_Coy",
                   "scrambled_tfn",
                   "scrambled_tfn_from",
                   "scrambled_tfn_to",
                   "c_bmt_group",
                   "Reported_ABN",
                   "Matched_ABN",
                   "CAC_Clnt_Iid",
                   "CAC_IID",
                   "CAC_Num",
                   "abn",
                   "c_abn",
                   "sp_id_enhanced",
                   "p_id",
                   "pid",
                   "a_id",
                   "alife_id"
                   )


Dangerous   <-  c("c_sa1_id",
                  "c_sa2_id",
                  "c_sa3_id",
                  "c_birth_date",
                  "c_birth_day_of_month",
                  "c_birth_month",
                  "c_lat",
                  "c_long",
                  "c_postcode",
                  "c_mb_id",
                  "sp_birth_date",
                  "sp_date_from",
                  "sp_date_to",
                  "sp_gender",
                  "sp_initials",
                  "sp_name_family",
                  "sp_name_given",
                  "sp_tfn_interpolated",
                  "sp_tfn_enhanced",
                  "sp_dob_day",
                  "sp_dob_month",
                  "c_death_day",
                  "c_death_month")
                  


Identifiers        <-  tolower(Identifiers)
Dangerous          <-  tolower(Dangerous)


IdentifierCounts   <-  cbind(Counts[which(Names %in% Identifiers)])
DangerousCounts    <-  cbind(Counts[which(Names %in% Dangerous)])
AllVarsCounts      <-  cbind(Counts)


Output             <-  list("Identifiers" = IdentifierCounts,
                            "Dangerous"  = DangerousCounts,
                            "AllVars"    = AllVarsCounts,
                            "Files"            = Files)

setwd(Getwd)

return(Output)
}





















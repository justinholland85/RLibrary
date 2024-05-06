#$$$$$ These are the source lines, add these to other codes, do not uncomment

# Work
# source("/rab3/Playground/Justin/Code/R Library/Start.R")

#--- Alien2021:
# source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")

#--- Alien2016:
# source("C:/Users/justi/Documents/GitHub/RLibrary/Start.R")


#--- Dell2014:
# source("C:/Users/Justin/Documents/GitHub/RLibrary/Start.R")

#--- RAB RADLAB:
# source("/home/rucwa6/gitlab/pal_revenue_analysis/Personal_Codes/Justin/R_Library/Start.R")

#--- Nick Laptop ("09503289-TMP449")
# source("C:/Users/09503289/Desktop/R - Codes/Justins Codes/Start.R")



######################################################################################################
#$$$$$ Need to change the Library directory, nothing else:

# Function Library Directory:

#--- Dell2014:
#Lib.Dir  <-  "C:/Users/Justin/Documents/GitHub/RLibrary"

#--- Alien2021
# Lib.Dir  <-  "C:/Users/justi/Documents/GitHub/RLibrary/"

#--- Alien2016
# Lib.Dir  <-  "C:/Users/justi/Documents/GitHub/RLibrary/"


#--- Work
#Lib.Dir  <-  "/rab3/Playground/Justin/Code/R Library"

#--- RAB RADLAB
#Lib.Dir  <- "/home/rucwa6/gitlab/pal_revenue_analysis/Personal_Codes/Justin/R_Library"


######################################################################################################

PathMap   <-  list("DESKTOP-MLKCE7A" = "C:/Users/justi/Documents/GitHub/RLibrary/",
                   "DELL2014"        = "C:/Users/Justin/Documents/GitHub/RLibrary",
                   "ALIEN2021"       = "C:/Users/justi/Documents/GitHub/RLibrary/",
                   "09503289-TMP449" = "C:/Users/09503289/Desktop/R - Codes/Justins Codes",
                   "DESKTOP-87M804Q" = "C:/Users/nickm/OneDrive/Documents/Coding/RLibrary-master"
                   )

NodeName  <-  Sys.info()["nodename"]

Lib.Dir   <-  PathMap[[NodeName]]


setwd(Lib.Dir)
source("Start_List.R")







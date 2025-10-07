
if(require(data.table) == FALSE){install.packages('data.table'); library(data.table)}

setwd(Lib.Dir)

# Graphics
source("Lib_ColourScheme.R")
source("Lib_ColourScheme3D.R")
source("Lib_ColourScheme_ExpRow.R")
source("Lib_ColourScheme_ExpCol.R")
source("Lib_Alluvial.R")
source("Lib_MultiAlluvial.R")
source("Lib_Circle.R")
source("Lib_TreeMap.R")
source("Lib_PolyPlot.R")
source("Lib_MountainPlot.R")
source("Lib_Legend.R")
source("Lib_LayeredHier.R")
source("Lib_HeatMap.R")
source("Lib_LogLines.R")
source("Lib_PlotLogLines.R")
source("Lib_HeatLegend.R")
source("Lib_BigNumbers.R")
source("Lib_PlotTable.R")
source("Lib_HeatScale.R")
source("Lib_Axis.R")
source("Lib_PlotHorzBar.R")
source("Lib_PrintList.R")
source("Lib_PlotRegions.R")
source("Lib_PlotArea.R")
source("Lib_Join.R")
source("Lib_Grids.R")
source("Lib_BarPlotStacked.R")
source("Lib_TrafficLightPlot.R")
source("Lib_Plot_Blank.R")
source("Lib_TextTable.R")

# Data 
source("Lib_RangeNames.R")
source("Lib_Hist.R")
source("Lib_Zeta.R")
source("Lib_RangeMap.R")
source("Lib_Rank.R")
source("Lib_AsNumeric.R")
source("Lib_Integer64Fix.R")
source("Lib_Dates.R")
source("Lib_Matrix_CellRef_Extract.R")
source("Lib_Rbind_DF.R")
source("Lib_ApplyMap.R")
source("Lib_Quantiles.R")
source("Lib_RowMax.R")
source("Lib_CumSum.R")
source("Lib_Mround.R")
source("Lib_SeqOn.R")
source("Lib_InvertedFill.R")
source("Lib_DF_Unlist.R")
source("Lib_NameByMap.R")
source("Lib_FillDown.R")


source("Lib_LeadingZeroes.R")
source("Lib_DummyDF.R")


# General Analytics
source("Lib_GeneralAnalytics_1D.R")
source("Lib_GeneralAnalytics_1D_DummyData.R")

# Strings
source("Lib_LoopedGrep.R")


# Organise
source("Lib_GiveNames.R")
source("Lib_MapListToSet.R")

# Lists
source("Lib_ListTrivials.R")
source("Lib_Layer_List_To_3DMat.R")
source("Lib_ListNumeric0.R")

# Fun
source("Lib_Sierpinski.R")
source("Lib_Markov.R")

# Admin
source("Lib_GuessDelim.R")
source("Lib_Table_To_DF.R")
source("Lib_Load.R")

# Numeric
source("Lib_LogScale.R")
source("Lib_Between.R")

# Touples
source("Lib_Tuple_Count.R")
source("Lib_Tuple_Match.R")

# Alife Specials
source("Lib_AlifeVarChecker.R")
source("Lib_SafeTable.R")
source("Lib_TaxCalculator.R")

# Stats
source("Lib_OLS.R")

# Math
source("Lib_Trig.R")
source("Lib_PowerLaw.R")
source("Lib_Primes.R")

# Reference
source("Lib_Recall.R")

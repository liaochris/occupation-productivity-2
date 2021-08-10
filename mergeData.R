# Import libraries
library(data.table)
library("readxl")
library(dplyr)
library(testthat)
library(readr)
library(stringr)
library(tidyr)
library(mgsub)
library(foreach)
library(vroom)

setwd("~/Google Drive/Non-Academic Work/Research/Traina/occupation-productivity-2/")
#starting this year we have files
'%ni%' <- Negate('%in%')

OEWS <- fread("Merge/OEWS_agg.csv")
OEWS_min <- unique(OEWS[,.SD, .SDcols = !c("NAICS_TITLE_OE", "OCC_TITLE_OE", "ANNUAL_OE", 
                                           "GROUP_OE", "HOURLY_OE", "LEVEL_OE")])
OEWS_min$OCC_CODE <- as.numeric(gsub("-", "", OEWS_min$OCC_CODE))
OEWS_min[str_length(OEWS_min$NAICS) == 2]$NAICS <- str_pad(OEWS_min[str_length(OEWS_min$NAICS) == 2]$NAICS, 6, "right", "0")

#productivity
MFP <- fread("Merge/MFP_agg.csv")
MFP_min <- unique(MFP[,.SD, .SDcols = !c("SECTOR_CODE_MP", "SECTOR_NAME_MP")])
MFP_min$NAICS <- as.character(MFP_min$NAICS)
LP <- fread("Merge/LP_agg.csv")
LP_min <- unique(LP[,.SD, .SDcols = !c("INDUSTRY_CODE_IP", "INDUSTRY_TEXT_IP", "SEASONAL_CODE_IP",
                                       "SEASONAL_TEXT_IP", "SECTOR_CODE_IP", "SECTOR_TEXT_IP", 
                                       "AREA_CODE_IP", 'AREA_TEXT_IP')])
mfp_merge <- MFP_min[OEWS_min, on = c("NAICS", "YEAR")]
lp_merge <- LP_min[mfp_merge, on = c("NAICS", "YEAR")]

#occupation characteristics
ORS <- fread("Merge/ORS_agg.csv")
ORS_min <- unique(ORS[,.SD, .SDcols = !c("occupation_code_OR", "occupation_text_OR",
                                         "period_OR", "seasonal_code_OR", "seasonal_text_OR",
                                         "ownership_code_OR")])
colnames(ORS_min)[1] <- 'OCC_CODE'
ORS_merge <- ORS_min[lp_merge, on = c("OCC_CODE")]

#employer cost
EC <- fread("Merge/EC_agg.csv")
EC_min <- unique(EC[,.SD, .SDcols = !c("occupation_text_CM", "seasonal_code_CM",
                                       "seasonal_text_CM", "owner_text_CM",
                                       "industry_text_CM", "subcell_code_CM", "subcell_text_CM",
                                       "area_code_CM", "area_text_CM")])
EC_min <- EC_min[EC_min$period_CM == "Q02"]
colnames(EC_min)[1] <- "OCC_CODE"
colnames(EC_min)[2] <- "YEAR"
colnames(EC_min)[5] <- "NAICS"

EC_min_all <- EC_min[NAICS == "000000"]
EC_min_all <- EC_min_all[,.SD, .SDcols = !c("NAICS")]

#additional entries created by the differentiation between different owner types
EC_merge <-  EC_min_all[ORS_merge, on = c("YEAR", "OCC_CODE"), allow.cartesian = TRUE]


#national compensation surveys
NCS_B <- fread("Merge/NCS_B_agg.csv")
NCS_B_min <- unique(NCS_B[,.SD, .SDcols = !c("period_NB",	"occupation_text_NB", "seasonal_code_NB", 
                                             "seasonal_text_NB", "ownership_text_NB",
                                             "industry_text_NB", "subcell_code_NB", "subcell_text_NB")])
colnames(NCS_B_min)[1] <- "OCC_CODE"
colnames(NCS_B_min)[2] <- "YEAR"
colnames(NCS_B_min)[4] <- "NAICS"
NCS_B_min_all <- NCS_B_min[NAICS == "000000"]
NCS_B_min_all <- NCS_B_min_all[,.SD, .SDcols = !c("NAICS")]
#additional entries created by the differentiation between different owner types
NCS_B_merge <-  NCS_B_min_all[EC_merge, on = c("YEAR", "OCC_CODE"), allow.cartesian = TRUE]

NCS_W <- fread("Merge/NCS_W_agg.csv")
NCS_W_min <- unique(NCS_W[,.SD, .SDcols = !c("period_NW",	"soc_text_NW", "seasonality_NW", 
                                             "ownership_text_NW", "industry_text_NW",
                                             "subcell_id_code_NW", "subcell_id_text_NW")])
colnames(NCS_W_min)[1] <- "OCC_CODE"
colnames(NCS_W_min)[2] <- "YEAR"
colnames(NCS_W_min)[4] <- "NAICS"
NCS_W_min_all <- NCS_W_min[NAICS == "000000"]
NCS_W_min_all <- NCS_W_min_all[,.SD, .SDcols = !c("NAICS")]
#additional entries created by the differentiation between different owner types
NCS_W_merge <-  NCS_W_min_all[NCS_B_merge, on = c("YEAR", "OCC_CODE"), allow.cartesian = TRUE]

#fwrite(NCS_W_merge, "merged_dataset_noWM.csv")
#saveRDS(NCS_W_merge, file = "merged_dataset_noWM.RDS") 

#Wage Model
WM <- fread("Merge/WM_agg.csv")
WM_min <- unique(WM[,.SD, .SDcols = !c("period_wm",	"soc_text_wm", "area_code_wm", 
                                       "seasonal_code_wm", "seasonal_text_wm", "ownership_text_wm",
                                       "industry_text_wm", "subcell_code_wm", "subcell_text_wm")])
colnames(WM_min)[1] <- "OCC_CODE"
colnames(WM_min)[2] <- "YEAR"
colnames(WM_min)[4] <- "NAICS"

WM_min_all <- WM_min[,.SD, .SDcols = !c("NAICS")]
#additional entries created by the differentiation between different owner types
WM_merge <-  WM_min_all[NCS_W_merge, on = c("YEAR", "OCC_CODE"), allow.cartesian = TRUE]

#fwrite(WM_merge, "merged_dataset.csv")
#saveRDS(WM_merge, file = "merged_dataset.RDS") 


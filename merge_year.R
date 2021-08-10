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

OEWS <- fread("Merge/OEWS_agg_year.csv")
OEWS_min <- unique(OEWS[,.SD, .SDcols = !c("NAICS_TITLE_OE", "OCC_TITLE_OE", "ANNUAL_OE", 
                                           "GROUP_OE", "HOURLY_OE")])
OEWS_min$OCC_CODE <- as.numeric(gsub("-", "", OEWS_min$OCC_CODE))
colnames(OEWS_min)[1] <- "NAICS_og"

####################################################################################
#################CREATE MULTIPLE LEVELS OF INDUSTRY GRANULARITY#####################
####################################################################################

# SIX DIGIT INDUSTRY 
OEWS_min[,`NAICS_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_OE == 6, OEWS_min$NAICS_og, NA))]
# FIVE DIGIT INDUSTRY
null_6 <- OEWS_min[is.na(NAICS_6d)]
present_6 <- OEWS_min[!is.na(NAICS_6d)]
OEWS_min[is.na(NAICS_6d), `NAICS_5d`:=as.numeric(ifelse(null_6$LEVEL_OE == 5, null_6$NAICS_og, NA))]
OEWS_min[!is.na(NAICS_6d),`NAICS_5d`:=as.numeric(str_pad(substr(present_6$NAICS_6d, 1, 5), 
                                                         6, side = "right", pad = "0"))]
# FOUR DIGIT INDUSTRY
null_5 <- OEWS_min[is.na(NAICS_5d)]
present_5 <- OEWS_min[!is.na(NAICS_5d)]
OEWS_min[is.na(NAICS_5d), `NAICS_4d`:=as.numeric(ifelse(null_5$LEVEL_OE == 4, null_5$NAICS_og, NA))]
OEWS_min[!is.na(NAICS_5d),`NAICS_4d`:=as.numeric(str_pad(substr(present_5$NAICS_5d, 1, 4), 
                                                         6, side = "right", pad = "0"))]
# THREE DIGIT INDUSTRY
null_4 <- OEWS_min[is.na(NAICS_4d)]
present_4 <- OEWS_min[!is.na(NAICS_4d)]
OEWS_min[is.na(NAICS_4d), 
         `NAICS_3d`:=as.numeric(ifelse(null_4$LEVEL_OE == 3, 
                                       unlist(lapply(null_4$NAICS_og, function (x) gsub("A", "0", x))), 
                                       NA))]
OEWS_min[!is.na(NAICS_4d),`NAICS_3d`:=as.numeric(str_pad(substr(present_4$NAICS_4d, 1, 3), 
                                                         6, side = "right", pad = "0"))]
# TWO DIGIT INDUSTRY
null_3 <- OEWS_min[is.na(NAICS_3d)]
present_3 <- OEWS_min[!is.na(NAICS_3d)]
OEWS_min[is.na(NAICS_3d), `NAICS_2d`:=ifelse(null_3$LEVEL_OE == 2, null_3$NAICS_og, NA)]
OEWS_min[!is.na(NAICS_3d),`NAICS_2d`:=as.numeric(str_pad(substr(present_3$NAICS_3d, 1, 2), 
                                                         6, side = "right", pad = "0"))]

# ONE DIGIT INDUSTRY
special_2 <- OEWS_min[NAICS_2d %in% c("31-33","44-45","48-49")]
normal_2 <- OEWS_min[NAICS_2d %ni% c("31-33","44-45","48-49")]
OEWS_min[NAICS_2d %in% c("31-33","44-45","48-49"), 
         `NAICS_1d`:=ifelse(substr(special_2$NAICS_2d, 1, 1) == "3", "300000", "400000")]
OEWS_min[NAICS_2d %ni% c("31-33","44-45","48-49"),
         `NAICS_1d`:=as.numeric(str_pad(substr(normal_2$NAICS_2d, 1, 1), 
                                        6, side = "right", pad = "0"))]

####################################################################################
#################CREATE MULTIPLE LEVELS OF OCCUPATION GRANULARITY###################
####################################################################################
OEWS_min$OCC_CODE <- as.numeric(gsub("-", "", OEWS_min$OCC_CODE))
colnames(OEWS_min)[which(colnames(OEWS_min) == "OCC_CODE")] <- "OCC_CODE_og"


# SIX DIGIT OCCUPATIONS
#ASSIGNING OCCUPATION LEVELS
OEWS_min[OEWS_min$OCC_CODE %% 10 ** 5 == 0, `LEVEL_OCC_OE` := 1]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 4 == 0, `LEVEL_OCC_OE` := 2]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 3 == 0, `LEVEL_OCC_OE` := 3]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 2 == 0, `LEVEL_OCC_OE` := 4]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 1 == 0, `LEVEL_OCC_OE` := 5]
OEWS_min[is.na(LEVEL_OCC_OE), `LEVEL_OCC_OE` := 6]

OEWS_min[,`OCC_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_OCC_OE == 6, OEWS_min$OCC_CODE_og, NA))]
# FIVE DIGIT OCCUPATIONS
null_6 <- OEWS_min[is.na(OCC_6d)]
present_6 <- OEWS_min[!is.na(OCC_6d)]
OEWS_min[is.na(OCC_6d), `OCC_5d`:=as.numeric(ifelse(null_6$LEVEL_OCC_OE == 5, null_6$OCC_CODE_og, NA))]
OEWS_min[!is.na(OCC_6d),`OCC_5d`:=as.numeric(str_pad(substr(present_6$OCC_6d, 1, 5), 
                                                     6, side = "right", pad = "0"))]
# FOUR DIGIT OCCUPATIONS
null_5 <- OEWS_min[is.na(OCC_5d)]
present_5 <- OEWS_min[!is.na(OCC_5d)]
OEWS_min[is.na(OCC_5d), `OCC_4d`:=as.numeric(ifelse(null_5$LEVEL_OCC_OE == 4, null_5$OCC_CODE_og, NA))]
OEWS_min[!is.na(OCC_5d),`OCC_4d`:=as.numeric(str_pad(substr(present_5$OCC_5d, 1, 4), 
                                                     6, side = "right", pad = "0"))]

# THREE DIGIT OCCUPATIONS
null_4 <- OEWS_min[is.na(OCC_4d)]
present_4 <- OEWS_min[!is.na(OCC_4d)]
OEWS_min[is.na(OCC_4d), `OCC_3d`:=as.numeric(ifelse(null_4$LEVEL_OCC_OE == 3, null_4$OCC_CODE_og, NA))]
OEWS_min[!is.na(OCC_4d),`OCC_3d`:=as.numeric(str_pad(substr(present_4$OCC_4d, 1, 3), 
                                                     6, side = "right", pad = "0"))]

# TWO DIGIT OCCUPATIONS
null_3 <- OEWS_min[is.na(OCC_3d)]
present_3 <- OEWS_min[!is.na(OCC_3d)]
OEWS_min[is.na(OCC_3d), `OCC_2d`:=as.numeric(ifelse(null_3$LEVEL_OCC_OE == 2, null_3$OCC_CODE_og, NA))]
OEWS_min[!is.na(OCC_3d),`OCC_2d`:=as.numeric(str_pad(substr(present_3$OCC_3d, 1, 2), 
                                                     6, side = "right", pad = "0"))]

# ONE DIGIT OCCUPATIONS
null_2 <- OEWS_min[is.na(OCC_2d)]
present_2 <- OEWS_min[!is.na(OCC_2d)]
OEWS_min[is.na(OCC_2d), `OCC_1d`:=as.numeric(ifelse(null_2$LEVEL_OCC_OE == 1, null_2$OCC_CODE_og, NA))]
OEWS_min[!is.na(OCC_2d),`OCC_1d`:=as.numeric(str_pad(substr(present_2$OCC_2d, 1, 1), 
                                                     6, side = "right", pad = "0"))]

#LP NAICS are 2, 3, 4, 5d
#can't convert to numeric
#filter out columns with more than 20% empty
LP <- fread("Merge/LP_agg.csv")
LP_min <- unique(LP[,.SD, .SDcols = !c("INDUSTRY_CODE_IP", "INDUSTRY_TEXT_IP", "SEASONAL_CODE_IP",
                                       "SEASONAL_TEXT_IP", "SECTOR_CODE_IP", "SECTOR_TEXT_IP", 
                                       "AREA_CODE_IP", 'AREA_TEXT_IP')])
LP_min <- LP_min[,apply(LP_min, 2, function (x) mean(is.na(x))) < .2, with = FALSE]

OEWS_min$NAICS_5d <- as.character(OEWS_min$NAICS_5d)
OEWS_min$NAICS_4d <- as.character(OEWS_min$NAICS_4d)
OEWS_min$NAICS_3d <- as.character(OEWS_min$NAICS_3d)
OEWS_min$NAICS_2d <- as.character(OEWS_min$NAICS_2d)

#perform the merge on 5 digits
cols <- unique(c(colnames(LP_min), colnames(OEWS_min)))
LP_min_col <- colnames(LP_min)[-c(1:2)]
merge <- LP_min[OEWS_min, on = c("YEAR", "NAICS" = "NAICS_5d"), ..cols]
merge[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = LP_min_col]
merge_success <- merge[missing_obs < length(LP_min_col)]

#perform the merge on 4 digits
OEWS_min_cols <- colnames(OEWS_min)
merge_fail <- merge[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r2 <- LP_min[merge_fail, on = c("YEAR", "NAICS" = "NAICS_4d"), ..cols]
merge_r2[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = LP_min_col]
merge_success2 <- merge_r2[missing_obs < length(LP_min_col)]

#perform the merge on 3 digits
merge_fail2 <- merge_r2[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r3 <- LP_min[merge_fail2, on = c("YEAR", "NAICS" = "NAICS_3d"), ..cols]
merge_r3[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = LP_min_col]
merge_success3 <- merge_r3[missing_obs < length(LP_min_col)]

#perform the merge on 2 digits
merge_fail3 <- merge_r3[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r4 <- LP_min[merge_fail3, on = c("YEAR", "NAICS" = "NAICS_2d"), ..cols]
merge_r4[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = LP_min_col]
LP_merged <- rbind(merge_success, merge_success2, merge_success3, merge_r4)

print(mean(LP_merged$missing_obs == 0))

cols <- colnames(LP_merged)[colnames(LP_merged) %ni% c("missing_obs")]
LP_merged <- LP_merged[,..cols]
colnames(LP_merged)[which(colnames(LP_merged) == "NAICS")] <- "NAICS_IP"

#occupational characteristics
#ORS OCC  NAICS are 2d, 6d
#can't convert to numeric
#filter out columns with more than 20% empty
ORS <- fread("Merge/ORS_agg.csv")
ORS_min <- unique(ORS[,.SD, .SDcols = !c("occupation_code_OR", "occupation_text_OR",
                                         "period_OR", "seasonal_code_OR", "seasonal_text_OR")])

ORS_min <- ORS_min[,apply(ORS_min, 2, function (x) mean(is.na(x))) < .2, with = FALSE]

#perform the merge on 6 digits
cols <- unique(c(colnames(ORS_min), colnames(LP_merged)))
ORS_min_col <- colnames(ORS_min)[-c(1:2)]
merge <- ORS_min[LP_merged, on = c("soc_code" = "OCC_6d"), ..cols]
merge[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = ORS_min_col]
merge_success <- merge[missing_obs < length(ORS_min_col)]

#perform the merge on 2 digits
ORS_min_cols <- colnames(LP_merged)
merge_fail <- merge[missing_obs == length(ORS_min_col),..ORS_min_cols]
merge_r2 <- ORS_min[merge_fail, on = c("soc_code" = "OCC_2d"), ..cols]
merge_r2[,missing_obs := Reduce("+", lapply(.SD, is.na)), .SDcols = ORS_min_col]

ORS_merged <- rbind(merge_success, merge_r2)

print(mean(ORS_merged$missing_obs != length(ORS_min_col)))
print(mean(ORS_merged$missing_obs != 0))

#NCS wages
#
#filter out columns with more than 20% empty

NCS_W <- fread("Merge_reduced/NCS_W_reduced_agg.csv")

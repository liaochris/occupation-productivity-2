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
merge <- LP_min[OEWS_min, on = c("YEAR", "NAICS_IP" = "NAICS_5d"), ..cols]
merge[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col]
merge_success <- merge[missing_obs < length(LP_min_col)]

#perform the merge on 4 digits
OEWS_min_cols <- colnames(OEWS_min)
merge_fail <- merge[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r2 <- LP_min[merge_fail, on = c("YEAR", "NAICS_IP" = "NAICS_4d"), ..cols]
merge_r2[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col]
merge_success2 <- merge_r2[missing_obs < length(LP_min_col)]

#perform the merge on 3 digits
merge_fail2 <- merge_r2[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r3 <- LP_min[merge_fail2, on = c("YEAR", "NAICS_IP" = "NAICS_3d"), ..cols]
system.time(merge_r3[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col])
system.time(merge_r3[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col])
merge_success3 <- merge_r3[missing_obs < length(LP_min_col)]

#perform the merge on 2 digits
merge_fail3 <- merge_r3[missing_obs == length(LP_min_col),..OEWS_min_cols]
merge_r4 <- LP_min[merge_fail3, on = c("YEAR", "NAICS_IP" = "NAICS_2d"), ..cols]
merge_r4[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col]
#system.time(merge_r4[,missing_obs := rowSums(is.na(.SD)), .SDcols = LP_min_col])
LP_merged <- rbind(merge_success, merge_success2, merge_success3, merge_r4)

print(mean(LP_merged$missing_obs == 0))

cols <- colnames(LP_merged)[colnames(LP_merged) %ni% c("missing_obs")]
LP_merged <- LP_merged[,..cols]

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
merge <- ORS_min[LP_merged, on = c("soc_code_OR" = "OCC_6d"), ..cols]
merge[,missing_obs := rowSums(is.na(.SD)), .SDcols = ORS_min_col]
merge_success <- merge[missing_obs < length(ORS_min_col)]

#perform the merge on 2 digits
ORS_min_cols <- colnames(LP_merged)
merge_fail <- merge[missing_obs == length(ORS_min_col),..ORS_min_cols]
merge_r2 <- ORS_min[merge_fail, on = c("soc_code_OR" = "OCC_2d"), ..cols]
merge_r2[,missing_obs := rowSums(is.na(.SD)), .SDcols = ORS_min_col]
ORS_merged <- rbind(merge_success, merge_r2)

#NCS wages
#all NAICS can be matched on 6 digit level
#filter out columns with more than 20% empty

NCS_W <- fread("Merge_reduced/NCS_W_reduced_agg.csv")
#no area
NCS_W <- NCS_W[NCS_W$area_code_NW == 99999]
NCS_W_min <- unique(NCS_W[,.SD, .SDcols = !c("soc_text_NW", "seasonality_NW",
                                             "period_NW", "ownership_text_NW", "industry_text_NW", 
                                             "level_code_NW", "level_text_NW", 
                                             "state_code_NW", "area_code_NW", "area_text_NW",
                                             "subcell_id_code_NW", "subcell_id_text_NW")])
NCS_W_min <- NCS_W_min[,apply(NCS_W_min, 2, function (x) mean(is.na(x))) < .2, with = FALSE]

#perform the merge on 6 digits occupation, 3 digit NAICS
cols <- unique(c(colnames(NCS_W_min), colnames(ORS_merged)))
NCS_W_min_col <- colnames(NCS_W_min)[-c(1:2, 4)]
merge <- NCS_W_min[ORS_merged, on = c("year" = "YEAR", "soc_code_NW" = "OCC_6d",
                                      "industry_code_NW" = "NAICS_3d"), ..cols, 
                   allow.cartesian=TRUE]
merge[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success <- merge[missing_obs < length(NCS_W_min_col)]

#perform the merge on 5 digits occupation, 3 digit NAICS
NCS_W_min_cols <- colnames(ORS_merged)
merge_fail <- merge[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r2 <- NCS_W_min[merge_fail, on = c("year" = "YEAR", "soc_code_NW" = "OCC_5d",
                                         "industry_code_NW" = "NAICS_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r2[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success2 <- merge_r2[missing_obs < length(NCS_W_min_col)]

#perform the merge on 4 digits occupation, 3 digit NAICS
merge_fail2 <- merge_r2[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r3 <- NCS_W_min[merge_fail2, on = c("year" = "YEAR", "soc_code_NW" = "OCC_4d",
                                          "industry_code_NW" = "NAICS_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r3[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success3 <- merge_r3[missing_obs < length(NCS_W_min_col)]

#perform the merge on 3 digits occupation, 3 digit NAICS
merge_fail3 <- merge_r3[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r4 <- NCS_W_min[merge_fail3, on = c("year" = "YEAR", "soc_code_NW" = "OCC_3d",
                                          "industry_code_NW" = "NAICS_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r4[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success4 <- merge_r4[missing_obs < length(NCS_W_min_col)]

#perform the merge on 2 digits occupation, 3 digit NAICS
merge_fail4 <- merge_r4[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r5 <- NCS_W_min[merge_fail4, on = c("year" = "YEAR", "soc_code_NW" = "OCC_2d",
                                          "industry_code_NW" = "NAICS_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r5[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success5 <- merge_r5[missing_obs < length(NCS_W_min_col)]

#perform the merge on 1 digits occupation, 3 digit NAICS
merge_fail5 <- merge_r5[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r6 <- NCS_W_min[merge_fail5, on = c("year" = "YEAR", "soc_code_NW" = "OCC_1d",
                                          "industry_code_NW" = "NAICS_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r6[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success6 <- merge_r6[missing_obs < length(NCS_W_min_col)]

#perform the merge on 6 digits occupation, no NAICS
merge_fail6 <- merge_r6[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r7 <- NCS_W_min[merge_fail6, on = c("year" = "YEAR", "soc_code_NW" = "OCC_6d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r7[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success7 <- merge_r7[missing_obs < length(NCS_W_min_col)]

#perform the merge on 5 digits occupation, no NAICS
merge_fail7 <- merge_r7[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r8 <- NCS_W_min[merge_fail7, on = c("year" = "YEAR", "soc_code_NW" = "OCC_5d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r8[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success8 <- merge_r8[missing_obs < length(NCS_W_min_col)]

#perform the merge on 4 digits occupation, no NAICS
merge_fail8 <- merge_r8[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r9 <- NCS_W_min[merge_fail8, on = c("year" = "YEAR", "soc_code_NW" = "OCC_4d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r9[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success9 <- merge_r9[missing_obs < length(NCS_W_min_col)]

#perform the merge on 3 digits occupation, no NAICS
merge_fail9 <- merge_r9[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r10 <- NCS_W_min[merge_fail9, on = c("year" = "YEAR", "soc_code_NW" = "OCC_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r10[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success10 <- merge_r10[missing_obs < length(NCS_W_min_col)]

#perform the merge on 2 digits occupation, no NAICS
merge_fail10 <- merge_r10[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r11 <- NCS_W_min[merge_fail10, on = c("year" = "YEAR", "soc_code_NW" = "OCC_2d"), ..cols, 
                       allow.cartesian=TRUE]
merge_r11[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]
merge_success11 <- merge_r11[missing_obs < length(NCS_W_min_col)]

#perform the merge on 1 digits occupation, no NAICS
merge_fail11 <- merge_r11[missing_obs == length(NCS_W_min_col), ..NCS_W_min_cols]
merge_r12 <- NCS_W_min[merge_fail11, on = c("year" = "YEAR", "soc_code_NW" = "OCC_1d"), ..cols, 
                       allow.cartesian=TRUE]
merge_r12[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_W_min_col]

NCS_W_merged <- rbind(merge_success, merge_success2, merge_success3, merge_success4,
                      merge_success5, merge_success6, merge_success7, merge_success8,
                      merge_success9, merge_success10, merge_success11, merge_r12)

#NCS benefits
#all NAICS can be matched on 6 digit level
#filter out columns with more than 20% empty

NCS_B <- fread("Merge_reduced/NCS_B_reduced_agg.csv")
NCS_B_min <- unique(NCS_B[,.SD, .SDcols = !c("occupation_text_NB", "seasonal_code_NB", "seasonal_text_NB",
                                             "period_NB", "ownership_text_NB", "industry_text_NB", 
                                             "subcell_code_NB", "subcell_text_NB")])
NCS_B_min <- NCS_B_min[,apply(NCS_B_min, 2, function (x) mean(is.na(x))) < .2, with = FALSE]

#perform the merge on 1 digits occupation, 3 digit NAICS
cols <- unique(c(colnames(NCS_B_min), colnames(NCS_W_merged)))
NCS_B_min_col <- colnames(NCS_B_min)[-c(1:2, 4)]
merge <- NCS_B_min[NCS_W_merged, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_1d",
                                        "industry_code_NB" = "NAICS_3d"), ..cols, 
                   allow.cartesian=TRUE]
merge[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success <- merge[missing_obs < length(NCS_B_min_col)]

#perform the merge on 1 digits occupation, 1 digit NAICS
NCS_B_min_cols <- colnames(NCS_W_merged)
system.time(merge_fail <- merge[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols])
merge_r2 <- NCS_B_min[merge_fail, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_1d",
                                         "industry_code_NB" = "NAICS_1d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r2[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success2 <- merge_r2[missing_obs < length(NCS_B_min_col)]

#perform the merge on 6 digits occupation
merge_fai2 <- merge_r2[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r3 <- NCS_B_min[merge_fai2, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_6d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r3[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success3 <- merge_r3[missing_obs < length(NCS_B_min_col)]

#perform the merge on 5 digits occupation
merge_fai3 <- merge_r3[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r4 <- NCS_B_min[merge_fai3, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_5d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r4[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success4 <- merge_r4[missing_obs < length(NCS_B_min_col)]

#perform the merge on 4 digits occupation
merge_fai4 <- merge_r4[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r5 <- NCS_B_min[merge_fai4, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_4d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r5[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success5 <- merge_r5[missing_obs < length(NCS_B_min_col)]

#perform the merge on 3 digits occupation
merge_fai5 <- merge_r5[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r6 <- NCS_B_min[merge_fai5, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_3d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r6[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success6 <- merge_r6[missing_obs < length(NCS_B_min_col)]

#perform the merge on 2 digits occupation
merge_fai6 <- merge_r6[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r7 <- NCS_B_min[merge_fai6, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_2d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r7[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]
merge_success7 <- merge_r7[missing_obs < length(NCS_B_min_col)]

#perform the merge on 1 digit occupation
merge_fai7 <- merge_r7[missing_obs == length(NCS_B_min_col), ..NCS_B_min_cols]
merge_r8 <- NCS_B_min[merge_fai7, on = c("year" = "YEAR", "occupation_code_NB" = "OCC_1d"), ..cols, 
                      allow.cartesian=TRUE]
merge_r8[,missing_obs := rowSums(is.na(.SD)), .SDcols = NCS_B_min_col]

NCS_B_merge <- rbind(merge_success, merge_success2, merge_success3, merge_success4,
                     merge_success5, merge_success6, merge_success7, merge_r8)

#WM wages
#all NAICS can be matched on 6 digit level
#filter out columns with more than 20% empty

WM <- fread("Merge_reduced/WM_reduced_agg.csv")
WM <- WM[area_code_wm == 0]
WM_min <- unique(WM[,.SD, .SDcols = !c("occupation_text_wm", "area_code_wm", "area_text_wm", "period_wm",
                                       "ownership_text_wm", "seasonal_code_wm", "seasonal_text_wm", 
                                       "industry_text_wm", "subcell_code_wm", "subcell_text_wm",
                                       "level_text_wm")])
WM_min <- WM_min[,apply(WM_min, 2, function (x) mean(is.na(x))) < .2, with = FALSE]

#perform the merge on 6 digits occupation
cols <- unique(c(colnames(WM_min), colnames(NCS_B_merged)))
WM_min_col <- colnames(WM_min)[-c(1:2, 4)]
merge <- WM_min[NCS_B_merged, on = c("year" = "YEAR", "occupation_code_WM" = "OCC_6d"), ..cols, 
                   allow.cartesian=TRUE]
merge[,missing_obs := rowSums(is.na(.SD)), .SDcols = WM_min_col]
merge_success <- merge[missing_obs < length(WM_min_col)]

#perform the merge on 2 digits occupation
WM_min_cols <- colnames(NCS_B_merged)
system.time(merge_fail <- merge[missing_obs == length(WM_min_col), ..WM_min_cols])
merge_r2 <- WM_min[merge_fail, on = c("year" = "YEAR", "occupation_code_WM" = "OCC_2d"), ..cols, 
                                      allow.cartesian=TRUE]
merge_r2[,missing_obs := rowSums(is.na(.SD)), .SDcols = WM_min_col]
merge_success2 <- merge_r2[missing_obs < length(WM_min_col)]

#perform the merge on 1 digits occupation
system.time(merge_fail2 <- merge[missing_obs == length(WM_min_col), ..WM_min_cols])
merge_r3 <- WM_min[merge_fail2, on = c("year" = "YEAR", "occupation_code_WM" = "OCC_1d"), ..cols, 
                                      allow.cartesian=TRUE]
merge_r3[,missing_obs := rowSums(is.na(.SD)), .SDcols = WM_min_col]

WM_merge <- rbind(merge_success, merge_success2, merge_r3)

fwrite(WM_merge, "fully_merged.csv")

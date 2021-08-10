# Import libraries
library(data.table)
library(readr)
library(dplyr)
library(haven)
library("doMC")
library(future)
library(vroom)
library(RCurl)
library(zoo)
library(collections)
library(glue)
library(stringr)
library(tidyr)
library(Rfast)
library(parallel)
library(doParallel)
plan(multisession)
# disable scientific notation
options(scipen = 999)
registerDoMC(cores = 100)

setwd("~/scratch-midway2/occupation-productivity/")
registerDoParallel(cores = 27)

#starting this year we have files
'%ni%' <- Negate('%in%')

OEWS <- fread("Merge/OEWS_agg.csv")
OEWS_min <- unique(OEWS[,.SD, .SDcols = c("industry_code_oe", "soc_code", "year", "area_code_oe")])
colnames(OEWS_min) <- c("NAICS", "SOC_CODE", "YEAR", "AREA_CODE")

####################################################################################
#################CREATE MULTIPLE LEVELS OF INDUSTRY GRANULARITY#####################
####################################################################################
#Create Level
OEWS_min[,`LEVEL_OE`:= as.numeric(ifelse(as.numeric(OEWS_min$NAICS) %% 10 != 0, 
                                         6, NA))]
OEWS_min[is.na(`LEVEL_OE`),
         `LEVEL_OE`:= as.numeric(ifelse(as.numeric(OEWS_min[is.na(`LEVEL_OE`), NAICS]) %% 100 != 0, 
                                        5, NA))]
OEWS_min[is.na(`LEVEL_OE`),
         `LEVEL_OE`:= as.numeric(ifelse(as.numeric(OEWS_min[is.na(`LEVEL_OE`), NAICS]) %% 1000 != 0, 
                                        4, NA))]
OEWS_min[is.na(`LEVEL_OE`),
         `LEVEL_OE`:= as.numeric(ifelse(as.numeric(OEWS_min[is.na(`LEVEL_OE`), NAICS]) %% 10000 != 0, 
                                        3, NA))]
OEWS_min[is.na(as.numeric(OEWS_min[,`NAICS`])), `LEVEL_OE` := 2]
OEWS_min[is.na(`LEVEL_OE`), `LEVEL_OE` := 0]

# SIX DIGIT INDUSTRY 
OEWS_min[,`NAICS_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_OE == 6, OEWS_min$NAICS, NA))]
# FIVE DIGIT INDUSTRY
null_6 <- OEWS_min[is.na(NAICS_6d)]
present_6 <- OEWS_min[!is.na(NAICS_6d)]
OEWS_min[is.na(NAICS_6d), `NAICS_5d`:=as.numeric(ifelse(null_6$LEVEL_OE == 5, null_6$NAICS, NA))]
OEWS_min[!is.na(NAICS_6d),`NAICS_5d`:=as.numeric(str_pad(substr(present_6$NAICS_6d, 1, 5), 
                                                         6, side = "right", pad = "0"))]
# FOUR DIGIT INDUSTRY
null_5 <- OEWS_min[is.na(NAICS_5d)]
present_5 <- OEWS_min[!is.na(NAICS_5d)]
OEWS_min[is.na(NAICS_5d), `NAICS_4d`:=as.numeric(ifelse(null_5$LEVEL_OE == 4, null_5$NAICS, NA))]
OEWS_min[!is.na(NAICS_5d),`NAICS_4d`:=as.numeric(str_pad(substr(present_5$NAICS_5d, 1, 4), 
                                                         6, side = "right", pad = "0"))]
# THREE DIGIT INDUSTRY
null_4 <- OEWS_min[is.na(NAICS_4d)]
present_4 <- OEWS_min[!is.na(NAICS_4d)]
OEWS_min[is.na(NAICS_4d), 
         `NAICS_3d`:=as.numeric(ifelse(null_4$LEVEL_OE == 3, 
                                       unlist(lapply(null_4$NAICS, function (x) gsub("A", "0", x))), 
                                       NA))]
OEWS_min[!is.na(NAICS_4d),`NAICS_3d`:=as.numeric(str_pad(substr(present_4$NAICS_4d, 1, 3), 
                                                         6, side = "right", pad = "0"))]
# TWO DIGIT INDUSTRY
null_3 <- OEWS_min[is.na(NAICS_3d)]
present_3 <- OEWS_min[!is.na(NAICS_3d)]
OEWS_min[is.na(NAICS_3d), `NAICS_2d`:=ifelse(null_3$LEVEL_OE == 2, null_3$NAICS, NA)]
OEWS_min[!is.na(NAICS_3d),`NAICS_2d`:=as.numeric(str_pad(substr(present_3$NAICS_3d, 1, 2), 
                                                         6, side = "right", pad = "0"))]

# ONE DIGIT INDUSTRY
OEWS_min[,`NAICS_1d`:=as.numeric(str_pad(substr(OEWS_min$NAICS_2d, 1, 1), 
                                        6, side = "right", pad = "0"))]

# ZERO DIGIT INDUSTRY
OEWS_min[,`NAICS_0d`:=0]

####################################################################################
#################CREATE MULTIPLE LEVELS OF OCCUPATION GRANULARITY###################
####################################################################################

# SIX DIGIT OCCUPATIONS
#ASSIGNING OCCUPATION LEVELS
OEWS_min[OEWS_min$SOC_CODE == 0, `LEVEL_OCC_OE`:= 0]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$SOC_CODE %% 10 ** 4 == 0, `LEVEL_OCC_OE` := 2]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$SOC_CODE %% 10 ** 3 == 0, `LEVEL_OCC_OE` := 3]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$SOC_CODE %% 10 ** 1 == 0, `LEVEL_OCC_OE` := 5]
OEWS_min[is.na(LEVEL_OCC_OE), `LEVEL_OCC_OE` := 6]

OEWS_min[,`OCC_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_OCC_OE == 6, OEWS_min$SOC_CODE, NA))]

# FIVE DIGIT OCCUPATIONS
null_6 <- OEWS_min[is.na(OCC_6d)]
present_6 <- OEWS_min[!is.na(OCC_6d)]
OEWS_min[is.na(OCC_6d), `OCC_5d`:=as.numeric(ifelse(null_6$LEVEL_OCC_OE == 5, null_6$SOC_CODE, NA))]
OEWS_min[!is.na(OCC_6d),`OCC_5d`:=as.numeric(str_pad(substr(present_6$OCC_6d, 1, 5), 
                                                     6, side = "right", pad = "0"))]
# THREE DIGIT OCCUPATIONS
null_4 <- OEWS_min[is.na(OCC_5d)]
present_4 <- OEWS_min[!is.na(OCC_5d)]
OEWS_min[is.na(OCC_5d), `OCC_3d`:=as.numeric(ifelse(null_4$LEVEL_OCC_OE == 3, null_4$SOC_CODE, NA))]
OEWS_min[!is.na(OCC_5d),`OCC_3d`:=as.numeric(str_pad(substr(present_4$OCC_5d, 1, 3), 
                                                     6, side = "right", pad = "0"))]

# TWO DIGIT OCCUPATIONS
null_3 <- OEWS_min[is.na(OCC_3d)]
present_3 <- OEWS_min[!is.na(OCC_3d)]
OEWS_min[is.na(OCC_3d), `OCC_2d`:=as.numeric(ifelse(null_3$LEVEL_OCC_OE == 2, null_3$SOC_CODE, NA))]
OEWS_min[!is.na(OCC_3d),`OCC_2d`:=as.numeric(str_pad(substr(present_3$OCC_3d, 1, 2), 
                                                     6, side = "right", pad = "0"))]

#ZERO DIGIT OCCUPATIONS 
OEWS_min[,`OCC_0d` := 0]

####################################################################################
########################Merging Labor Productivity Data#############################
####################################################################################
LP <- fread("Merge/LP_agg.csv")
LP_min <- unique(LP[,.SD, .SDcols = c("YEAR", "NAICS_IP", "AREA_CODE_IP")])
LP_min[,`AREA_CODE_IP` := 10 * `AREA_CODE_IP`]

OEWS_min$NAICS_6d <- as.character(OEWS_min$NAICS_6d)

#perform the merge on 6 digits
OEWS_min[,'merge_string':=paste(`YEAR`, `NAICS_6d`, `AREA_CODE`, sep = "_")]
LP_min[,'merge_string':=paste(`YEAR`, `NAICS_IP`, `AREA_CODE_IP`, sep = "_")]
cols <- unique(c(colnames(LP_min), colnames(OEWS_min)))
merge <- LP_min[OEWS_min, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 5 digits
matched <- unique(merge$merge_string)
OEWS_min_cols <- colnames(OEWS_min)
merge_fail <- OEWS_min[`merge_string` %ni% matched,.SD, .SDcols = OEWS_min_cols]
merge_fail[,'merge_string':=paste(`YEAR`, `NAICS_5d`, `AREA_CODE`, sep = "_")]
merge_r2 <- LP_min[merge_fail, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 4 digits
matched <- unique(merge_r2$merge_string)
merge_fail2 <- merge_fail[`merge_string` %ni% matched,.SD, .SDcols = OEWS_min_cols]
merge_fail2[,'merge_string':=paste(`YEAR`, `NAICS_4d`, `AREA_CODE`, sep = "_")]
merge_r3 <- LP_min[merge_fail2, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 3 digits
matched <- unique(merge_r3$merge_string)
merge_fail3 <- merge_fail2[`merge_string` %ni% matched,.SD, .SDcols = OEWS_min_cols]
merge_fail3[,'merge_string':=paste(`YEAR`, `NAICS_3d`, `AREA_CODE`, sep = "_")]
merge_r4 <- LP_min[merge_fail3, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 2 digits
matched <- unique(merge_r4$merge_string)
merge_fail4 <- merge_fail3[`merge_string` %ni% matched,.SD, .SDcols = OEWS_min_cols]
merge_fail4[,'merge_string':=paste(`YEAR`, `NAICS_2d`, `AREA_CODE`, sep = "_")]
merge_r5 <- LP_min[merge_fail4, on = 'merge_string', ..cols]

LP_merged <- rbind(merge, merge_r2, merge_r3, merge_r4, merge_r5)

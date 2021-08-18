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
# disable scientific notation
options(scipen = 999)

setwd("~/Google Drive/Non-Academic Work/Research/Traina/occupation-productivity-2/")

#starting this year we have files
'%ni%' <- Negate('%in%')

#function to check whether our column selection creates a unique key for the file
unique_id <- function(x, ...) {
  id_set <- x %>% select(...)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>% 
      filter(id_set %>% duplicated()) %>% 
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange(...)
    )
  }
}

OEWS <- fread("Merge/OEWS_agg.csv")
#checking that this is a unique id
OEWS %>% unique_id("NAICS", "OCC_CODE", "YEAR", "AREA_CODE")

OEWS_min <- unique(OEWS[,.SD, .SDcols = c("NAICS", "OCC_CODE", "YEAR", "AREA_CODE", "LEVEL_NAICS_OE")])

# SIX DIGIT INDUSTRY 
OEWS_min[,`NAICS_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_NAICS_OE == 6, OEWS_min$NAICS, NA))]
# FIVE DIGIT INDUSTRY
null_6 <- OEWS_min[is.na(NAICS_6d)]
present_6 <- OEWS_min[!is.na(NAICS_6d)]
OEWS_min[is.na(NAICS_6d), `NAICS_5d`:=as.numeric(ifelse(null_6$LEVEL_NAICS_OE == 5, null_6$NAICS, NA))]
OEWS_min[!is.na(NAICS_6d),`NAICS_5d`:=as.numeric(str_pad(substr(present_6$NAICS_6d, 1, 5), 
                                                         6, side = "right", pad = "0"))]
# FOUR DIGIT INDUSTRY
null_5 <- OEWS_min[is.na(NAICS_5d)]
present_5 <- OEWS_min[!is.na(NAICS_5d)]
OEWS_min[is.na(NAICS_5d), `NAICS_4d`:=ifelse(null_5$LEVEL_NAICS_OE == 4, null_5$NAICS, NA)]
OEWS_min[!is.na(NAICS_5d),`NAICS_4d`:=str_pad(substr(present_5$NAICS_5d, 1, 4), 
                                              6, side = "right", pad = "0")]
# THREE DIGIT INDUSTRY
null_4 <- OEWS_min[is.na(NAICS_4d)]
present_4 <- OEWS_min[!is.na(NAICS_4d)]
OEWS_min[is.na(NAICS_4d), `NAICS_3d`:=as.numeric(ifelse(null_4$LEVEL_NAICS_OE == 3, null_4$NAICS, NA))]
OEWS_min[!is.na(NAICS_4d),`NAICS_3d`:=as.numeric(str_pad(substr(present_4$NAICS_4d, 1, 3), 
                                                         6, side = "right", pad = "0"))]
# TWO DIGIT INDUSTRY
null_3 <- OEWS_min[is.na(NAICS_3d)]
present_3 <- OEWS_min[!is.na(NAICS_3d)]
OEWS_min[is.na(NAICS_3d), `NAICS_2d`:=ifelse(null_3$LEVEL_NAICS_OE == 2, null_3$NAICS, NA)]
OEWS_min[!is.na(NAICS_3d),`NAICS_2d`:=as.numeric(str_pad(substr(present_3$NAICS_3d, 1, 2), 
                                                         6, side = "right", pad = "0"))]

# ONE DIGIT INDUSTRY
OEWS_min[,`NAICS_1d`:=as.numeric(str_pad(substr(OEWS_min$NAICS_2d, 1, 1), 
                                         6, side = "right", pad = "0"))]

# ZERO DIGIT INDUSTRY
OEWS_min[,`NAICS_0d`:="000000"]

####################################################################################
#################CREATE MULTIPLE LEVELS OF OCCUPATION GRANULARITY###################
####################################################################################

# SIX DIGIT OCCUPATIONS
#ASSIGNING OCCUPATION LEVELS
OEWS_min[OEWS_min$OCC_CODE == 0, `LEVEL_OCC_OE`:= 0]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 4 == 0, `LEVEL_OCC_OE` := 2]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 3 == 0, `LEVEL_OCC_OE` := 3]
OEWS_min[is.na(LEVEL_OCC_OE) & OEWS_min$OCC_CODE %% 10 ** 1 == 0, `LEVEL_OCC_OE` := 5]
OEWS_min[is.na(LEVEL_OCC_OE), `LEVEL_OCC_OE` := 6]

OEWS_min[,`OCC_6d`:=as.numeric(ifelse(OEWS_min$LEVEL_OCC_OE == 6, OEWS_min$OCC_CODE, NA))]

# FIVE DIGIT OCCUPATIONS
null_6 <- OEWS_min[is.na(OCC_6d)]
present_6 <- OEWS_min[!is.na(OCC_6d)]
OEWS_min[is.na(OCC_6d), `OCC_5d`:=as.numeric(ifelse(null_6$LEVEL_OCC_OE == 5, null_6$OCC_CODE, NA))]
OEWS_min[!is.na(OCC_6d),`OCC_5d`:=as.numeric(str_pad(substr(present_6$OCC_6d, 1, 5), 
                                                     6, side = "right", pad = "0"))]
# THREE DIGIT OCCUPATIONS
null_4 <- OEWS_min[is.na(OCC_5d)]
present_4 <- OEWS_min[!is.na(OCC_5d)]
OEWS_min[is.na(OCC_5d), `OCC_3d`:=as.numeric(ifelse(null_4$LEVEL_OCC_OE == 3, null_4$OCC_CODE, NA))]
OEWS_min[!is.na(OCC_5d),`OCC_3d`:=as.numeric(str_pad(substr(present_4$OCC_5d, 1, 3), 
                                                     6, side = "right", pad = "0"))]
# TWO DIGIT OCCUPATIONS
null_3 <- OEWS_min[is.na(OCC_3d)]
present_3 <- OEWS_min[!is.na(OCC_3d)]
OEWS_min[is.na(OCC_3d), `OCC_2d`:=as.numeric(ifelse(null_3$LEVEL_OCC_OE == 2, null_3$OCC_CODE, NA))]
OEWS_min[!is.na(OCC_3d),`OCC_2d`:=as.numeric(str_pad(substr(present_3$OCC_3d, 1, 2), 
                                                     6, side = "right", pad = "0"))]
#ZERO DIGIT OCCUPATIONS 
OEWS_min[`OCC_CODE`== 0,`OCC_0d` := 0]

####################################################################################
######################## Merging Labor Productivity Data ###########################
####################################################################################
LP <- fread("Merge/LP_agg.csv")
LP %>% unique_id("YEAR_IP", "NAICS_IP", "AREA_CODE_IP")

LP_min <- unique(LP[,.SD, .SDcols = c("YEAR_IP", "NAICS_IP", "AREA_CODE_IP")])

#perform the merge on 6 digits
OEWS_min[,'merge_string':=paste(`YEAR`, `NAICS_6d`, `AREA_CODE`, sep = "_")]
LP_min[,'merge_string':=paste(`YEAR_IP`, `NAICS_IP`, `AREA_CODE_IP`, sep = "_")]
cols <- unique(c(colnames(LP_min), colnames(OEWS_min)))
merge_r1 <- LP_min[OEWS_min, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 5 digits
matched <- unique(merge_r1$merge_string)
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
merge_r5 <- LP_min[merge_fail4, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 digits
matched <- unique(merge_r5$merge_string)
merge_fail5 <- merge_fail4[`merge_string` %ni% matched,.SD, .SDcols = OEWS_min_cols]
merge_fail5[,'merge_string':=paste(`YEAR`, `NAICS_0d`, `AREA_CODE`, sep = "_")]
merge_r6 <- LP_min[merge_fail5, on = 'merge_string', ..cols]

LP_merged <- rbind(merge_r1, merge_r2, merge_r3, merge_r4, merge_r5, merge_r6)

print(paste("merge rate was", 1-mean(is.na(LP_merged$YEAR_IP))))

# Generate the NAICS Level for BLS codes
LP_merged[as.numeric(NAICS_IP) %% 10 != 0, `LEVEL_NAICS_IP` := 6]
LP_merged[as.numeric(NAICS_IP) %% 100 != 0 & is.na(`LEVEL_NAICS_IP`), `LEVEL_NAICS_IP` := 5]
LP_merged[as.numeric(NAICS_IP) %% 1000 != 0 & is.na(`LEVEL_NAICS_IP`), `LEVEL_NAICS_IP` := 4]
LP_merged[as.numeric(NAICS_IP) %% 10000 != 0 & is.na(`LEVEL_NAICS_IP`), `LEVEL_NAICS_IP` := 3]
LP_merged[NAICS_IP %in% c("44-45"), `LEVEL_NAICS_IP` := 2]
LP_merged[as.numeric(NAICS_IP) %% 100000 != 0 & is.na(`LEVEL_NAICS_IP`), `LEVEL_NAICS_IP` := 2]
LP_merged[`NAICS_IP` == "000000", `LEVEL_NAICS_IP`:=0]

# Reformat Merge Table
LP_merged <- LP_merged[,c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE", "LEVEL_NAICS_OE", "LEVEL_OCC_OE", 
                          "YEAR_IP", "NAICS_IP", "AREA_CODE_IP", "LEVEL_NAICS_IP",
                          "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d", "NAICS_1d", "NAICS_0d", 
                          "OCC_6d", "OCC_5d", "OCC_3d", "OCC_2d", "OCC_0d"), with = FALSE]

####################################################################################
#################### Merging Occupational Requirements Data ########################
####################################################################################
#has no year
ORS <- fread("Merge/ORS_agg.csv")
ORS_min <- unique(ORS[,.SD, .SDcols = c("SOC_2018_CODE_OR")])

ORS %>% unique_id(`SOC_2018_CODE_OR`)

#perform the merge on 6 occupation digits
LP_merged[,'merge_string':=`OCC_6d`]
ORS_min[,'merge_string':=`SOC_2018_CODE_OR`]
cols <- unique(c(colnames(ORS_min), colnames(LP_merged)))
merge_r1 <- ORS_min[LP_merged, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 5 occupation digits
matched <- unique(merge_r1$merge_string)
LP_merged_cols <- colnames(LP_merged)
merge_fail <- LP_merged[`merge_string` %ni% matched,.SD, .SDcols = LP_merged_cols]
merge_fail[,'merge_string':=`OCC_5d`]
merge_r2 <- ORS_min[merge_fail, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 3 occupation digits
matched <- unique(merge_r2$merge_string)
LP_merged_cols <- colnames(LP_merged)
merge_fail2 <- merge_fail[`merge_string` %ni% matched,.SD, .SDcols = LP_merged_cols]
merge_fail2[,'merge_string':=`OCC_3d`]
merge_r3 <- ORS_min[merge_fail2, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 2 occupation digits
matched <- unique(merge_r3$merge_string)
LP_merged_cols <- colnames(LP_merged)
merge_fail3 <- merge_fail2[`merge_string` %ni% matched,.SD, .SDcols = LP_merged_cols]
merge_fail3[,'merge_string':=`OCC_2d`]
merge_r4 <- ORS_min[merge_fail3, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 industry digits
matched <- unique(merge_r4$merge_string)
LP_merged_cols <- colnames(LP_merged)
merge_fail4 <- merge_fail3[`merge_string` %ni% matched,.SD, .SDcols = LP_merged_cols]
merge_fail4[,'merge_string':=`OCC_0d`]
merge_r5 <- ORS_min[merge_fail4, on = 'merge_string', ..cols]

ORS_merged <- rbind(merge_r1, merge_r2, merge_r3, merge_r4, merge_r5)

print(paste("merge rate was", 1-mean(is.na(ORS_merged$SOC_2018_CODE_OR))))

# Generate the SOC Level for OCC codes
ORS_merged[as.numeric(SOC_2018_CODE_OR) %% 10 != 0, `LEVEL_OCC_OR` := 6]
ORS_merged[as.numeric(SOC_2018_CODE_OR) %% 100 != 0 & is.na(`LEVEL_OCC_OR`), `LEVEL_OCC_OR` := 5]
ORS_merged[as.numeric(SOC_2018_CODE_OR) %% 10000 != 0 & is.na(`LEVEL_OCC_OR`), `LEVEL_OCC_OR` := 3]
ORS_merged[as.numeric(SOC_2018_CODE_OR) %% 100000 != 0 & is.na(`LEVEL_OCC_OR`), `LEVEL_OCC_OR` := 2]
ORS_merged[is.na(`LEVEL_OCC_OR`), `LEVEL_OCC_OR`:= 0]

# Reformat Merge Table
ORS_merged <- ORS_merged[,c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE", "LEVEL_NAICS_OE", "LEVEL_OCC_OE", 
                            "YEAR_IP", "NAICS_IP", "AREA_CODE_IP", "LEVEL_NAICS_IP",
                            "SOC_2018_CODE_OR", "LEVEL_OCC_OR",
                            "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d", "NAICS_1d", "NAICS_0d", 
                            "OCC_6d", "OCC_5d", "OCC_3d", "OCC_2d", "OCC_0d"), with = FALSE]


####################################################################################
######################### Modelled Wage Estimates Data #############################
####################################################################################

WM <- fread("Merge/WM_agg.csv")

WM_min <- WM[,c("OCCUPATION_CODE_WM", "YEAR_WM", "AREA_CODE_WM"), with = FALSE]

WM %>% unique_id("OCCUPATION_CODE_WM", "YEAR_WM", "AREA_CODE_WM")

#perform the merge on 6 occupation digits
ORS_merged[,'merge_string':=paste(`OCC_6d`, `YEAR`, `AREA_CODE`, sep = "_")]
WM_min[,'merge_string':=paste(`OCCUPATION_CODE_WM`, `YEAR_WM`, `AREA_CODE_WM`, sep = "_")]
cols <- unique(c(colnames(WM_min), colnames(ORS_merged)))
merge_r1 <- WM_min[ORS_merged, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 5 occupation digits
matched <- unique(merge_r1$merge_string)
ORS_merged_cols <- colnames(ORS_merged)
merge_fail <- ORS_merged[`merge_string` %ni% matched,.SD, .SDcols = ORS_merged_cols]
merge_fail[,'merge_string':=paste(`OCC_5d`, `YEAR`, `AREA_CODE`, sep = "_")]
merge_r2 <- WM_min[merge_fail, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 3 occupation digits
matched <- unique(merge_r2$merge_string)
ORS_merged_cols <- colnames(ORS_merged)
merge_fail2 <- merge_fail[`merge_string` %ni% matched,.SD, .SDcols = ORS_merged_cols]
merge_fail2[,'merge_string':=paste(`OCC_3d`, `YEAR`, `AREA_CODE`, sep = "_")]
merge_r3 <- WM_min[merge_fail2, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 2 occupation digits
matched <- unique(merge_r3$merge_string)
ORS_merged_cols <- colnames(ORS_merged)
merge_fail3 <- merge_fail2[`merge_string` %ni% matched,.SD, .SDcols = ORS_merged_cols]
merge_fail3[,'merge_string':=paste(`OCC_2d`, `YEAR`, `AREA_CODE`, sep = "_")]
merge_r4 <- WM_min[merge_fail3, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 occupation digits
matched <- unique(merge_r4$merge_string)
ORS_merged_cols <- colnames(ORS_merged)
merge_fail4 <- merge_fail3[`merge_string` %ni% matched,.SD, .SDcols = ORS_merged_cols]
merge_fail4[,'merge_string':=paste(`OCC_0d`, `YEAR`, `AREA_CODE`, sep = "_")]
merge_r5 <- WM_min[merge_fail4, on = 'merge_string', ..cols]

WM_merged <- rbind(merge_r1, merge_r2, merge_r3, merge_r4, merge_r5)

print(paste("merge rate was", 1-mean(is.na(WM_merged$YEAR_WM))))

# Generate the OCC Level for OCC codes
WM_merged[as.numeric(`OCCUPATION_CODE_WM`) %% 10 != 0, `LEVEL_OCC_WM` := 6]
WM_merged[as.numeric(`OCCUPATION_CODE_WM`) %% 100 != 0 & is.na(`LEVEL_OCC_WM`), `LEVEL_OCC_WM` := 5]
WM_merged[as.numeric(`OCCUPATION_CODE_WM`) %% 10000 != 0 & is.na(`LEVEL_OCC_WM`), `LEVEL_OCC_WM` := 3]
WM_merged[as.numeric(`OCCUPATION_CODE_WM`) %% 100000 != 0 & is.na(`LEVEL_OCC_WM`), `LEVEL_OCC_WM` := 2]

# Reformat Merge Table
WM_merged <- WM_merged[,c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE", "LEVEL_NAICS_OE", "LEVEL_OCC_OE", 
                          "YEAR_IP", "NAICS_IP", "AREA_CODE_IP", "LEVEL_NAICS_IP",
                          "SOC_2018_CODE_OR", "LEVEL_OCC_OR",
                          "YEAR_WM", "OCCUPATION_CODE_WM", "AREA_CODE_WM", "LEVEL_OCC_WM",
                          "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d", "NAICS_1d", "NAICS_0d", 
                          "OCC_6d", "OCC_5d", "OCC_3d", "OCC_2d", "OCC_0d"), with = FALSE]


####################################################################################
###################### Employer Cost of Employment Data ############################
####################################################################################

EC <- fread("Merge/EC_agg.csv")

EC %>% unique_id(`OCCUPATION_CODE_CM`, `YEAR_CM`, `INDUSTRY_CODE_CM`, `AREA_CODE_CM`)
EC_min <- EC[,c("OCCUPATION_CODE_CM", "YEAR_CM", "INDUSTRY_CODE_CM", "AREA_CODE_CM"), with = FALSE]

#perform the merge on 6 OCC digits, 0 digit industry (higher granularity matches revealed nothing)
WM_merged[,'merge_string':=paste(`OCC_6d`, `YEAR`, `NAICS_0d`, `AREA_CODE`, sep = "_")]
EC_min[,'merge_string':=paste(`OCCUPATION_CODE_CM`, `YEAR_CM`, `INDUSTRY_CODE_CM`, `AREA_CODE_CM`, sep = "_")]
cols <- unique(c(colnames(EC_min), colnames(WM_merged)))
merge_r1 <- EC_min[WM_merged, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 3 OCC digits, 0 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r1$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail1 <- WM_merged[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail1[,'merge_string':=paste(`OCC_3d`, `YEAR`, `NAICS_0d`,`AREA_CODE`, sep = "_")]
merge_r2 <- EC_min[merge_fail1, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 2 OCC digits, 0 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r2$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail2 <- merge_fail1[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail2[,'merge_string':=paste(`OCC_2d`, `YEAR`, `NAICS_0d`,`AREA_CODE`, sep = "_")]
merge_r3 <- EC_min[merge_fail2, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 OCC digits, 4 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r3$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail3 <- merge_fail2[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail3[,'merge_string':=paste(`OCC_0d`, `YEAR`, `NAICS_4d`,`AREA_CODE`, sep = "_")]
merge_r4 <- EC_min[merge_fail3, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 OCC digits, 3 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r4$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail4 <- merge_fail3[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail4[,'merge_string':=paste(`OCC_0d`, `YEAR`, `NAICS_3d`,`AREA_CODE`, sep = "_")]
merge_r5 <- EC_min[merge_fail4, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 OCC digits, 2 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r5$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail5 <- merge_fail4[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail5[,'merge_string':=paste(`OCC_0d`, `YEAR`, `NAICS_2d`,`AREA_CODE`, sep = "_")]
merge_r6 <- EC_min[merge_fail5, on = 'merge_string', ..cols, nomatch = 0]

#perform the merge on 0 OCC digits, 1 digit industry (higher granularity industry/occ matches revealed nothing)
matched <- unique(merge_r6$merge_string)
WM_merged_cols <- colnames(WM_merged)
merge_fail6 <- merge_fail5[`merge_string` %ni% matched,.SD, .SDcols = WM_merged_cols]
merge_fail6[,'merge_string':=paste(`OCC_0d`, `YEAR`, `NAICS_0d`,`AREA_CODE`, sep = "_")]
merge_r7 <- EC_min[merge_fail6, on = 'merge_string', ..cols]

EC_merged <- rbind(merge_r1, merge_r2, merge_r3, merge_r4, merge_r5, merge_r6, merge_r7)

print(paste("merge rate was", 1-mean(is.na(EC_merged$YEAR_CM))))

# Generate the OCC Level for OCC codes
EC_merged[as.numeric(`OCCUPATION_CODE_CM`) %% 10 != 0, `LEVEL_OCC_CM` := 6]
EC_merged[as.numeric(`OCCUPATION_CODE_CM`) %% 100 != 0 & is.na(`LEVEL_OCC_CM`), `LEVEL_OCC_CM` := 5]
EC_merged[as.numeric(`OCCUPATION_CODE_CM`) %% 10000 != 0 & is.na(`LEVEL_OCC_CM`), `LEVEL_OCC_CM` := 3]
EC_merged[as.numeric(`OCCUPATION_CODE_CM`) %% 100000 != 0 & is.na(`LEVEL_OCC_CM`), `LEVEL_OCC_CM` := 2]
EC_merged[`OCCUPATION_CODE_CM` == 0, `LEVEL_OCC_CM`:=0]

# Generate the NAICS Level for BLS codes
EC_merged[as.numeric(INDUSTRY_CODE_CM) %% 10 != 0, `LEVEL_NAICS_CM` := 6]
EC_merged[as.numeric(INDUSTRY_CODE_CM) %% 100 != 0 & is.na(`LEVEL_NAICS_CM`), `LEVEL_NAICS_CM` := 5]
EC_merged[as.numeric(INDUSTRY_CODE_CM) %% 1000 != 0 & is.na(`LEVEL_NAICS_CM`), `LEVEL_NAICS_CM` := 4]
EC_merged[as.numeric(INDUSTRY_CODE_CM) %% 10000 != 0 & is.na(`LEVEL_NAICS_CM`), `LEVEL_NAICS_CM` := 3]
EC_merged[as.numeric(INDUSTRY_CODE_CM) %% 100000 != 0 & is.na(`LEVEL_NAICS_CM`), `LEVEL_NAICS_CM` := 2]
EC_merged[`INDUSTRY_CODE_CM` == "000000", `LEVEL_NAICS_CM`:=0]

# Reformat Merge Table
EC_merged <- EC_merged[,c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE", "LEVEL_NAICS_OE", "LEVEL_OCC_OE", 
                          "YEAR_IP", "NAICS_IP", "AREA_CODE_IP", "LEVEL_NAICS_IP",
                          "SOC_2018_CODE_OR", "LEVEL_OCC_OR",
                          "YEAR_WM", "OCCUPATION_CODE_WM", "AREA_CODE_WM","LEVEL_OCC_WM",
                          "OCCUPATION_CODE_CM", "YEAR_CM", "INDUSTRY_CODE_CM", "AREA_CODE_CM", "LEVEL_NAICS_CM", "LEVEL_OCC_CM",
                          "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d", "NAICS_1d", "NAICS_0d", 
                          "OCC_6d", "OCC_5d", "OCC_3d", "OCC_2d", "OCC_0d"), with = FALSE]

fwrite(EC_merged, "merge_table.csv")
saveRDS(EC_merged, "merge_table.rds")

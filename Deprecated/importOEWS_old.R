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

setwd("~/Google Drive/Non-Academic Work/Research/Traina/occupation-productivity-2/")
#starting this year we have files
'%ni%' <- Negate('%in%')

#Importing OEWS data
OEWS_agg <- data.table()
for (i in 2002:2020) {
  for (file in dir(paste("Data/Unused/OEWS_old/", i, sep = ""))) {
    if (!(grepl("descriptions", file)) && !(grepl("owner", file))) {
      data <- data.table(read_excel(paste("Data/Unused/OEWS_old/", i, "/", file, sep = "")))
      colnames(data) <- toupper(colnames(data))
      colnames(data)[which(colnames(data) == "OCC_GROUP")] <- "GROUP"
      colnames(data)[which(colnames(data) == "O_GROUP")] <- "GROUP"
      colnames(data)[which(colnames(data) == "O_GROUP")] <- "GROUP"
      
      if ("GROUP" %ni% colnames(data)) {
        data$GROUP <- ifelse(as.numeric(gsub("-", "", data$OCC_CODE)) %% 10000 == 0, "major", NA)
        data[as.numeric(gsub("-", "", data$OCC_CODE)) == 0]$GROUP <- "TOTAL"
      }
      if ("HOURLY" %ni% colnames(data)) {
        data$HOURLY <- NA
      }
      if ("PCT_RPT" %ni% colnames(data)) {
        data$PCT_RPT <- NA
      }
      for (col in c("OWNERSHIP", "NAICS_LEVEL", "AREA",
                    "AREA_TITLE", "AREA_TYPE", "I_GROUP",
                    "OWN_CODE", "JOBS_1000", "LOC_QUOTIENT", "PRIM_STATE")) {
        if (col %in% colnames(data)) {
          data <- data %>% select(-col)
        }
      }
      level <- as.numeric(substr(file, 4, 4))
      if (level %ni% c(3, 4, 5)) {
        level = 2
      }
      data$YEAR <- i
      data$LEVEL <- level
      print(i)
      OEWS_agg <- rbind(OEWS_agg, data)
    }
  } 
}

OEWS_agg$GROUP <- tolower(OEWS_agg$GROUP)
colnames(OEWS_agg) <- paste(colnames(OEWS_agg), "OE", sep = "_")
colnames(OEWS_agg)[1] <- "NAICS"
colnames(OEWS_agg)[3] <- "OCC_CODE"
colnames(OEWS_agg)[25] <- "YEAR"
OEWS_agg[str_length(OEWS_agg$NAICS) == 2]$NAICS <- str_pad(OEWS_agg[str_length(OEWS_agg$NAICS) == 2]$NAICS, 6, "right", "0")
OEWS_agg[,`LEVEL_OE`:=ifelse(as.numeric(OEWS_agg$NAICS) %% 10 == 0, OEWS_agg$LEVEL_OE, 6)]

OEWS_agg[is.na(LEVEL_OE) & str_length(NAICS) == 6]$LEVEL_OE = 3
OEWS_agg[is.na(LEVEL_OE) & str_length(NAICS) == 5]$LEVEL_OE = 2

OEWS_desc <- read_excel("Data/Unused/OEWS_old/2018/field_descriptions.xlsx", skip = 8)
OEWS_desc$Field <- toupper(OEWS_desc$Field)
OEWS_desc <- OEWS_desc[OEWS_desc$Field %in% colnames(OEWS_agg),]
OEWS_desc <- rbind(OEWS_desc,c("LEVEL", "Number of NAICS digits"))
OEWS_desc$Field <- paste(OEWS_desc$Field, "OE", sep = "_")
OEWS_desc$Field[1] <- "NAICS"
OEWS_desc$Field[2] <- "OCC_CODE"


fwrite(OEWS_agg, "Merge/OEWS_agg_year.csv")
fwrite(OEWS_desc, "Merge/OEWS_desc_year.csv")

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
for (i in 2002:2010) {
  for (file in dir(paste("Data/OEWS/", i, sep = ""))) {
    if (!(grepl("descriptions", file)) && !(grepl("owner", file))) {
      data <- data.table(read_excel(paste("Data/OEWS/", i, "/", file, sep = "")))
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
OEWS_agg[str_length(OEWS_agg$NAICS) == 2]$NAICS <- str_pad(OEWS_agg[str_length(OEWS_agg$NAICS) == 2]$NAICS, 6, "right", "0")
OEWS_agg[,`LEVEL`:=ifelse(as.numeric(OEWS_agg$NAICS) %% 10 == 0, OEWS_agg$LEVEL, 6)]

OEWS_agg[is.na(LEVEL) & str_length(NAICS) == 6]$LEVEL = 3
OEWS_agg[is.na(LEVEL) & str_length(NAICS) == 5]$LEVEL = 2

OEWS_agg2 <- data.table()
col_name <- c("AREA", "AREA_TITLE", "AREA_TYPE", "NAICS", "NAICS_TITLE", "OWN_CODE", "OCC_CODE",
              "OCC_TITLE", "O_GROUP", "TOT_EMP", "EMP_PRSE", "JOBS_1000",
              "LOC_QUOTIENT", "PCT_TOTAL", "H_MEAN", "A_MEAN", "MEAN_PRSE", "H_PCT10",
              "H_PCT25", "H_MEDIAN", "H_PCT75", "H_PCT90", "A_PCT10", "A_PCT25", "A_MEDIAN",
              "A_PCT75",	"A_PCT90", "ANNUAL", "HOURLY", "YEAR")
for (file in dir("Data/OEWS/")[is.na(as.numeric(dir("Data/OEWS/")))]) {
  if (file != "original_data" & !grepl("zip", file)) {
    print(file)
    data <- fread(paste("Data/OEWS/", file, sep = ""))
    colnames(data) <- toupper(colnames(data))
    data <- data[,-c("PRIM_STATE", "I_GROUP", "V30", "V31"), with = FALSE]
    data$YEAR <- as.numeric(str_sub(file, length(file)-9, length(file)-6))
    colnames(data) <- col_name
    print(as.numeric(str_sub(file, length(file)-9, length(file)-6)))
    OEWS_agg2 <- rbind(OEWS_agg2, data)
  }
}
OEWS_agg2[, `TOT_EMP`:= as.numeric(gsub(",", "", TOT_EMP))]
OEWS_agg2[NAICS == "48-490", `NAICS`:="48-49"]
OEWS_agg2[NAICS %ni% c("44-45", "31-33", "48-49"), `NAICS`:=str_pad(NAICS, 6, side = "right", pad = "0")]
OEWS_agg2[as.numeric(NAICS) %% 10 != 0, `LEVEL` := 6]
OEWS_agg2[as.numeric(NAICS) %% 100 != 0 & is.na(`LEVEL`), `LEVEL` := 5]
OEWS_agg2[as.numeric(NAICS) %% 1000 != 0 & is.na(`LEVEL`), `LEVEL` := 4]
OEWS_agg2[as.numeric(NAICS) %% 10000 != 0 & is.na(`LEVEL`), `LEVEL` := 3]
OEWS_agg2[NAICS %in% c("44-45", "31-33",  "48-49"), `LEVEL` := 2]
OEWS_agg2[as.numeric(NAICS) %% 100000 != 0 & is.na(`LEVEL`), `LEVEL` := 2]
OEWS_agg2[NAICS == '000000', `LEVEL` := 0]
OEWS_agg2[is.na(`LEVEL`), `LEVEL`:= 4]
OEWS_agg2 <- OEWS_agg2[OWN_CODE == 5]
OEWS_agg_final <- rbind(OEWS_agg2, OEWS_agg, fill = TRUE)

OEWS_agg_final[is.na(AREA), AREA := 99]
OEWS_agg_final <- OEWS_agg_final[NAICS != "000001"]
OEWS_agg_final <- unique(OEWS_agg_final[,-c("O_GROUP"), with = FALSE])
  
colnames(OEWS_agg_final)[1] <- "AREA_CODE"
colnames(OEWS_agg_final)[30] <- "LEVEL_NAICS_OE"
OEWS_agg_final[,`OCC_CODE` := as.numeric(gsub("-", "", `OCC_CODE`))]

fwrite(OEWS_agg_final,"Merge/OEWS_agg.csv")

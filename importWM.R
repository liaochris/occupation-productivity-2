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

#reading in wage model files from 2014-2020
wage_model <- data.table()
for (year in 2014:2020) {
  separator <- ifelse(year < 2017, "_", "-")
  filename <- paste("Data/Wage_Model/mwe",separator,year,"complete.xlsx", sep = "")
  data <- data.table(read_excel(filename, sheet = 2))
  data$year <- year
  colnames(data) <- toupper(colnames(data))
  colnames(data) <- unlist(lapply(colnames(data), function (x) gsub("\r\n", " ", x)))
  colnames(data) <- unlist(lapply(colnames(data), function (x) gsub(" \\(\\$\\)", "", x)))
  colnames(data) <- unlist(lapply(colnames(data), function (x) gsub("  ", " ", x)))
  colnames(data)[which(colnames(data) == "AREAT TEXT")] <- "AREA TEXT"
  print(year)
  wage_model <- rbind(data, wage_model, fill = TRUE)
}
wage_model$MEASURE <- "Average Hourly Wage"

#generating wide tables for labor productivity data to make merging easier
WM_tbl1 <- dcast(wage_model, `OCCUPATION CODE` + `YEAR` + `OCCUPATION TEXT` + `AREA CODE` + 
                   `AREA TEXT` + `WORK LEVEL CODE` + `WORK LEVEL TEXT` + 
                   `JOB CHARACTERISTIC CODE` + `JOB CHARACTERISTIC TEXT` ~ `MEASURE`, 
                 value.var = c("AVERAGE HOURLY WAGE"))
colnames(WM_tbl1) <- paste(colnames(WM_tbl1), "WM", sep = "_")
#colnames(WM_tbl1)[1] <- "soc_code"

WM_tbl1 <- WM_tbl1[`WORK LEVEL CODE_WM` == "00"]

WM_tbl1 <- WM_tbl1[`JOB CHARACTERISTIC CODE_WM` == "25"]

WM_tbl1$`AREA CODE_WM` <- as.numeric(WM_tbl1$`AREA CODE_WM`)
WM_tbl1[`AREA CODE_WM` == 0, `AREA CODE_WM`:=99]
WM_tbl1[`AREA CODE_WM` %% 10000== 0,`AREA CODE_WM` := `AREA CODE_WM`/100000]

colnames(WM_tbl1)[which(colnames(WM_tbl1) == "OCCUPATION CODE_WM")] <- "OCCUPATION_CODE_WM"
colnames(WM_tbl1)[which(colnames(WM_tbl1) == "AREA CODE_WM")] <- "AREA_CODE_WM"

cols <- c("MEASURE")
WM_desc <- unique(wage_model[,..cols])
WM_desc$MEASURE <- paste(WM_desc$MEASURE, "WM", sep = "_")

fwrite(WM_tbl1, "Merge/WM_agg.csv")
fwrite(WM_desc, "Merge/WM_desc.csv")



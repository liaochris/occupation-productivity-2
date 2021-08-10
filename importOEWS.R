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

setwd("~/scratch-midway2/aoccupation-productivity-2/")
#starting this year we have files
'%ni%' <- Negate('%in%')


# reading in the various occupational requirement data files - see ip.txt for further information
area <- read_delim("Data/OEWS/oe.area.txt", delim = "\t")
area <- data.table(area)
datatype <- read_delim("Data/OEWS/oe.datatype.txt", delim = "\t") 
datatype <- data.table(datatype)
footnote <- read_delim("Data/OEWS/oe.footnote.txt", delim = "\t") 
footnote <- data.table(footnote)
industry <- read_delim("Data/OEWS/oe.industry.txt", delim = "\t") %>%
  select(c(`industry_code`, `industry_name`))
industry <- data.table(industry)
occupation <- read_delim("Data/OEWS/oe.occupation.txt", delim = "\t") %>% 
  select(c(`occupation_code`, `occupation_name`))
occupation <- data.table(occupation)
seasonal <- read_delim("Data/OEWS/oe.seasonal.txt", delim = "\t")
seasonal <- data.table(seasonal)
sector <- read_delim("Data/OEWS/oe.sector.txt", delim = "\t")
sector <- data.table(sector)

#read in series
series <- fread("Data/OEWS/oe.series.txt")
series_expanded <- series
data_all <- fread("Data/OEWS/oe.data.1.AllData.txt")

#only include data from all geographic regions
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"
colnames(series_expanded)[which(colnames(series_expanded) == "seasonal")] <- "seasonal_code"
# function for applying dictionary containing descriptions mapped to codes
matching_func <- function(col, df) {
  col_code <- paste(col, "code", sep = "_")
  # matching descriptions to values
  cols <- unique(c(colnames(df), colnames(series_expanded)))
  series_expanded <- series_expanded[df, on = col_code, nomatch = 0, ..cols]
  series_expanded
}
area$area_code <- as.numeric(area$area_code)
area$state_code <- as.numeric(area$state_code)
series_expanded <- matching_func(c("area", "state", "areatype"), area)
datatype$datatype_code <- as.numeric(datatype$datatype_code)
series_expanded <- matching_func("datatype", datatype)
series_expanded <- matching_func("industry", industry)
occupation$occupation_code <- as.numeric(occupation$occupation_code)
series_expanded <- matching_func("occupation", occupation)
series_expanded <- matching_func("seasonal", seasonal)
series_expanded <- matching_func("sector", sector)

# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- str_trim(colnames(series_expanded))
colnames(data_all) <- str_trim(colnames(data_all))

data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""

cols <- unique(c(colnames(data_all), colnames(series_expanded)))
cols <- cols[cols != 'footnote_codes']
data_all_expanded <- data_all[series_expanded, on = "series_id", nomatch = 0, ..cols]
data_all_expanded$series_id <- str_trim(data_all_expanded$series_id)


#generating wide tables for labor productivity data to make merging easier
OECS_tbl1 <- dcast(data_all_expanded, occupation_code + year + period + occupation_name + 
                     seasonal_code + seasonal_text + industry_code + industry_name + 
                     sector_code + sector_name + state_code + area_code + area_name ~ datatype_code, 
                   value.var = c("value"))
colnames(OECS_tbl1) <- paste(colnames(OECS_tbl1), "oe", sep = "_")
colnames(OECS_tbl1)[1] <- "soc_code"
colnames(OECS_tbl1)[2] <- "year"

cols <- c("datatype_code", "datatype_name")
OECS_desc <- unique(data_all_expanded[,..cols])
OECS_desc$datatype_code <- paste(OECS_desc$datatype_code, "oe", sep = "_")

fwrite(OECS_tbl1, "Merge/OEWS_agg.csv")
fwrite(OECS_desc, "Merge/OEWS_desc.csv")



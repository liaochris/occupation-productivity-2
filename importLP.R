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

# reading in the various labor productivity data files - see ip.txt for further information
area <- read_delim("Data/Productivity/ip.area.txt",
                   col_types = list(col_character(), col_character(), col_number(), col_character(), col_number()),
                   delim = "\t") %>% select(c(`area_code`, `area_text`))
duration <- read_delim("Data/Productivity/ip.duration.txt",
                       col_types = list(col_number(), col_character()),
                       delim = "\t") %>% select(c(`duration_code`, `duration_text`))
footnote <- read_delim("Data/Productivity/ip.footnote.txt",
                       col_types = list(col_character(), col_character()),
                       delim = "\t") %>% select(c(`footnote_code`, `footnote_text`))
footnote[nrow(footnote) + 1, ] <- list("", "")
industry <- read_delim("Data/Productivity/ip.industry.txt",
                       col_types = list(col_character(), col_character(), col_character(), col_number(), col_character(), col_number()),
                       delim = "\t") %>% select(c(`industry_code`, `industry_text`))
measure <- read_delim("Data/Productivity/ip.measure.txt",
                      col_types = list(col_character(), col_character(), col_number(), col_character(), col_number()),
                      delim = "\t") %>% select(c(`measure_code`, `measure_text`))
seasonal <- read_delim("Data/Productivity/ip.seasonal.txt",
                       col_types = list(col_character(), col_character()),
                       delim = "\t") %>% select(c(`seasonal_code`, `seasonal_text`))
sector <- read_delim("Data/Productivity/ip.sector.txt",
                     col_types = list(col_character(), col_character()),
                     delim = "\t") %>% select(c(`sector_code`, `sector_text`))
type <- read_delim("Data/Productivity/ip.type.txt",
                   col_types = list(col_character(), col_character()),
                   delim = "\t") %>% select(c(`type_code`, `type_text`))

# reading in Labor productivity time series data
# setting column types
ctypes_series <- list(
  col_character(), col_character(), col_character(), col_character(),
  col_character(), col_number(), col_character(),
  col_character(), col_character(), col_character(), col_character(),
  col_number(), col_character(), col_number(), col_character()
)
series <- read_delim("Data/Productivity/ip.series.txt",
                     col_types = ctypes_series,
                     delim = "\t")

# reading in labor productivity data for industries from 2003 onwards
data_all <- read_delim("Data/Productivity/ip.data.1.AllData.txt",
                       col_types = list(
                         col_character(), col_number(), col_character(), col_number(),
                         col_character()
                       ),
                       delim = "\t")

# adding applying dictionary get function to dataframe to get values for codes for columns
series_expanded <- series
colnames(series_expanded)[which(colnames(series) == "seasonal")] <- "seasonal_code"
colnames(series_expanded)[which(colnames(series) == "footnote_codes")] <- "footnote_code"

# function for applying dictionary containing descriptions mapped to codes
matching_func <- function(col, df) {
  col_code <- paste(col, "code", sep = "_")
  # matching descriptions to values
  series_expanded <- series_expanded %>% inner_join(df)
  target_col <- which(colnames(series_expanded) == col_code)
  # reordering dataframe columns
  series_expanded <- series_expanded[, c(
    1:target_col,
    length(colnames(series_expanded)),
    (target_col + 2):length(colnames(series_expanded)) - 1
  )]
  series_expanded
}
series_expanded <- matching_func("seasonal", seasonal)
series_expanded <- matching_func("sector", sector)
series_expanded <- matching_func("measure", measure)
series_expanded <- matching_func("industry", industry)
series_expanded <- matching_func("area", area)
series_expanded <- matching_func("duration", duration)


# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- lapply(colnames(series_expanded), str_trim)
colnames(data_all) <- lapply(colnames(data_all), str_trim)
data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""
data_all_expanded <- data_all %>% left_join(series_expanded, by = c("series_id")) %>% 
  select(-c('footnote_codes'))
data_all_expanded <- data.table(data_all_expanded)

#adding extra columns for more detail
data_all_expanded$NAICS <- gsub("N", "", gsub("_", "0", data_all_expanded$industry_code))
data_all_expanded$measure_code_expanded <- paste(data_all_expanded$measure_code, data_all_expanded$duration_code, sep = "_")
data_all_expanded[data_all_expanded$NAICS == "440450"]$NAICS <- "44-45"

#removing duplicates
filtered_cols <- colnames(data_all_expanded)[colnames(data_all_expanded) %ni% c('industry_code', 'series_id', 'series_title')]
data_all_expanded <- data_all_expanded[!duplicated(data_all_expanded, by = filtered_cols)]
colnames(data_all_expanded) <- toupper(colnames(data_all_expanded))

#generating wide tables for labor productivity data to make merging easier
lp_tbl1 <- dcast(data_all_expanded, YEAR + NAICS + INDUSTRY_CODE + INDUSTRY_TEXT +
                SEASONAL_CODE + SEASONAL_TEXT + SECTOR_CODE + SECTOR_TEXT + 
                AREA_CODE + AREA_TEXT ~ MEASURE_CODE_EXPANDED, value.var = "VALUE")
colnames(lp_tbl1) <- paste(colnames(lp_tbl1), "IP", sep = "_")
colnames(lp_tbl1)[1] <- "YEAR"
#colnames(lp_tbl1)[2] <- "NAICS"


#add dictionary mapping tbl3 to measure text

cols <- c("MEASURE_CODE_EXPANDED", "MEASURE_CODE", "DURATION_CODE", "MEASURE_TEXT", "DURATION_TEXT")
lp_desc <- unique(data_all_expanded[,..cols])
lp_desc$MEASURE_CODE_EXPANDED <- paste(lp_desc$MEASURE_CODE_EXPANDED, "IP", sep = "_")
lp_desc <- lp_desc[order(MEASURE_CODE_EXPANDED)]
  
fwrite(lp_tbl1, "Merge/LP_agg.csv")
fwrite(lp_desc, "Merge/LP_desc.csv")


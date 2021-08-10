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


# reading in the various occupational requirement data files - see ip.txt for further information
area <- read_delim("Data/Wage_Model/wm.area.txt",
                       delim = "\t") %>% select(c(`area_code`, `area_text`))
aspect <- read_delim("Data/Wage_Model/wm.aspect.txt",
                       delim = "\t")
datatype <- read_delim("Data/Wage_Model/wm.datatype.txt",
                       delim = "\t") %>% select(c(`datatype_code`, `datatype_text`))
estimate <- read_delim("Data/Wage_Model/wm.estimate.txt",
                       delim = "\t") %>% select(c(`estimate_code`, `estimate_text`))
footnote <- read_delim("Data/Wage_Model/wm.footnote.txt",
                       delim = "\t") 
industry <- read_delim("Data/Wage_Model/wm.industry.txt",
                       delim = "\t") %>% select(c(`industry_code`, `industry_text`))
level <- read_delim("Data/Wage_Model/wm.level.txt",
                    delim = "\t") %>% select(c(`level_code`, `level_text`))
occupation <- read_delim("Data/Wage_Model/wm.occupation.txt",
                         delim = "\t") %>% select(c(`occupation_code`, `occupation_text`))

ownership <- read_delim("Data/Wage_Model/wm.ownership.txt",
                        delim = "\t") %>% select(c(`ownership_code`, `ownership_text`))
period <- read_delim("Data/Wage_Model/wm.period.txt",
                     delim = "\t") 
seasonal <- read_delim("Data/Wage_Model/wm.seasonal.txt",
                     delim = "\t") 
subcell <- read_delim("Data/Wage_Model/wm.subcell.txt",
                      delim = "\t") %>% select(c(`subcell_code`, `subcell_text`))
#read in series
series <- vroom("Data/Wage_Model/wm.series.txt")
series_expanded <- series
data_all <- vroom("Data/Wage_Model/wm.data.1.AllData.txt")

#only include data from all geographic regions
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"
colnames(series_expanded)[which(colnames(series_expanded) == "seasonal")] <- "seasonal_code"

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
series_expanded <- matching_func("area", area)
series_expanded <- matching_func("datatype", datatype)
series_expanded <- matching_func("estimate", estimate)
series_expanded <- matching_func("industry", industry)
series_expanded <- matching_func("level", level)
series_expanded <- matching_func("occupation", occupation)
series_expanded <- matching_func("ownership", ownership)
series_expanded <- matching_func("seasonal", seasonal)
series_expanded <- matching_func("subcell", subcell)

# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- str_trim(colnames(series_expanded))
colnames(data_all) <- str_trim(colnames(data_all))

data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""

data_all_expanded <- data_all %>% left_join(series_expanded, by = c("series_id")) %>% 
  select(-c('footnote_codes'))
data_all_expanded$series_id <- str_trim(data_all_expanded$series_id)
data_all_expanded <- data.table(data_all_expanded)


data_all_expanded[,`datatype_estimate_code`:=paste(datatype_code, estimate_code, sep = "_")]

#data_all_expanded <- data_all_expanded[data_all_expanded$datatype_estimate_level_code != "NA_NA_NA"]

#generating wide tables for labor productivity data to make merging easier
WM_tbl1 <- dcast(data_all_expanded, occupation_code + year + period + occupation_text + area_code + 
                   area_text + ownership_code + ownership_text +seasonal_code + seasonal_text + 
                   industry_code + industry_text + subcell_code + subcell_text + level_code + 
                   level_text ~ datatype_estimate_code, value.var = c("value"))
colnames(WM_tbl1) <- paste(colnames(WM_tbl1), "wm", sep = "_")
#colnames(WM_tbl1)[1] <- "soc_code"
colnames(WM_tbl1)[2] <- "year"

cols <- c("datatype_estimate_code", "datatype_code", "datatype_text", 'estimate_code', 
          'estimate_text')
WM_desc <- unique(data_all_expanded[,..cols])
WM_desc$datatype_estimate_code <- paste(WM_desc$datatype_estimate_code, "wm", sep = "_")

fwrite(WM_tbl1, "Merge/WM_agg.csv")
fwrite(WM_desc, "Merge/WM_desc.csv")

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

setwd("~/scratch-midway2/occupation-productivity-2/")

#starting this year we have files
'%ni%' <- Negate('%in%')


# reading in the various occupational requirement data files - see ip.txt for further information
datatype <- read_delim("Data/NCS_wage/nw.datatype_id.txt",
                      delim = "\t") %>% select(c("datatype_id_code", "datatype_id_text"))
datatype <- data.table(datatype)
estimate <- read_delim("Data/NCS_wage/nw.estimate_id.txt",
                       delim = "\t") %>% select(c(`estimate_id_code`, `estimate_id_text`))
estimate <- data.table(estimate)
footnote <- read_delim("Data/NCS_wage/nw.footnote.txt",
                       delim = "\t") 
footnote <- data.table(footnote)
industry <- read_delim("Data/NCS_wage/nw.industry.txt",
                       delim = "\t") %>% select(c(`industry_code`, `industry_text`))
industry <- data.table(industry)
level <- read_delim("Data/NCS_wage/nw.level.txt",
                       delim = "\t") %>% select(c(`level_code`, `level_coefficient`))
level <- data.table(level)
colnames(level)[2] <- "level_text"
occupation <- read_delim("Data/NCS_wage/nw.occupation.txt",
                         delim = "\t") %>% select(c(`soc_code`, `soc_text`))
occupation <- data.table(occupation)
ownership <- read_delim("Data/NCS_wage/nw.ownership.txt",
                        delim = "\t") %>% select(c(`ownership_code`, `ownership_text`))
ownership <- data.table(ownership)
starea <- read_delim("Data/NCS_wage/nw.starea.txt",
                        delim = "\t") %>% select(c(`state_code`, `area_code`, `area_text`))
starea <- data.table(starea)
subcell <- read_delim("Data/NCS_wage/nw.subcell_id.txt",
                      delim = "\t") %>% select(c(`subcell_id_code`, `subcell_id_text`))
subcell <- data.table(subcell)

#read in series

series <- fread("Data/NCS_wage/nw.series.txt")
series_expanded <- series
data_all <- fread("Data/NCS_wage/nw.data.1.AllData.txt")

#only include data from all geographic regions
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"
# function for applying dictionary containing descriptions mapped to codes
matching_func <- function(col, df) {
  col_code <- paste(col, "code", sep = "_")
  # matching descriptions to values
  cols <- unique(c(colnames(df), colnames(series_expanded)))
  series_expanded <- series_expanded[df, on = col_code, nomatch = 0, ..cols]
  series_expanded
}
datatype$datatype_id_code <- as.numeric(datatype$datatype_id_code)
series_expanded <- matching_func("datatype_id", datatype)
estimate$estimate_id_code <- as.numeric(estimate$estimate_id_code)
series_expanded <- matching_func("estimate_id", estimate)
series_expanded <- matching_func("industry", industry)
level$level_code <- as.numeric(level$level_code)
series_expanded <- matching_func("level", level)
occupation$soc_code <- as.numeric(occupation$soc_code)
series_expanded <- matching_func("soc", occupation)
ownership$ownership_code <- as.numeric(ownership$ownership_code)
series_expanded <- matching_func("ownership", ownership)
#series_expanded <- matching_func("area", starea)
subcell$subcell_id_code <- as.numeric(subcell$subcell_id_code)
series_expanded <- matching_func("subcell_id", subcell)

starea$area_code <- as.numeric(starea$area_code)
starea$state_code <- as.numeric(starea$state_code)
series_expanded <-  matching_func(c("state", "area"), starea)

# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- str_trim(colnames(series_expanded))
colnames(data_all) <- str_trim(colnames(data_all))

data_all$footnote_codes <- as.character(data_all$footnote_codes)
data_all[is.na(footnote_codes)]$footnote_codes <- ""

cols <- unique(c(colnames(data_all), colnames(series_expanded)))
cols <- cols[cols != 'footnote_codes']
data_all_expanded <- data_all[series_expanded, on = "series_id", nomatch = 0, ..cols]
data_all_expanded$series_id <- str_trim(data_all_expanded$series_id)

data_all_expanded[,`datatype_estimate_code`:=paste(datatype_id_code, estimate_id_code, sep = "_")]

#generating wide tables for labor productivity data to make merging easier
NCS_W_tbl1 <- dcast(data_all_expanded, soc_code + year + period + soc_text + 
                      seasonality + ownership_code + ownership_text + industry_code + 
                      industry_text + level_code + level_text + state_code + area_code + area_text +
                      subcell_id_code + subcell_id_text ~ datatype_estimate_code, 
                    value.var = c("value"))
colnames(NCS_W_tbl1) <- paste(colnames(NCS_W_tbl1), "NW", sep = "_")
#colnames(NCS_W_tbl1)[1] <- "soc_code"
colnames(NCS_W_tbl1)[2] <- "year"

cols <- c("datatype_estimate_code", "datatype_id_code", "datatype_id_text", 'estimate_id_code', 
          'estimate_id_text')
NCS_W_desc <- unique(data_all_expanded[,..cols])
NCS_W_desc$datatype_estimate_code <- paste(NCS_W_desc$datatype_estimate_code, "NW", sep = "_")

fwrite(NCS_W_tbl1, "Merge/NCS_W_agg.csv")
fwrite(NCS_W_desc, "Merge/NCS_W_desc.csv")

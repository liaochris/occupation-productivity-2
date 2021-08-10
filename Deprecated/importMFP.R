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

#Multifactor productivity

# reading in the various labor productivity data files - see ip.txt for further information
duration <- read_delim("Data/Multifactor_Productivity/mp.duration.txt",
                       delim = "\t") %>% select(c(`DURATION_CODE`, `DURATION_TEXT`))
colnames(duration) <- tolower(colnames(duration))

footnote <- read_delim("Data/Multifactor_Productivity/mp.footnote.txt",
                       delim = "\t") %>% select(c(`footnote_code`, `footnote_text`))
footnote[nrow(footnote) + 1, ] <- list("", "")
colnames(footnote) <- tolower(colnames(footnote))

measure <- read_delim("Data/Multifactor_Productivity/mp.measure.txt",
                      delim = "\t") %>% select(c(`MEASURE_CODE`, `MEASURE_TEXT`))
colnames(measure) <- tolower(colnames(measure))

#seasonal <- read_delim("Data/Multifactor_Productivity/mp.seasonal.txt",
#                       delim = "\t") %>% select(c(`SEASONAL_CODE`, `SEASONAL_TEXT`))

sector <- read_delim("Data/Multifactor_Productivity/mp.sector.txt",
                     delim = "\t") %>% select(c(`SECTOR_CODE`, `SECTOR_NAME`))
colnames(sector) <- tolower(colnames(sector))

# reading in Labor productivity time series data
# setting column types
series <- read_delim("Data/Multifactor_Productivity/mp.series.txt",
                     delim = "\t")
series[is.na(series$footnote_codes),]$footnote_codes <- ""

# reading in labor productivity data for industries from 2003 onwards
data_all <- read_delim("Data/Multifactor_Productivity/mp.data.1.AllData.txt",
                       delim = "\t")

series_expanded <- series
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"

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
series_expanded <- matching_func("sector", sector)
series_expanded <- matching_func("measure", measure)
series_expanded <- matching_func("duration", duration)
series_expanded <- matching_func("footnote", footnote)


# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- lapply(colnames(series_expanded), str_trim)
colnames(data_all) <- lapply(colnames(data_all), str_trim)

data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""
data_all_expanded <- data_all %>% left_join(series_expanded, by = c("series_id")) %>% 
  select(-c('footnote_codes'))
#adding naics code
data_all_expanded$NAICS_raw <- str_trim(mgsub(unlist(regmatches(data_all_expanded$sector_name, gregexpr("\\(.*?\\)", data_all_expanded$sector_name))), c("\\(", "NAICS", "\\)"), c("", "", "")))

#cleaning up naics code
cleanStrings <- function(df, col_ind) {
  temp <- foreach(i = 1:dim(df)[1], .combine = "bind_rows") %do% {
    # select row being modified
    selRow <- df[i, ]
    val <- selRow[[col_ind]]
    # check if there is a dash and a comma in this row
    if (grepl(",", val)) {
      codes_list <- strsplit(val, split = ",")[[1]]
      selRow_temp <- foreach(i = codes_list, .combine = "bind_rows") %do% {
        temp <- selRow
        temp$NAICS <- as.character(i)
        temp
      }
      selRow <- selRow_temp
    }
    else if (grepl("-", val)) {
      codes_list <- strsplit(val, split = "-")[[1]]
      selRow_temp <- foreach(i = codes_list[1]:codes_list[2], .combine = "bind_rows") %do% {
        temp <- selRow
        temp$NAICS <- as.character(i)
        temp
      }
      selRow <-selRow_temp
    }
    else if (!is.na(as.numeric(val))) {
      selRow$NAICS <- as.character(val)
    }
    else {
      selRow$NAICS <- "REMOVE"
    }
    selRow
  }
  # return created dataframe
  temp
}
data_naics_all_expanded_raw <- cleanStrings(data_all_expanded, 'NAICS_raw')

data_naics_all_expanded <- data_naics_all_expanded_raw[data_naics_all_expanded_raw$NAICS != "REMOVE",]
data_naics_all_expanded$NAICS <- str_pad(data_naics_all_expanded$NAICS, 6, side = "right", pad = "0")
data_naics_all_expanded$measure_code_expanded <- paste(data_naics_all_expanded$measure_code, data_naics_all_expanded$duration_code, sep = "_")

colnames(data_naics_all_expanded) <- toupper(colnames(data_naics_all_expanded))
data_naics_all_expanded <- data.table(data_naics_all_expanded)

#generating wide tables for labor productivity data to make merging easier
mfp_tbl1 <- dcast(data_naics_all_expanded, YEAR + NAICS + SECTOR_CODE + SECTOR_NAME +
                DURATION_CODE + DURATION_TEXT ~ MEASURE_CODE, value.var = "VALUE")
colnames(mfp_tbl1) <- paste(colnames(mfp_tbl1), "MP", sep = "_")
colnames(mfp_tbl1)[1] <- "YEAR"
colnames(mfp_tbl1)[2] <- "NAICS"

mfp_tbl2 <- dcast(data_naics_all_expanded, YEAR + NAICS + SECTOR_CODE + SECTOR_NAME +
                DURATION_CODE + DURATION_TEXT ~ MEASURE_CODE + MEASURE_TEXT, value.var = "VALUE")
colnames(mfp_tbl2) <- paste(colnames(mfp_tbl2), "MP", sep = "_")
colnames(mfp_tbl2)[1] <- "YEAR"
colnames(mfp_tbl2)[2] <- "NAICS"

mfp_tbl3 <- dcast(data_naics_all_expanded, YEAR + NAICS + SECTOR_CODE + 
                SECTOR_NAME ~ MEASURE_CODE_EXPANDED, value.var = "VALUE")
colnames(mfp_tbl3) <- paste(colnames(mfp_tbl3), "MP", sep = "_")
colnames(mfp_tbl3)[1] <- "YEAR"
colnames(mfp_tbl3)[2] <- "NAICS"

mfp_tbl1 <- dcast(data_naics_all_expanded, YEAR + NAICS + SECTOR_CODE +
                SECTOR_NAME ~ MEASURE_CODE_EXPANDED + MEASURE_TEXT, value.var = "VALUE")
colnames(mfp_tbl1) <- paste(colnames(mfp_tbl1), "MP", sep = "_")
colnames(mfp_tbl1)[1] <- "YEAR"
colnames(mfp_tbl1)[2] <- "NAICS"

#add dictionary mapping tbl3 to measure text

cols <- c("MEASURE_CODE_EXPANDED", "MEASURE_CODE", "DURATION_CODE", "MEASURE_TEXT", "DURATION_TEXT")
mfp_desc <- unique(data_naics_all_expanded[,..cols])
mfp_desc$MEASURE_CODE_EXPANDED <- paste(mfp_desc$MEASURE_CODE_EXPANDED, "MP", sep = "_")
fwrite(mfp_tbl3, "Merge/MFP_agg.csv")
fwrite(mfp_desc, "Merge/MFP_desc.csv")

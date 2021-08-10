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

# reading in the various occupational requirement data files - see ip.txt for further information
additive <- read_delim("Data/Occupational_Requirements/or.additive.txt",
                       delim = "\t") %>% select(c(`additive_code`, `additive_text`))
colnames(additive) <- tolower(colnames(additive))
aspect <- read_delim("Data/Occupational_Requirements/or.aspect.txt",
                     delim = "\t") 
colnames(aspect) <- tolower(colnames(aspect))
category <- read_delim("Data/Occupational_Requirements/or.category.txt",
                       delim = "\t") %>% select(c(`category_code`, `category_text`))
colnames(category) <- tolower(colnames(category))
datatype <- read_delim("Data/Occupational_Requirements/or.datatype.txt",
                       delim = "\t") %>% select(c(`datatype_code`, `datatype_text`))
colnames(datatype) <- tolower(colnames(datatype))
estimate <- read_delim("Data/Occupational_Requirements/or.estimate.txt",
                       delim = "\t") %>% select(c(`estimate_code`, `estimate_text`))
colnames(estimate) <- tolower(colnames(estimate))
footnote <- read_delim("Data/Occupational_Requirements/or.footnote.txt",
                       delim = "\t") 
footnote[nrow(footnote) + 1, ] <- list("", "")
colnames(footnote) <- tolower(colnames(footnote))
occupation <- read_delim("Data/Occupational_Requirements/or.occupation.txt",
                         delim = "\t") %>% select(c(`occupation_code`, `soc_code`, `occupation_text`))
colnames(occupation) <- tolower(colnames(occupation)) 
requirement <- read_delim("Data/Occupational_Requirements/or.requirement.txt",
                          delim = "\t") %>% select(c(`requirement_code`, `requirement_text`))
colnames(requirement) <- tolower(colnames(requirement)) 
seasonal <- read_delim("Data/Occupational_Requirements/or.seasonal.txt",
                       delim = "\t") %>% select(c(`seasonal_code`, `seasonal_text`))
colnames(seasonal) <- tolower(colnames(seasonal)) 

#read in series
series <- read_delim("Data/Occupational_Requirements/or.series.txt",
                     delim = "\t")
series[is.na(series$footnote_codes),]$footnote_codes <- ""
series_expanded <- series
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"
colnames(series_expanded)[which(colnames(series_expanded) == "seasonal")] <- "seasonal_code"

data_all <- read_delim("Data/Occupational_Requirements/or.data.1.AllData.txt",
                       delim = "\t")
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
series_expanded <- matching_func("additive", additive)
series_expanded <- matching_func("category", category)
series_expanded <- matching_func("datatype", datatype)
series_expanded <- matching_func("estimate", estimate)
series_expanded <- matching_func("occupation", occupation)
series_expanded <- matching_func("requirement", requirement)
series_expanded <- matching_func("seasonal", seasonal)

# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- lapply(colnames(series_expanded), str_trim)
colnames(data_all) <- lapply(colnames(data_all), str_trim)

data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""
data_all_expanded <- data_all %>% left_join(series_expanded, by = c("series_id")) %>% 
  select(-c('footnote_codes'))
data_all_expanded$series_id <- str_trim(data_all_expanded$series_id)
colnames(aspect)[which(colnames(aspect) == "value")] <- "std_error"

aspect$`std_error` <- as.numeric(str_trim(aspect$`std_error`))

data_all_expanded <- data_all_expanded %>% 
  left_join(aspect %>% select(-c(`aspect_type`, `footnote_codes`)))

data_all_expanded <- data.table(data_all_expanded)

#how do I deal with requirement code

#generating wide tables for labor productivity data to make merging easier
ORS_tbl1 <- dcast(data_all_expanded, soc_code + occupation_code + occupation_text + 
                    period + seasonal_code + seasonal_text + ownership_code ~  
                    estimate_code, value.var = c("value"))
colnames(ORS_tbl1) <- paste(colnames(ORS_tbl1), "OR", sep = "_")
#colnames(ORS_tbl1)[1] <- "soc_code"

#add dictionary mapping tbl1 to measure estimate text
cols <- c("estimate_code", "estimate_text", "category_code", 'category_text',
          'datatype_code', 'datatype_text')
ors_desc <- unique(data_all_expanded[,..cols])
ors_desc$estimate_code <- paste(ors_desc$estimate_code, "OR", sep = "_")
ors_desc <- ors_desc[order(estimate_code)]

fwrite(ORS_tbl1, "Merge/ORS_agg.csv")
fwrite(ors_desc, "Merge/ORS_desc.csv")



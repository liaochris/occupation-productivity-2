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
aspect <- read_delim("Data/NCS_benefits/nb.aspect.txt",
                     delim = "\t") 
datatype <- read_delim("Data/NCS_benefits/nb.datatype.txt",
                       delim = "\t") %>% select(c(`datatype_code`, `datatype_text`))
estimate <- read_delim("Data/NCS_benefits/nb.estimate.txt",
                       delim = "\t") %>% select(c(`estimate_code`, `estimate_text`))
footnote <- read_delim("Data/NCS_benefits/nb.footnote.txt",
                       delim = "\t") 
industry <- read_delim("Data/NCS_benefits/nb.industry.txt",
                       delim = "\t") %>% select(c(`industry_code`, `industry_text`))
occupation <- read_delim("Data/NCS_benefits/nb.occupation.txt",
                         delim = "\t") %>% select(c(`occupation_code`, `occupation_text`))
ownership <- read_delim("Data/NCS_benefits/nb.ownership.txt",
                        delim = "\t") %>% select(c(`ownership_code`, `ownership_text`))
provision <- read_delim("Data/NCS_benefits/nb.provision.txt",
                        delim = "\t") %>% select(c(`provision_code`, `provision_text`))
seasonal <- read_delim("Data/NCS_benefits/nb.seasonal.txt",
                       delim = "\t") %>% select(c(`seasonal_code`, `seasonal_text`))
subcell <- read_delim("Data/NCS_benefits/nb.subcell.txt",
                      delim = "\t") %>% select(c(`subcell_code`, `subcell_text`))
survey <- read_delim("Data/NCS_benefits/nb.survey.txt",
                     delim = "\t") %>% select(c(`survey_code`, `survey_text`))

#read in series
series <- read_delim("Data/NCS_benefits/nb.series.txt",
                     delim = "\t")
series[is.na(series$footnote_codes),]$footnote_codes <- ""
series_expanded <- series
colnames(series_expanded)[which(colnames(series_expanded) == "footnote_codes")] <- "footnote_code"
colnames(series_expanded)[which(colnames(series_expanded) == "seasonal")] <- "seasonal_code"

data_all <- read_delim("Data/NCS_benefits/nb.data.1.AllData.txt",
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
series_expanded <- matching_func("datatype", datatype)
series_expanded <- matching_func("estimate", estimate)
series_expanded <- matching_func("industry", industry)
series_expanded <- matching_func("occupation", occupation)
series_expanded <- matching_func("ownership", ownership)
series_expanded <- matching_func("provision", provision)
series_expanded <- matching_func("seasonal", seasonal)
series_expanded <- matching_func("subcell", subcell)
series_expanded <- matching_func("survey", survey)

# merge data_current from the BLS time series data and series_expanded with information on each series
# matchines descriptors to each numerical value
colnames(series_expanded) <- str_trim(colnames(series_expanded))
colnames(data_all) <- str_trim(colnames(data_all))

data_all$footnote_codes[is.na(data_all$footnote_codes)] <- ""
data_all_expanded <- data_all %>% left_join(series_expanded, by = c("series_id")) %>% 
  select(-c('footnote_codes'))
data_all_expanded$series_id <- str_trim(data_all_expanded$series_id)
colnames(aspect)[which(colnames(aspect) == "value")] <- "std_error"
aspect$`std_error` <- as.numeric(str_trim(aspect$`std_error`))
data_all_expanded <- data_all_expanded %>% 
  left_join(aspect %>% select(-c(`aspect_type`, `footnote_codes`)))

data_all_expanded <- data.table(data_all_expanded)
data_all_expanded[,`datatype_provision_code`:=paste(datatype_code, provision_code, sep = "_")]


#generating wide tables for labor productivity data to make merging easier
NCS_B_tbl1 <- dcast(data_all_expanded, occupation_code + year + period + occupation_text + 
                      seasonal_code + seasonal_text + ownership_code + ownership_text +
                      industry_code + industry_text + subcell_code + subcell_text + survey_code +
                      survey_text ~ datatype_provision_code, value.var = c("value"))
colnames(NCS_B_tbl1) <- paste(colnames(NCS_B_tbl1), "NB", sep = "_")
#colnames(NCS_B_tbl1)[1] <- "soc_group"
colnames(NCS_B_tbl1)[2] <- "year"

cols <- c("datatype_provision_code", "datatype_code", "datatype_text", 
          'provision_code', 'provision_text')
NCS_B_desc <- unique(data_all_expanded[,..cols])
NCS_B_desc$datatype_provision_code <- paste(NCS_B_desc$datatype_provision_code, "NB", sep = "_")

fwrite(NCS_B_tbl1, "Merge/NCS_B_agg.csv")
fwrite(NCS_B_desc, "Merge/NCS_B_desc.csv")

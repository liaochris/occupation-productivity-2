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

data_2018_raw <- data.table(read_xlsx("Data/Occupational_Requirements/2018_excel_output.xlsx", sheet = 2))

data_2020 <- data.table(read_xlsx("Data/Occupational_Requirements/ors-complete-dataset.xlsx", sheet = 3))

#crosswalk for 2010 and 2018 SOC Codes
soc1018 <- data.table(read_xlsx("Data/Occupational_Requirements/soc_2010_to_2018_crosswalk.xlsx", skip = 8))
soc1018 <- soc1018[`2010 SOC Code` != `2018 SOC Code`]
soc1018[,`2010 SOC Code`:=as.numeric(gsub("-", "", `2010 SOC Code`))]
soc1018[,`2018 SOC Code`:=as.numeric(gsub("-", "", `2018 SOC Code`))]

#numerical conversion and filtering out ultraspecific
data_2018_raw <- data_2018_raw[str_sub(`O*NET-SOC 2010 CODE`, -2) == "00"]
data_2018_raw$`O*NET-SOC 2010 CODE` <- as.numeric(data_2018_raw$`O*NET-SOC 2010 CODE`)/100

#converting 2010 SOC Codes to 2018 SOC Codes
keepcols <- unique(c(colnames(soc1018), colnames(data_2018_raw)))
data_2018 <- soc1018[data_2018_raw, on = c("2010 SOC Code" = "O*NET-SOC 2010 CODE"), ..keepcols, 
                     allow.cartesian = TRUE]
data_2018[!is.na(`2010 SOC Title`), `SOC 2018 CODE`:=`2018 SOC Code`]
data_2018[is.na(`2010 SOC Title`), `SOC 2018 CODE`:=`O*NET-SOC 2010 CODE`]
data_2018[!is.na(`2010 SOC Title`), `OCCUPATION`:=`2018 SOC Title`]

#removing merging document
remcols <- c("2010 SOC Code", "2010 SOC Title", "2018 SOC Code", "2018 SOC Title",
             "O*NET-SOC 2010 CODE")
data_2018 <- data_2018[,-remcols,with = FALSE]

#adding datatype
datatype <- fread("Data/Occupational_Requirements/or.datatype.txt")[,c(1,2), with = FALSE]
data_2018[grepl("hours",`SERIES TITLE`), `DATATYPE`:="Hours"]
data_2018[grepl("%",`SERIES TITLE`), `DATATYPE`:="Percentage"]
data_2018[grepl("pounds",`SERIES TITLE`), `DATATYPE`:="Pounds"]
data_2018[grepl("days",`SERIES TITLE`), `DATATYPE`:="Days"]
data_2018 <- data_2018[datatype, on = c("DATATYPE" = "datatype_text")]

#adding estimate
estimate <- fread("Data/Occupational_Requirements/or.estimate.txt")[,c(1,2), with = FALSE]
data_2018[,`ESTIMATE CODE`:= as.numeric(str_sub(`SERIES ID`, -5))]
keepcols <- unique(c(colnames(data_2018), colnames(estimate)))
data_2018 <- estimate[data_2018, on = c("estimate_code" = "ESTIMATE CODE" ), ..keepcols]

semicolon_pos <- data_2018[is.na(`estimate_text`) & grepl("%", `SERIES TITLE`), str_locate(`SERIES TITLE`, ";")[1]]

#Extracting ones with percenages
data_2018[is.na(`estimate_text`) & grepl("%", `SERIES TITLE`), 
          `estimate_text`:=str_sub(`SERIES TITLE`,semicolon_pos + 2)]
data_2018[grepl("% of all workers; ", `estimate_text`), 
          `estimate_text`:= gsub("% of all workers; ", "Percent of workers, ", `estimate_text`)]
data_2018[grepl("% of workers in ", `estimate_text`),
          `estimate_text`:= paste("Percent of workers,",
                                  str_sub(`estimate_text`, str_locate(`estimate_text`, ";")[,1]+2))]

#Extracting ones with days
data_2018[grepl("days of", `SERIES TITLE`) & is.na(`estimate_text`),
          `estimate_text`:= 
            paste("D",str_sub(`SERIES TITLE`, str_locate(`SERIES TITLE`, "days of")[,1]+1), sep = "")]
data_2018 <- data_2018[,-c("estimate_code"), with = FALSE]

#adding categories
category <- fread("Data/Occupational_Requirements/or.category.txt")[,c(1,2), with = FALSE]
keepcols <- unique(c(colnames(category), colnames(data_2018)))
data_2018 <- category[data_2018, on = c("category_text" = "CATEGORY"), ..keepcols]

#add category codes
data_2018[`CATEGORY` == "SVP",`category_code`:=10]
data_2018[`CATEGORY` == "Post-employment training",`category_code`:=14]
data_2018[`CATEGORY` == c("Pre-employment training"),`category_code`:=12]
data_2018[`CATEGORY` == c("Pre-employment training: Certification"),`category_code`:=89]
data_2018[`CATEGORY` == c("Pre-employment training: Educational Certificate"),`category_code`:=90]
data_2018[`CATEGORY` ==  c("Pre-employment training: License"),`category_code`:=91]
data_2018[`CATEGORY` == "High, exposed places",`category_code`:=50]
data_2018[`CATEGORY` == "Climbing ramps or stairs (structure-related)",`category_code`:=38]
data_2018[`CATEGORY` == "Climbing ramps or stairs (work-related)",`category_code`:=39]
data_2018[`CATEGORY` == "Communicating verbally",`category_code`:=55]
data_2018[`CATEGORY` == "Far visual acuity",`category_code`:=53]
data_2018[`CATEGORY` == "Fine manipulation: one or both hands",`category_code`:=37]
data_2018[`CATEGORY` == "Foot/leg controls",`category_code`:=32]
data_2018[`CATEGORY` == "Foot/leg controls: one or both feet/legs",`category_code`:=33]
data_2018[`CATEGORY` == "Gross manipulation: one or both hands",`category_code`:=35]
data_2018[`CATEGORY` == "Keyboarding: Traditional",`category_code`:=20]
data_2018[`CATEGORY` == "Lifting/carrying Constantly",`category_code`:=27]
data_2018[`CATEGORY` == "Lifting/carrying Frequently",`category_code`:=26]
data_2018[`CATEGORY` == "Lifting/carrying Occasionally",`category_code`:=25]
data_2018[`CATEGORY` == "Lifting/carrying Seldom",`category_code`:=24]
data_2018[`CATEGORY` == "Near visual acuity",`category_code`:=52]
data_2018[`CATEGORY` == "Pushing/pulling: Feet/legs",`category_code`:=30]
data_2018[`CATEGORY` == "Pushing/pulling: Hands/arms",`category_code`:=29]
data_2018[`CATEGORY` == "Pushing/pulling: one or both feet/legs",`category_code`:=64]
data_2018[`CATEGORY` == "Pushing/pulling: one or both hands/arms",`category_code`:=63]
data_2018[`CATEGORY` == "Reaching at/below the shoulder",`category_code`:=18]
data_2018[`CATEGORY` == "Reaching at/below the shoulder: one or both hands",`category_code`:=19]
data_2018[`CATEGORY` == "Reaching overhead: one or both hands",`category_code`:=17]
data_2018[`CATEGORY` == "Sitting vs. standing/walking at will",`category_code`:=15]
data_2018[`CATEGORY` == "Standing/walking",`category_code`:=77]
data_2018[`CATEGORY` == "Maximum weight lifted/carried",`category_code`:=79]
data_2018[`CATEGORY` == "Hearing requirements: Group or conference",`category_code`:=112]
data_2018[`CATEGORY` == "Hearing requirements: One-on-one",`category_code`:=112]
data_2018[`CATEGORY` == "Hearing requirements: Other Sounds",`category_code`:=60]
data_2018[`CATEGORY` == "Associates Degree Time",`category_code`:=91]
data_2018[`CATEGORY` == "High School Vocational Time",`category_code`:=91]
data_2018[`CATEGORY` == "Vocational Associates Degree Time",`category_code`:=91]
data_2018 <- data_2018[,-c("category_text"), with = FALSE]

keepcols <- unique(c(colnames(category), colnames(data_2018)))
data_2018 <- category[data_2018, on = c("category_code"), ..keepcols]

#additive text
additive <- fread("Data/Occupational_Requirements/or.additive.txt")[,c(1,2), with = FALSE]
keepcols <- unique(c(colnames(additive), colnames(data_2018)))
data_2018$`ADDITIVE CODE` <- as.numeric(data_2018$`ADDITIVE CODE`)
data_2018 <- additive[data_2018, on = c("additive_code" = "ADDITIVE CODE"), ..keepcols]
unique(data_2018[is.na(additive_text), additive_code])

data_2018[additive_code == 28, additive_text:= "Crawling"]
data_2018[additive_code == 62, additive_text:= "Crouching"]
data_2018[additive_code == 58, additive_text:= "Hearing requirements in group/conference"]
data_2018[additive_code == 57, additive_text:= "Hearing requirements in one-on-one"]
data_2018[additive_code == 61, additive_text:= "Hearing Test"]
data_2018[additive_code == 42, additive_text:= "Kneeling"]
data_2018[`SERIES ID` == "ORUP1000000000000802", additive_text:= "Kneeling"]
data_2018[`SERIES ID` == "ORUP1000000000000802", additive_code:= 0]
data_2018[additive_code == 66, additive_text:= "Hearing Test"]
data_2018[additive_code == 41, additive_text:= "Stooping"]

#combining 2018 and 2020 data
data_2018[is.na(category_text), category_text := paste(`CATEGORY`, "- nomatch")]
data_2018[is.na(`category_code`), `category_code` := ifelse(`additive_code` %in% c(66, 0), 66, 61)]
data_2018 <- data_2018[,-c("additive_code", "CATEGORY")]

colnames(data_2018)[1:5] <- c("ADDITIVE", "CATEGORY CODE", "CATEGORY", "SERIES_ID",
                              "SERIES_TITLE")
colnames(data_2018)[16] <- c("DATATYPE CODE")
colnames(data_2018)[18] <- c("ESTIMATE TEXT")
data_2018$YEAR <- 2018
data_2020$YEAR <- 2020

combined_ORS_data <- rbind(data_2020, data_2018)
combined_ORS_data$`ESTIMATE CODE` <- as.numeric(combined_ORS_data$`ESTIMATE CODE`)
combined_ORS_data$`CATEGORY CODE` <- as.numeric(combined_ORS_data$`CATEGORY CODE`)
combined_ORS_data$`ADDITIVE CODE` <- as.numeric(combined_ORS_data$`ADDITIVE CODE`)
combined_ORS_data$`DATATYPE CODE` <- as.numeric(combined_ORS_data$`DATATYPE CODE`)

combined_ORS_data$`SOC 2018 CODE` <- as.numeric(combined_ORS_data$`SOC 2018 CODE`)

#filtering out non uq, keep most recent
combined_ORS_data_filtered <- combined_ORS_data[!duplicated(combined_ORS_data,by = c("SOC 2018 CODE", "ESTIMATE CODE"))]

colnames(combined_ORS_data_filtered)[which(colnames(combined_ORS_data_filtered) == "SOC 2018 CODE")] <- "SOC_2018_CODE"
colnames(combined_ORS_data_filtered)[which(colnames(combined_ORS_data_filtered) == "ESTIMATE CODE")] <- "ESTIMATE_CODE"

#generating wide tables for labor productivity data to make merging easier
ORS_tbl1 <- dcast(combined_ORS_data_filtered, `SOC_2018_CODE` + `OCCUPATION`  ~  
                    `ESTIMATE_CODE`, value.var = c("ESTIMATE"))
colnames(ORS_tbl1) <- paste(colnames(ORS_tbl1), "OR", sep = "_")
ORS_tbl1 <- ORS_tbl1[!duplicated(ORS_tbl1, by = c("SOC_2018_CODE_OR"))]

#add dictionary mapping tbl1 to measure estimate text
cols <- c("ESTIMATE_CODE", "ESTIMATE TEXT", "CATEGORY CODE", 'CATEGORY',
          'DATATYPE CODE', 'DATATYPE')
ors_desc <- unique(combined_ORS_data_filtered[,..cols])
ors_desc$ESTIMATE_CODE <- paste(ors_desc$ESTIMATE_CODE, "OR", sep = "_")
ors_desc <- ors_desc[order(ESTIMATE_CODE)]

fwrite(ORS_tbl1, "Merge/ORS_agg.csv")
fwrite(ors_desc, "Merge/ORS_desc.csv")



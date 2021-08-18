# Import libraries
library(data.table)
library(ggplot2)
library(Hmisc)
library(stringr)
library(plm)
# disable scientific notation
options(scipen = 999)

setwd("~/Google Drive/Non-Academic Work/Research/Traina/occupation-productivity-2/")

#starting this year we have files
'%ni%' <- Negate('%in%')

#summary statistics function
summaryStats <- function (x, name) {
  print(name)
  stats_vector <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE), mean(is.na(x)))
  names(stats_vector) <- c("Mean", "SD", "Missing")
  print(c(stats_vector, quantile(x, c(0,.1,.25, .50,  .75, .9,1), na.rm = TRUE)))
}

merge_table_default <- fread("merge_table.csv")
merge_table <- merge_table_default
sink("describe_dataset.txt")
print("General Base Variable Statistics")
print("--------------------------------------------------------------------------------")
print("Decriptive Statistics for Single Variables")
cat("\n")
print("Occupation Codes")
print(paste("number of unique occupation codes",length(unique(merge_table$OCC_CODE))))
print("distribution of unique occupation codes by # digits")
table(unique(merge_table[,c("OCC_CODE", "LEVEL_OCC_OE"), with = FALSE])$LEVEL_OCC_OE)
print(paste("overall distribution of occupation codes in table"))
table(merge_table$LEVEL_OCC_OE)
cat("\n\n")
print("Industry Codes")
print(paste("number of unique industry codes", length(unique(merge_table$NAICS))))
print(paste("distribution of unique industry codes by # digits"))
table(unique(merge_table[,c("OCC_CODE", "LEVEL_NAICS_OE"), with = FALSE])$LEVEL_NAICS_OE)
print(paste("overall distribution of industry codes in table"))
table(merge_table$LEVEL_NAICS_OE)
cat("\n\n")
print("Years")
print(paste("number of years",length(unique(merge_table$YEAR))))
print("table of years")
table(merge_table$YEAR)
cat("\n\n")
print("Area Codes")
print(paste("#number of area codes",length(unique(merge_table$AREA_CODE))))
temp <- merge_table[,c("AREA_CODE"), with = FALSE]
temp[AREA_CODE == 99, classification := "national"]
temp[is.na(classification) & AREA_CODE<100, classification := "state/territory"]
temp[is.na(classification) & AREA_CODE<100000 & AREA_CODE %% 10 == 0, classification := "city"]
temp[is.na(classification) & AREA_CODE<100000, classification := "metropolitan area"]
temp[is.na(classification), classification := "non metropolitan area"]
print("distribution of unique codes by area classification")
table(unique(temp[,c("AREA_CODE", "classification"), with = FALSE])$classification)
print("table of years")
table(temp$classification)
cat("\n\n")
print("2 variable pairs")
print(paste("number of unique industry-year pairs",
            dim(unique(merge_table[,c("NAICS", "YEAR"), with = FALSE]))[1]))
print(paste("number of unique industry-occupation pairs",
            dim(unique(merge_table[,c("NAICS", "OCC_CODE"), with = FALSE]))[1]))
print(paste("number of unique industry-area pairs",
            dim(unique(merge_table[,c("NAICS", "AREA_CODE"), with = FALSE]))[1]))
print(paste("number of unique year-occupation pairs",
            dim(unique(merge_table[,c("YEAR", "OCC_CODE"), with = FALSE]))[1]))
print(paste("number of unique year-area pairs",
            dim(unique(merge_table[,c("YEAR", "AREA_CODE"), with = FALSE]))[1]))
print(paste("number of unique occupation-area pairs",
            dim(unique(merge_table[,c("OCC_CODE", "AREA_CODE"), with = FALSE]))[1]))
cat("\n")
print("3 value pairs")
print(paste("number of unique industry-year-occupation groups",
            dim(unique(merge_table[,c("NAICS", "YEAR", "OCC_CODE"), with = FALSE]))[1]))
print(paste("number of unique industry-year-area groups",
            dim(unique(merge_table[,c("NAICS", "YEAR", "AREA_CODE"), with = FALSE]))[1]))
print(paste("number of unique industry-occupation-area groups",
            dim(unique(merge_table[,c("NAICS", "OCC_CODE", "AREA_CODE"), with = FALSE]))[1]))
print(paste("number of unique year-occupation-area groups",
            dim(unique(merge_table[,c("YEAR", "OCC_CODE", "AREA_CODE"), with = FALSE]))[1]))
cat("\n")
print("4 value pairs")
print(paste("number of unique industry-year-occupation-area groups",
            dim(unique(merge_table[,c("NAICS", "YEAR", "OCC_CODE", "AREA_CODE"), with = FALSE]))[1]))

cat("\n\n\n")
print("OEWS Data")
print("--------------------------------------------------------------------------------")
#occupational employment
OE_default <- c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE")
OE_desc <- read.xlsx("Merge/OEWS_desc.xlsx", 1, rowIndex = 9:32, colIndex = 1:2)
colnames(OE_desc) <- OE_desc[1,]
OE_desc <- data.table(OE_desc[-1,])
OE_select <- gsub(" ", "_", toupper(OE_desc[12:dim(OE_desc)[1],]$Field))
OE <- fread("Merge/OEWS_agg.csv", select = c(OE_default, OE_select))
merge_table_OE <- OE[merge_table, on = c(OE_default)]
merge_table_OE[,(OE_select):= lapply(.SD, as.numeric), .SDcols = OE_select]

for (col in OE_select) {
  summaryStats(c(merge_table_OE %>% select(col))[[1]], 
               paste("OE", col, OE_desc[Field == tolower(col)]$`Field Description`, sep = " - "))
  cat("\n")
}


cat("\n\n\n")
print("Labor Productivity Data")
print("--------------------------------------------------------------------------------")
#labor productivity
LP_default <- c("YEAR_IP", "NAICS_IP", "AREA_CODE_IP")
LP_desc <- fread("Merge/LP_desc.csv")
LP_select <- LP_desc$MEASURE_CODE_EXPANDED
IP <- fread("Merge/LP_agg.csv", select= c(LP_default,LP_select))
merge_table_IP <- IP[merge_table, on = c(LP_default)]
merge_table_IP[,(LP_select):= lapply(.SD, as.numeric), .SDcols = LP_select]
merge_table_IP[NAICS_IP == "", `NAICS_IP`:=NA]
print("general statistics")
cat("\n")
print("exact industry-year-area matches")
perfect_match <- dim(merge_table_IP[NAICS == NAICS_IP & YEAR == YEAR_IP & AREA_CODE == AREA_CODE_IP])[1]
perfect_match
perfect_match/dim(merge_table_IP)[1]
cat("\n")
print("aggregated industry-year-area matches (industry was aggregated)")
agg_match <- dim(merge_table_IP[LEVEL_NAICS_OE - LEVEL_NAICS_IP != 0])[1]
agg_match
agg_match/dim(merge_table_IP)[1]
print("table of the number of digits that industries were aggregated by")
table(merge_table_IP[LEVEL_NAICS_OE - LEVEL_NAICS_IP != 0, LEVEL_NAICS_OE - LEVEL_NAICS_IP])
cat("\n")
print("missing industry-year-area matches")
sum(is.na(merge_table_IP$NAICS_IP))
mean(is.na(merge_table_IP$NAICS_IP))
cat("\n")
print("Years covered")
sort(unique(merge_table_IP$YEAR_IP))
cat("\n")
cat("\n")
print("Measure Statistics")
for (col in LP_select) {
  summaryStats(c(merge_table_IP %>% select(col))[[1]], 
               paste("IP", col, 
                     LP_desc[MEASURE_CODE_EXPANDED == col, 
                             paste(MEASURE_TEXT, DURATION_TEXT)], sep = " - "))
  cat("\n")
}

cat("\n\n\n")
print("Employer Cost Data")
print("--------------------------------------------------------------------------------")
EC_default <- c("YEAR_CM", "INDUSTRY_CODE_CM", "OCCUPATION_CODE_CM", "AREA_CODE_CM")
EC_desc <- fread("Merge/EC_desc.csv")
EC_select <- EC_desc$estimate_datatype_code
EC <- fread("Merge/EC_agg.csv", select= c(EC_default,EC_select))
EC$INDUSTRY_CODE_CM <- as.numeric(EC$INDUSTRY_CODE_CM)
merge_table_EC <- EC[merge_table, on = c(EC_default)]
merge_table_EC[,(EC_select):= lapply(.SD, as.numeric), .SDcols = EC_select]
print("general statistics")
cat("\n")
print("exact industry-occupation-year-area matches")
perfect_match <- dim(merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) == 0 & 
                                      (LEVEL_OCC_OE - LEVEL_OCC_CM) == 0])[1]
perfect_match
perfect_match/dim(merge_table_EC)[1]
cat("\n")
print("aggregated industry-occuation-year-area matches (just industry was aggregated, occupation perfect match)")
agg_match_ind <- dim(merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) != 0 & 
                                      (LEVEL_OCC_OE - LEVEL_OCC_CM) == 0])[1]
agg_match_ind
agg_match_ind/dim(merge_table_EC)[1]
print("tanle of the number of industry digits that were aggregated")
table(merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) != 0 & 
                       (LEVEL_OCC_OE - LEVEL_OCC_CM) == 0, LEVEL_NAICS_OE - LEVEL_NAICS_CM])
cat("\n")
print("aggregated industry-occuation-year-area matches (just occupation was aggregated, industry perfect match)")
agg_match_occ <- dim(merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) == 0 & 
                                      (LEVEL_OCC_OE - LEVEL_OCC_CM) != 0])[1]
agg_match_occ
agg_match_occ/dim(merge_table_EC)[1]
print("table of the number of digits occupationswere aggregated by")
table(merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) == 0 & 
                       (LEVEL_OCC_OE - LEVEL_OCC_CM) != 0, LEVEL_OCC_OE - LEVEL_OCC_CM])
cat("\n")
print("aggregated industry-occuation-year-area matches (occupation & industry were both aggregated)")
temp_match <- merge_table_EC[(LEVEL_NAICS_OE - LEVEL_NAICS_CM) != 0 & 
                               (LEVEL_OCC_OE - LEVEL_OCC_CM) != 0]
temp_match[, `occ_dif`:= LEVEL_OCC_OE - LEVEL_OCC_CM]
temp_match[, `ind_dif`:= LEVEL_NAICS_OE - LEVEL_NAICS_CM]
agg_match_occ_ind <- dim(temp_match)[1]
agg_match_occ_ind
agg_match_occ_ind/dim(merge_table_EC)[1]
print("table of the number of digits occupation-industry pairs were aggregated by")
table(temp_match[,c("occ_dif", "ind_dif"), with = FALSE])
cat("\n")
print("missing industry-occupation-year-area matches")
sum(is.na(merge_table_EC$INDUSTRY_CODE_CM))
mean(is.na(merge_table_EC$INDUSTRY_CODE_CM))
cat("\n")
print("Years covered")
sort(unique(merge_table_EC$YEAR_CM))
cat("\n")
cat("\n")
print("Measure Statistics")
for (col in EC_select) {
  summaryStats(c(merge_table_EC %>% select(col))[[1]], 
               paste("CM", col, 
                     EC_desc[estimate_datatype_code == col, 
                             paste(estimate_text, datatype_text)], sep = " - "))
  cat("\n")
}

cat("\n\n\n")
print("Wage Model Data")
print("--------------------------------------------------------------------------------")
WM_default <- c("YEAR_WM", "OCCUPATION_CODE_WM", "AREA_CODE_WM")
WM_desc <- fread("Merge/WM_desc.csv", sep="^")
WM_select <- WM_desc$MEASURE
WM <- fread("Merge/WM_agg.csv", select= c(WM_default,WM_select))
merge_table_WM <- WM[merge_table, on = c(WM_default)]
merge_table_WM[,(WM_select):= lapply(.SD, as.numeric), .SDcols = WM_select]
print("general statistics")
cat("\n")
print("exact occupation-year-area matches")
perfect_match <- dim(merge_table_WM[LEVEL_OCC_OE - LEVEL_OCC_WM == 0])[1]
perfect_match
perfect_match/dim(merge_table_WM)[1]
cat("\n")
print("aggregated occuation-year-area matches")
agg_match_occ <- dim(merge_table_WM[LEVEL_OCC_OE - LEVEL_OCC_WM != 0])[1]
agg_match_occ
agg_match_occ/dim(merge_table_WM)[1]
print("table of the number of digits occupations were aggregated by")
table(merge_table_WM[LEVEL_OCC_OE - LEVEL_OCC_WM != 0, LEVEL_OCC_OE - LEVEL_OCC_WM])
cat("\n")
print("missing occupation-year-area matches")
sum(is.na(merge_table_WM$OCCUPATION_CODE_WM))
mean(is.na(merge_table_WM$OCCUPATION_CODE_WM))
cat("\n")
print("Years covered")
sort(unique(merge_table_WM$YEAR_WM))
cat("\n")
cat("\n")
print("Measure Statistics")
for (col in WM_select) {
  summaryStats(c(merge_table_WM %>% select(col))[[1]], 
               paste("LP", col, 
                     WM_desc[MEASURE == col, MEASURE], sep = " - "))
  cat("\n")
}
sink()





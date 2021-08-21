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

merge_table_default <- fread("merge_table.csv")
merge_table <- merge_table_default

OE_default <- c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE")
OE_select <- c("H_MEAN", "A_MEAN", "PCT_TOTAL", "TOT_EMP")
OE <- fread("Merge/OEWS_agg.csv", select = c(OE_default, OE_select))
merge_table <- OE[merge_table, on = c(OE_default)]
merge_table[,(OE_select):= lapply(.SD, as.numeric), .SDcols = OE_select]


LP_default <- c("YEAR_IP", "NAICS_IP", "AREA_CODE_IP")
LP_select <- c("L00_0_IP", "L00_1_IP", "L02_0_IP", "L02_1_IP", 
               "U10_0_IP", "U10_1_IP", "U12_0_IP", "U12_1_IP", 
               "T05_0_IP", "W20_0_IP")
IP <- fread("Merge/LP_agg.csv", select= c(LP_default,LP_select))
merge_table <- IP[merge_table, on = c(LP_default)]
merge_table[,(LP_select):= lapply(.SD, as.numeric), .SDcols = LP_select]

EC_default <- c("YEAR_CM", "INDUSTRY_CODE_CM", "OCCUPATION_CODE_CM", "AREA_CODE_CM")
EC_select <- c("01_D_CM", "01_L_CM", "02_D_CM", "02_L_CM")
EC <- fread("Merge/EC_agg.csv", select= c(EC_default,EC_select))
EC$INDUSTRY_CODE_CM <- as.numeric(EC$INDUSTRY_CODE_CM)
merge_table <- EC[merge_table, on = c(EC_default)]
merge_table[,(EC_select):= lapply(.SD, as.numeric), .SDcols = EC_select]

merge_table_analysis <- merge_table#[,.SD,.SDcols = c(LP_select, OE_select)]

setnames(merge_table_analysis, "H_MEAN", "hourly_wage")
setnames(merge_table_analysis, "A_MEAN", "annual_wage")
setnames(merge_table_analysis, "PCT_TOTAL", "percent_employed_industry")
setnames(merge_table_analysis, "TOT_EMP", "total_employment")

setnames(merge_table_analysis, "L00_0_IP", "productivity")
setnames(merge_table_analysis, "L00_1_IP", "productivity_pctchange")
setnames(merge_table_analysis, "L02_0_IP", "labor_compensation")
setnames(merge_table_analysis, "L02_1_IP", "labor_compensation_pctchange")
setnames(merge_table_analysis, "U10_0_IP", "unit_labor_costs")
setnames(merge_table_analysis, "U10_1_IP", "unit_labor_costs_pctchange")
setnames(merge_table_analysis, "U12_0_IP", "hourly_compensation")
setnames(merge_table_analysis, "U12_1_IP", "hourly_compensation_pctchange")
setnames(merge_table_analysis, "T05_0_IP", "output_deflator")
setnames(merge_table_analysis, "W20_0_IP", "industry_employment")

setnames(merge_table_analysis, "01_D_CM", "hourly_compensation_cost")
setnames(merge_table_analysis, "01_L_CM", "employee_hour_compensation_cost")
setnames(merge_table_analysis, "02_D_CM", "hourly_wage_cost")
setnames(merge_table_analysis, "02_L_CM", "employee_hour_wage_cost")


merge_table_analysis <- merge_table_analysis[order(OCC_CODE, NAICS, AREA_CODE, YEAR)]
#filter out occ-naics-area_code filters 
yearShift <- function(x) x - shift(x)
pctChange <- function(x) (x/shift(x) - 1) * 100
logRate <- function(x) log(x) - log(shift(x))
cols <- c("YEAR")
merge_table_analysis[, paste0("change", cols) := lapply(.SD, yearShift), 
                     by = c("OCC_CODE", "NAICS", "AREA_CODE"), .SDcols = cols]
merge_table_analysis[,`ordering`:= seq(.N),  by = c("OCC_CODE", "NAICS", "AREA_CODE")]
merge_table_analysis[changeYEAR == 1 | ordering == 1, cnt_continuous:=.N,  by = c("OCC_CODE", "NAICS", "AREA_CODE")]

merge_table_analysis[,cnt_overall:=.N, by = c("OCC_CODE", "NAICS", "AREA_CODE")]

# log_cols <- c("productivity", "labor_compensation", "unit_labor_costs", "hourly_compensation", 
#           "hourly_wage", "annual_wage", "hourly_compensation_cost", "employee_hour_compensation_cost",
#           "hourly_wage_cost","employee_hour_wage_cost")
pct_cols <- c("hourly_wage")
# merge_table_analysis[cnt_continuous == cnt_overall, paste0("lograte_", log_cols):= lapply(.SD, logRate),
#                      by = c("OCC_CODE", "NAICS", "AREA_CODE"), .SDcols = log_cols]
merge_table_analysis[cnt_continuous == cnt_overall, paste0("pctchange_", pct_cols):= lapply(.SD, pctChange),
                     by = c("OCC_CODE", "NAICS", "AREA_CODE"), .SDcols = pct_cols]

summaryStats <- function (x, name) {
  print(name)
  print(quantile(x, c(0,.1,.25, .50,  .75, .9,1), na.rm = TRUE))
  print(paste("Mean:", mean(x, na.rm = TRUE)))
  print(paste("SD:", sd(x, na.rm = TRUE)))
}

summaryStats(merge_table_analysis$productivity_pctchange, "Productivity")

ggplot(merge_table_analysis, aes(x=hourly_compensation_pctchange, y=productivity_pctchange)) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point')+
  #geom_point() +
  #geom_smooth(method=lm, se=FALSE) +
  geom_abline(intercept = 0, slope = 1, color = 'blue', size = 1) +
  ggtitle("productivity_pctchange ~ hourly_compensation_pctchange")
summary(lm(productivity_pctchange ~ hourly_compensation_pctchange, data = merge_table_analysis))
meaned <- merge_table_analysis[,lapply(.SD, function (x) mean(x, na.rm = TRUE)), 
                             by = c("YEAR", "NAICS"), 
                             .SDcols = c("hourly_compensation_pctchange", "productivity_pctchange")]
ggplot(meaned, aes(x=hourly_compensation_pctchange, y=productivity_pctchange)) +
  #stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point')+
  geom_point() +
  #geom_smooth(method=lm, se=FALSE) +
  geom_abline(intercept = 0, slope = 1, color = 'blue', size = 1) +
  ggtitle("productivity_pctchange ~ hourly_compensation_pctchange")
model <- lm(productivity_pctchange ~ hourly_compensation_pctchange, data = meaned)
summary(model)

#hourly compensation and productivity pct change analysis
analysis_subset <- copy(meaned)
analysis_subset[,`NAICS_2d`:=as.numeric(paste(substr(`NAICS`, 1, 2), "0000", sep = ""))]
analysis_subset[!is.na(hourly_compensation_pctchange) & !is.na(productivity_pctchange),
                `residual`:=model$residuals]
summaryStats(analysis_subset$residual, "residuals")
percentile_2_5 <-  quantile(analysis_subset$residual, c(.025), na.rm = TRUE)
print(paste("2.5 percentile", quantile(analysis_subset$residual, c(.025), na.rm = TRUE)))
percentile_97_5 <-  quantile(analysis_subset$residual, c(.975), na.rm = TRUE)
print(paste("97.5 percentile", quantile(analysis_subset$residual, c(.975), na.rm = TRUE)))

large_res <- analysis_subset[residual < percentile_2_5 | residual > percentile_97_5]
year_tb <- table(large_res$YEAR)/table(analysis_subset[YEAR %in% unique(large_res$YEAR), YEAR])
year_tb_norm <- year_tb/sum(year_tb) * 100
sort(year_tb_norm)

naics_tb <- sort(table(large_res$NAICS)/table(analysis_subset[NAICS %in% unique(large_res$NAICS), NAICS]))
naics_tb_norm <- naics_tb/sum(naics_tb) * 100
sort(naics_tb_norm)

naics2_tb <- sort(table(large_res$NAICS_2d)/table(analysis_subset[NAICS_2d %in% unique(large_res$NAICS_2d), NAICS_2d]))
naics2_tb_norm <- naics2_tb/sum(naics2_tb) * 100
sort(naics2_tb_norm)

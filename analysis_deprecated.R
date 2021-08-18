# Import libraries
library(data.table)
library(ggplot2)
library(Hmisc)
library(plm)
# disable scientific notation
options(scipen = 999)

setwd("~/Google Drive/Non-Academic Work/Research/Traina/occupation-productivity-2/")

#starting this year we have files
'%ni%' <- Negate('%in%')

merge_table <- fread("merge_table.csv")

#h_mean
OE_default <- c("YEAR", "NAICS", "OCC_CODE", "AREA_CODE")
OE_select <- c("H_MEAN", "A_MEAN")
OE <- fread("Merge/OEWS_agg.csv", select = c(OE_default, OE_select))
merge_table <- OE[merge_table, on = c(OE_default)]
merge_table[,(OE_select):= lapply(.SD, as.numeric), .SDcols = OE_select]

#L00 - productivity?
#L02, L06, L07, U11, U12, U13, U14, U15 - wages
LP_default <- c("YEAR_IP", "NAICS_IP", "AREA_CODE_IP")
LP_select <- c("L00_0_IP", "L02_0_IP", "L06_0_IP", "U11_0_IP", "U12_0_IP",
               "U13_0_IP", "U14_0_IP", "U15_0_IP")
IP <- fread("Merge/LP_agg.csv", select= c(LP_default,LP_select))
merge_table <- IP[merge_table, on = c(LP_default)]
merge_table <- merge_table[, lapply(.SD, as.numeric), by=LP_select]
merge_table[,(LP_select):= lapply(.SD, as.numeric), .SDcols = LP_select]

merge_table_analysis <- merge_table#[,.SD,.SDcols = c(LP_select, OE_select)]

setnames(merge_table_analysis, "H_MEAN", "hourly_wage")
setnames(merge_table_analysis, "A_MEAN", "annual_wage")

setnames(merge_table_analysis, "L00_0_IP", "productivity")
setnames(merge_table_analysis, "L02_0_IP", "labor_compensation_$")
setnames(merge_table_analysis, "L06_0_IP", "real_labor_compensation_$")
setnames(merge_table_analysis, "U11_0_IP", "labor_compensation_ind")
setnames(merge_table_analysis, "U12_0_IP", "hourly_compensation_$")
setnames(merge_table_analysis, "U13_0_IP", "hourly_compensation_ind")
setnames(merge_table_analysis, "U14_0_IP", "real_hourly_compensation_$")
setnames(merge_table_analysis, "U15_0_IP", "real_hourly_compensation_ind")

summaryStats <- function (x, name) {
  print(name)
  print(quantile(x, c(0,.1,.25, .50,  .75, .9,1), na.rm = TRUE))
  print(paste("Mean:", mean(x, na.rm = TRUE)))
  print(paste("SD:", sd(x, na.rm = TRUE)))
}

summaryStats(merge_table_analysis$productivity, "Productivity")
summaryStats(log(merge_table_analysis$productivity), "Log Productivity")

summaryStats(merge_table_analysis$hourly_wage, "Wages")
summaryStats(log(merge_table_analysis$hourly_wage), "Log Wages")

#log productivity
logged_prod <- ggplot(merge_table_analysis, aes(x=log(productivity),y=log(hourly_wage))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point')+
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("log(wage) ~ log(productivity)")
logged_prod
#ggsave("Plots/logged_prod.png", logged_prod)

#order swapped
logged_prod_reverse <- ggplot(merge_table_analysis, aes(x=log(hourly_wage),y=log(productivity))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point')+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("log(productivity) ~ log(wage)")
logged_prod_reverse
#ggsave("Plots/logged_prod_reversed.png", logged_prod_reverse)

hourly_wage_prod <- merge_table_analysis[!is.na(hourly_wage) & !is.na(productivity) & !is.na(NAICS_IP)]
summary(lm(formula = log(hourly_wage) ~ log(productivity), data = hourly_wage_prod))
summary(lm(formula = log(productivity) ~ log(hourly_wage), data = hourly_wage_prod))
summary(lm(formula = log(hourly_wage) ~ log(productivity) + factor(YEAR), data = hourly_wage_prod))
summary(lm(formula = log(productivity) ~ log(hourly_wage) + factor(YEAR), data = hourly_wage_prod))
summary(plm(log(productivity) ~ log(hourly_wage),
            data = hourly_wage_prod,
            index = c("NAICS_IP"), model = "within"))
summary(plm(log(hourly_wage) ~ log(productivity),
            data = hourly_wage_prod,
            index = c("NAICS_IP"), model = "within"))

lm(formula = log(hourly_wage) ~ log(productivity) + factor(NAICS_IP), data = merge_table_analysis)


mean_ind <- merge_table_analysis[,lapply(.SD, function (x) mean(x, na.rm = TRUE)), 
                                 .SDcols = c("productivity", "hourly_wage"), by = c("NAICS_IP", "YEAR")]
ggplot(mean_ind, aes(x=log(hourly_wage),y=log(productivity), color = YEAR)) + 
  geom_point() + geom_smooth(method=lm)
lm(formula = log(productivity) ~ log(hourly_wage), data = mean_ind)

#insufficient data
ggplot(merge_table_analysis, aes(x=log(`labor_compensation_$`),
                                 y=log(productivity))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point') +
  geom_smooth(method = lm)
lm(formula = log(productivity) ~ log(`labor_compensation_$`), data = merge_table_analysis)

ggplot(merge_table_analysis, aes(x=log(`real_labor_compensation_$`),
                                 y=log(productivity))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point') +
  geom_smooth(method = lm)
lm(formula = log(productivity) ~ log(`real_labor_compensation_$`), data = merge_table_analysis)

ggplot(merge_table_analysis, aes(x=log(`labor_compensation_ind`),
                                 y=log(productivity))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point') +
  geom_smooth(method = lm)
lm(formula = log(productivity) ~ log(`labor_compensation_ind`), data = merge_table_analysis)

ggplot(merge_table_analysis, aes(x=log(`hourly_compensation_$`),
                                 y=log(productivity))) +
  stat_summary_bin(fun='mean', bins=100, color='orange', size=2, geom='point') +
  geom_smooth(method = lm)
lm(formula = log(productivity) ~ log(`hourly_compensation_$`), data = merge_table_analysis)

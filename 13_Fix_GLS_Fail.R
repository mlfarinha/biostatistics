######################################################
###### 13. SCRIPT TO IMPROVE GLS MODEL (FAILED) ######
######################################################

# --------------------------------------------------------------------------
# 1. Import the necessary data
# 2. Build the dataset
# 3. Fitting GLS with relevant predictors
# 4. Analysed VIF of covariates and removed ones with highest value (failed)
# 5. Log of response variable (failed and not presented here)
#
# Miguel Farinha 
# (MMA BioStatistics June 2021)
# --------------------------------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(RColorBrewer)
library(VIM)
library(DataExplorer)
library(lubridate)
library(tidyr)
library(scales)
library(ggplot2)
library(viridis)
library(PerformanceAnalytics)
library(reshape2)
library(hrbrthemes)
library(gplots)
library(ggpubr)
library(car)
library(HH)
library(stats)
library(MARSS)
library(pracma)
library(lmtest)
library(zoo)
library(urca)
library(kdensity)
library(SkewHyperbolic)
library(EQL)
library(extraDistr)
library(purrr)
library(kedd)
library(leaps)
library(nlme)
library(olsrr)
library(MASS)
library(glmulti)
library(qqplotr)
library(forecast)
library(rcompanion)
library(Hmisc)
library(COVID19) # Package with COVID-19 data

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

# LOAD HYPERTENSION DATA -------------------------------------------------------------------------------------------

# Import the dataset hypertension
hyper <- read.table("hipertensao.csv", sep = ";", header = TRUE)

# Change the names of columns and set the appropriate datatype for each column of the data
colnames(hyper)
colnames(hyper)[1] <- "Period"
colnames(hyper)[2] <- "Region"
colnames(hyper)[3] <- "Entity"
colnames(hyper)[4] <- "GPS"
colnames(hyper)[5] <- "BP"
colnames(hyper)[6] <- "Prop"

# as.Date requires a day, thus we consider a new registry occurs on the 1st day of each month
hyper$Period <- as.Date(paste0(hyper$Period,"-15"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
unique(hyper$Region)
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("RegiÃ£o", "Região", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("SaÃºde", "Saúde", x)
}))
unique(hyper$Region)
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("Região de Saúde do Alentejo", "Alentejo", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("Região de Saúde do Algarve", "Algarve", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("Região de Saúde do Centro", "Centro", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("Região de Saúde LVT", "LVT", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("Região de Saúde Norte", "Norte", x)
}))
unique(hyper$Region)
hyper$Region <- as.character(hyper$Region)
hyper$BP <- as.numeric(hyper$BP)
hyper$Prop <- as.numeric(hyper$Prop)

# Check datatypes and summary of data
hyper$Period <- as.Date(hyper$Period, format="%Y-%m-%d", tz="GMT")
min(hyper$Period) # oldest registration: "2014-01"
max(hyper$Period) # newest registration: "2021-02"
# The data has no missing values

# Add year variable
hyper$Year <- year(hyper$Period)

# Consider only period from  March 2020 - February 2021
hyper <- hyper[which(hyper$Period > "2020-03-01"),]
min(hyper$Period)

# Create month variables, variable with total of patients (denom) and percentage from proportion
hyper$month <- month(hyper$Period, abbr = FALSE, label = FALSE)
hyper <- transform(hyper, denom = BP/(Prop/100)) # Use formula at https://transparencia.sns.gov.pt/
hyper <- transform(hyper, percent = Prop/100) # Percentage variable from proportion (values between 0 and 1)
summary(hyper)

# Create dataset by ARS, Month and Year
Sys.setlocale("LC_TIME", "C")
hyper_g <- 
  hyper %>%
  group_by(hyper$Region, month(hyper$Period, label = TRUE, abbr = FALSE), year(hyper$Period)) %>% 
  summarise(meanBP = mean(BP, na.rm = TRUE),
            meanProp = mean(Prop, na.rm = TRUE),
            meanDenom = mean(denom, na.rm = TRUE),
            meanPercent = mean(percent, na.rm = TRUE))
colnames(hyper_g)[1] <- "ARS"
colnames(hyper_g)[2] <- "Month"
colnames(hyper_g)[3] <- "Year"
hyper_g <- hyper_g[order(hyper_g[,1], hyper_g[,3]), ]
str(hyper_g)

# Add Date variable for future use
hyper_g <- transform(hyper_g, Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))
hyper_g$nMonth <- month(hyper_g$Date, abbr = FALSE, label = FALSE) # number of the month
hyper_g <- transform(hyper_g, Time = as.numeric(Date) / 1000) # time variable (numeric)

# Create factor variables for categorical variables year and number of month
hyper_g$nmonth.f <- factor(hyper_g$nMonth)
is.factor(hyper_g$nmonth.f)
hyper_g$year.f <- factor(hyper_g$Year)
is.factor(hyper_g$year.f)


# LOAD DATA FROM COVID19 PACKAGE -----------------------------------------------------------------------------------
# Source: https://covid19datahub.io/

covid_lvl2 <- covid19(country = "Portugal", level = 2)
covid_lvl2$administrative_area_level_2 # ARS information
# Select only ARS Lisboa, Centro, Norte, Alentejo, Algarve (exclude Açores and Madeira)
covid_lvl2 <- filter(covid_lvl2, administrative_area_level_2 != "Madeira" & administrative_area_level_2 != "Açores")
covid_lvl2 <- covid_lvl2[which(covid_lvl2$date < "2021-03-01"),]
min(covid_lvl2$date) # "2020-02-26"
max(covid_lvl2$date) # "2021-02-28"

# Stringency index calculated as in https://www.bsg.ox.ac.uk/sites/default/files/2020-04/BSG-WP-2020-032-v5.0.pdf
unique(covid_lvl1$stringency_index)

# Select period from March 2020 - February 2021
covid_lvl2 <- covid_lvl2[which(covid_lvl2$date > "2020-02-29"), ]
min(covid_lvl2$date)

# Construct dataset with variables of interest (monthly data due to hypertension dataset)
# Select the median for categorical variables
Sys.setlocale("LC_TIME", "C")
covid_supp <-
  covid_lvl2 %>%
  group_by(administrative_area_level_2,month(covid_lvl2$date, label = TRUE, abbr = FALSE),year(covid_lvl2$date)) %>% 
  summarise(med_school_closing = median(school_closing, na.rm = TRUE),
            med_workplace_closing = median(workplace_closing, na.rm = TRUE),
            med_cancel_events = median(cancel_events, na.rm = TRUE),
            med_gatherings_restrictions = median(gatherings_restrictions, na.rm = TRUE),
            med_transport_closing = median(transport_closing, na.rm = TRUE),
            med_stay_home_restrictions = median(stay_home_restrictions, na.rm = TRUE),
            med_internal_movement_restrictions = median(internal_movement_restrictions, na.rm = TRUE),
            mean_stringency_index = mean(stringency_index, na.rm = TRUE))
colnames(covid_supp)[1] <- "ARS"
colnames(covid_supp)[2] <- "Month"
colnames(covid_supp)[3] <- "Year"
covid_supp <- covid_supp[order(covid_supp[,1], covid_supp[,3]), ]

# The data contains some NA values which will be dealt with manually according to official information
# covid_supp[which(covid_supp$Month == "February" & covid_supp$Year == "2020"), c(4:9)] <- 0 (if February included)
covid_final <- na.locf(covid_supp, na.rm=FALSE) # impute previous value in NA
sum(is.na(covid_final))
covid_final <- data.frame(lapply(covid_final, function(x) {
  gsub("Lisboa", "LVT", x)
}))


# LOAD DATA RELATIVE TO PRIMARY CARE -------------------------------------------------------------------------------
# Source: https://transparencia.sns.gov.pt/explore/dataset/monitorizacao-sazonal-csp/table/?disjunctive.ars&sort=dia
Sys.setlocale("LC_TIME", "C")

# Import the dataset monitorizacao_sazonal_csp
csp_s <- read.table("monitorizacao_sazonal_csp.csv", sep = ";", header = TRUE)
csp_s <- csp_s[,c(1,2,3)]

# Change the names of columns and set the appropriate datatype for each column of the data
colnames(csp_s)
colnames(csp_s)[1] <- "Period"
colnames(csp_s)[2] <- "ARS"
colnames(csp_s)[3] <- "NumberCSP"

# as.Date
csp_s$Period <- as.Date(csp_s$Period, format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
unique(csp_s$ARS)
csp_s <- data.frame(lapply(csp_s, function(x) {
  gsub("ARS Alentejo", "Alentejo", x)
}))
csp_s <- data.frame(lapply(csp_s, function(x) {
  gsub("ARS Algarve", "Algarve", x)
}))
csp_s <- data.frame(lapply(csp_s, function(x) {
  gsub("ARS Centro", "Centro", x)
}))
csp_s <- data.frame(lapply(csp_s, function(x) {
  gsub("ARS Lisboa e Vale do Tejo", "LVT", x)
}))
csp_s <- data.frame(lapply(csp_s, function(x) {
  gsub("ARS Norte", "Norte", x)
}))
csp_s$ARS <- as.character(csp_s$ARS)
csp_s$NumberCSP <- as.numeric(csp_s$NumberCSP)

# Check datatypes and summary of data
csp_s$Period <- as.Date(csp_s$Period, format="%Y-%m-%d", tz="GMT")
min(csp_s$Period) # oldest registration: "2016-11-01"
max(csp_s$Period) # newest registration: "2021-06-14"
sum(is.na(csp_s)) # The data has no missing values

# Add year variable
csp_s$Year <- year(csp_s$Period)

# Consider only period from February 2020 - February 2021
csp_s <- csp_s[which(csp_s$Period > "2020-02-29" & csp_s$Period < "2021-03-01"),]

# Create month variables, variable with total of patients (denom) and percentage from proportion
csp_s$month <- month(csp_s$Period, abbr = FALSE, label = FALSE)

# Create dataset by ARS, Month and Year
Sys.setlocale("LC_TIME", "C")
csp1 <- 
  csp_s %>%
  group_by(csp_s$ARS, month(csp_s$Period, label = TRUE, abbr = FALSE), year(csp_s$Period)) %>% 
  summarise(meanCSP = mean(NumberCSP, na.rm = TRUE))
colnames(csp1)[1] <- "ARS"
colnames(csp1)[2] <- "Month"
colnames(csp1)[3] <- "Year"
csp1 <- csp1[order(csp1[,1], csp1[,3]),]


# LOAD DATA RELATIVE TO EVOLUTION OF PRIMARY CARE APPOINTMENTS ---------------------------------------------------- 
# Source: https://transparencia.sns.gov.pt/explore/dataset/evolucao-das-consultas-medicas-nos-csp/table/?sort=tempo
Sys.setlocale("LC_TIME", "C")

# Import the dataset evolucao_consultas_medicas_csp
csp_e <- read.table("evolucao_consultas_medicas_csp.csv", sep = ";", header = TRUE)
csp_e <- csp_e[,c(1,2,5,6,7)]

# Change the names of columns and set the appropriate datatype for each column of the data
colnames(csp_e)
colnames(csp_e)[1] <- "Period"
colnames(csp_e)[2] <- "ARS"
colnames(csp_e)[3] <- "CSPpresent"
colnames(csp_e)[4] <- "CSPabsent"
colnames(csp_e)[5] <- "CSPhome"

# as.Date requires a day, thus we consider a new registry occurs on the 1st day of each month
csp_e$Period <- as.Date(paste0(csp_e$Period,"-15"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
unique(csp_e$ARS)
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("RegiÃ£o", "Região", x)
}))
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("SaÃºde", "Saúde", x)
}))
unique(csp_e$ARS)
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("Região de Saúde do Alentejo", "Alentejo", x)
}))
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("Região de Saúde do Algarve", "Algarve", x)
}))
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("Região de Saúde do Centro", "Centro", x)
}))
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("Região de Saúde LVT", "LVT", x)
}))
csp_e <- data.frame(lapply(csp_e, function(x) {
  gsub("Região de Saúde Norte", "Norte", x)
}))
csp_e$ARS <- as.character(csp_e$ARS)
csp_e$CSPpresent <- as.numeric(csp_e$CSPpresent)
csp_e$CSPabsent <- as.numeric((csp_e$CSPabsent))
csp_e$CSPhome <- as.numeric(csp_e$CSPhome)

# Check datatypes and summary of data
csp_e$Period <- as.Date(paste0(csp_e$Period,"-15"), format="%Y-%m-%d", tz="GMT")
min(csp_e$Period) # oldest registration: "2014-01"
max(csp_e$Period) # newest registration: "2021-02"
sum(is.na(csp_e)) # The data has 1 NA

# Add year variable
csp_e$Year <- year(csp_e$Period)

# Consider only period from February 2020 - February 2021
csp_e <- csp_e[which(csp_e$Period > "2020-03-01"),]

# Create month variables, variable with total of patients (denom) and percentage from proportion
csp_e$month <- month(csp_e$Period, abbr = FALSE, label = FALSE)

# Create dataset by ARS, Month and Year
Sys.setlocale("LC_TIME", "C")
csp2 <- 
  csp_e %>%
  group_by(csp_e$ARS, month(csp_e$Period, label = TRUE, abbr = FALSE), year(csp_e$Period)) %>% 
  summarise(meanCSPpresent = mean(CSPpresent, na.rm = TRUE),
            meanCSPabsent = mean(CSPabsent, na.rm = TRUE),
            meanCSPhome = mean(CSPhome, na.rm = TRUE))
colnames(csp2)[1] <- "ARS"
colnames(csp2)[2] <- "Month"
colnames(csp2)[3] <- "Year"
csp2 <- csp2[order(csp2[,1], csp2[,3]),]


# MERGE DATASETS ---------------------------------------------------------------------------------------------------

# There are 4 datasets of interest: hyper_g, covid_final, csp1, csp2
total <- Reduce(function(x, y) merge(x, y, all=TRUE), list(hyper_g, covid_final, csp1, csp2))
total <- total[order(total[,1], total[,3], total[,2]),]
total <- transform(total, ARSf = factor(ARS),
                   Timevar = as.numeric(Time),
                   med_school_closing = factor(med_school_closing),
                   med_workplace_closing = factor(med_workplace_closing),
                   med_cancel_events = factor(med_cancel_events),
                   med_gatherings_restrictions = factor(med_gatherings_restrictions),
                   med_transport_closing = factor(med_transport_closing),
                   med_stay_home_restrictions = factor(med_stay_home_restrictions),
                   med_internal_movement_restrictions = factor(med_internal_movement_restrictions),
                   mean_stringency_index = as.numeric(mean_stringency_index))
str(total)


# BUILD DATASETS FOR EACH ARS --------------------------------------------------------------------------------------
alentejo <- total[which(total$ARS == "Alentejo"),]
algarve <- total[which(total$ARS == "Algarve"),]
centro <- total[which(total$ARS == "Centro"),]
lvt <- total[which(total$ARS == "LVT"),]
norte <- total[which(total$ARS == "Norte"),]

rm("csp_e"); rm("csp_s"); rm("covid_lvl2"); rm("covid_supp"); rm("hyper")
rm("csp1"); rm("csp2"); rm("hyper_g"); rm("covid_final")

####################################################################################################################
# The datasets will not be split into training and testing as we are not interested in prediction
# Moreover, the datasets for each ARS have few observations since we are working with monthly data
# We will model the meanBP, i.e., the mean number of patients with HP aged < 65 yrs for each month

# KERNEL DENSITY ESTIMATION ----------------------------------------------------------------------------------------
h.amise(total$meanBP, kernel = "epanechnikov", deriv.order = 0)
h.amise(total$meanBP, kernel = "gaussian", deriv.order = 0)
par(mfrow = c(1,2))
kde_normal <- kdensity(total$meanBP, kernel = "gaussian", bw = "nrd")
kde_normal.plot <- plot(kde_normal , lwd = 2, col = "blue", main = "meanBP kernel density estimation",
                        cex.lab = 1.1, bty = "n")
hist.response <- hist(total$meanBP, breaks = "FD", bty = "n", cex.lab = 1.1,
                      main = "Histogram of meanBP", xlab = "meanBP", ylab = "Frequency")
# Density estimation using basic methods
ruff.density <- plot(density(total$meanBP), lwd = 2, col = "red", cex.lab = 1.1,
                     main = "Simple density estimation", bty = "n")
# Use family = gaussian when modeling

# SIMPLE FULL AND REDUCED MODELS -----------------------------------------------------------------------------------

# Model with time variables and ARS
time1 <- lm(meanBP ~  nmonth.f + ARSf, data = total, weights = meanPercent)
summary(time1)
plot(time1)
rm("time1")

# M0: med_school_closing
M0_alentejo <- lm(meanBP ~ med_school_closing, data = alentejo, weights = meanPercent); summary(M0_alentejo)
M0_algarve <- lm(meanBP ~ med_school_closing, data = algarve, weights = meanPercent); summary(M0_algarve)
M0_centro <- lm(meanBP ~ med_school_closing, data = centro, weights = meanPercent); summary(M0_centro)
M0_lvt <- lm(meanBP ~ med_school_closing, data = lvt, weights = meanPercent); summary(M0_lvt)
M0_norte <- lm(meanBP ~ med_school_closing, data = norte, weights = meanPercent); summary(M0_norte)
M0_all <- lm(meanBP ~ med_school_closing + ARSf, data = total, weights = meanPercent); summary(M0_all)
acf(M0_all$resid); AIC(M0_all)
# The individual models were not significant, med_workplace_closing may be a weak predictor

rm("M0_alentejo", "M0_algarve", "M0_centro", "M0_lvt", "M0_norte")

# M1: med_workplace_closing
M1_alentejo <- lm(meanBP ~ med_workplace_closing, data = alentejo, weights = meanPercent); summary(M1_alentejo)
M1_algarve <- lm(meanBP ~ med_workplace_closing, data = algarve, weights = meanPercent); summary(M1_algarve)
M1_centro <- lm(meanBP ~ med_workplace_closing, data = centro, weights = meanPercent); summary(M1_centro)
M1_lvt <- lm(meanBP ~ med_workplace_closing, data = lvt, weights = meanPercent); summary(M1_lvt)
M1_norte <- lm(meanBP ~ med_workplace_closing, data = norte, weights = meanPercent); summary(M1_norte)
M1_all <- lm(meanBP ~ med_workplace_closing + ARSf, data = total, weights = meanPercent); summary(M1_all)
acf(M1_all$resid); AIC(M1_all)
# The individual models were not significant, med_workplace_closing may be a weak predictor

rm("M1_alentejo", "M1_algarve", "M1_centro", "M1_lvt", "M1_norte")

# M2: med_cancel_events DUE TO USING MEDIAN WE ONLY HAVE 1 LEVEL
# M2_alentejo <- lm(meanBP ~ med_cancel_events, data = alentejo, weights = meanPercent); summary(M2_alentejo)
# M2_algarve <- lm(meanBP ~ med_cancel_events, data = algarve, weights = meanPercent); summary(M2_algarve)
# M2_centro <- lm(meanBP ~ med_cancel_events, data = centro, weights = meanPercent); summary(M2_centro)
# M2_lvt <- lm(meanBP ~ med_cancel_events, data = lvt, weights = meanPercent); summary(M2_lvt)
# M2_norte <- lm(meanBP ~ med_cancel_events, data = norte, weights = meanPercent); summary(M2_norte)
# M2_all <- lm(meanBP ~ med_cancel_events + ARSf, data = total, weights = meanPercent); summary(M2_all)
# The individual models were not significant, med_cancel_events may be a weak predictor
# med_cancel_events has the same level for all months except February thus rendering it useless

# M3: med_gatherings_restrictions DUE TO USING MEDIAN WE ONLY HAVE 1 LEVEL
# M3_alentejo <- lm(meanBP ~ med_gatherings_restrictions, data = alentejo, weights = meanPercent); summary(M3_alentejo)
# M3_algarve <- lm(meanBP ~ med_gatherings_restrictions, data = algarve, weights = meanPercent); summary(M3_algarve)
# M3_centro <- lm(meanBP ~ med_gatherings_restrictions, data = centro, weights = meanPercent); summary(M3_centro)
# M3_lvt <- lm(meanBP ~ med_gatherings_restrictions, data = lvt, weights = meanPercent); summary(M3_lvt)
# M3_norte <- lm(meanBP ~ med_gatherings_restrictions, data = norte, weights = meanPercent); summary(M3_norte)
# M3_all <- lm(meanBP ~ med_gatherings_restrictions + ARSf, data = total, weights = meanPercent); summary(M3_all)
# The individual models were not significant, med_gathering_restrictions may be a weak predictor
# med_gathering_restrictions has the same level for all months except February thus rendering it useless

# M4: med_transport_closing DUE TO USING MEDIAN WE ONLY HAVE 1 LEVEL
# M4_alentejo <- lm(meanBP ~ med_transport_closing, data = alentejo, weights = meanPercent); summary(M4_alentejo)
# M4_algarve <- lm(meanBP ~ med_transport_closing, data = algarve, weights = meanPercent); summary(M4_algarve)
# M4_centro <- lm(meanBP ~ med_transport_closing, data = centro, weights = meanPercent); summary(M4_centro)
# M4_lvt <- lm(meanBP ~ med_transport_closing, data = lvt, weights = meanPercent); summary(M4_lvt)
# M4_norte <- lm(meanBP ~ med_transport_closing, data = norte, weights = meanPercent); summary(M4_norte)
# M4_all <- lm(meanBP ~ med_transport_closing + ARSf, data = total, weights = meanPercent); summary(M4_all)
# The individual models were not significant, med_transport_closing may be a weak predictor
# med_transport_closing has the same level for all months except February thus rendering it useless

# M5: med_stay_home_restrictions
M5_alentejo <- lm(meanBP ~ med_stay_home_restrictions, data = alentejo, weights = meanPercent); summary(M5_alentejo)
M5_algarve <- lm(meanBP ~ med_stay_home_restrictions, data = algarve, weights = meanPercent); summary(M5_algarve)
M5_centro <- lm(meanBP ~ med_stay_home_restrictions, data = centro, weights = meanPercent); summary(M5_centro)
M5_lvt <- lm(meanBP ~ med_stay_home_restrictions, data = lvt, weights = meanPercent); summary(M5_lvt)
M5_norte <- lm(meanBP ~ med_stay_home_restrictions, data = norte, weights = meanPercent); summary(M5_norte)
M5_all <- lm(meanBP ~ med_stay_home_restrictions + ARSf, data = total, weights = meanPercent); summary(M5_all)
acf(M5_all$resid)
# The individual models were not significant, med_stay_home_restrictions may be a weak predictor
# Slight improvement compared to previous models; med_stay_home_restrictions1 is significant at 1%, 5% and 10%

rm("M5_alentejo", "M5_algarve", "M5_centro", "M5_lvt", "M5_norte")

# M6: med_internal_movement_restrictions
M6_alentejo<-lm(meanBP~med_internal_movement_restrictions, data = alentejo,weights=meanPercent);summary(M6_alentejo)
M6_algarve<-lm(meanBP~med_internal_movement_restrictions, data = algarve, weights = meanPercent);summary(M6_algarve)
M6_centro <- lm(meanBP ~ med_internal_movement_restrictions, data = centro, weights = meanPercent);summary(M6_centro)
M6_lvt <- lm(meanBP ~ med_internal_movement_restrictions, data = lvt, weights = meanPercent);summary(M6_lvt)
M6_norte <- lm(meanBP ~ med_internal_movement_restrictions, data = norte, weights = meanPercent);summary(M6_norte)
M6_all <- lm(meanBP ~ med_internal_movement_restrictions + ARSf, data = total, weights = meanPercent);summary(M6_all)
acf(M6_all$resid)
# The individual models were not significant, med_internal_movement_restrictions may be a weak predictor
# M6_all worst than M5_all, med_internal_movement_restrictions less predictive than med_stay_home_restrictions

rm("M6_alentejo", "M6_algarve", "M6_centro", "M6_lvt", "M6_norte")

# M7: mean_stringency_index
M7_alentejo <- lm(meanBP ~ mean_stringency_index, data = alentejo, weights=meanPercent); summary(M7_alentejo)
M7_algarve <- lm(meanBP ~ mean_stringency_index, data = algarve, weights = meanPercent); summary(M7_algarve)
M7_centro <- lm(meanBP ~ mean_stringency_index, data = centro, weights = meanPercent); summary(M7_centro)
M7_lvt <- lm(meanBP ~ mean_stringency_index, data = lvt, weights = meanPercent); summary(M7_lvt)
M7_norte <- lm(meanBP ~ mean_stringency_index, data = norte, weights = meanPercent); summary(M7_norte)
M7_all <- lm(meanBP~mean_stringency_index+ARSf,data=total,weights = meanPercent);summary(M7_all);acf(M7_all$resid)
# mean_stringency_index is not significant in any of the models

rm("M7_alentejo", "M7_algarve", "M7_centro", "M7_lvt", "M7_norte")

# I1: meanCSP
I1_alentejo <- lm(meanBP ~ meanCSP, data = alentejo, weights=meanPercent); summary(I1_alentejo)
I1_algarve <- lm(meanBP ~ meanCSP, data = algarve, weights = meanPercent); summary(I1_algarve)
I1_centro <- lm(meanBP ~ meanCSP, data = centro, weights = meanPercent); summary(I1_centro)
I1_lvt <- lm(meanBP ~ meanCSP, data = lvt, weights = meanPercent); summary(I1_lvt)
I1_norte <- lm(meanBP ~ meanCSP, data = norte, weights = meanPercent); summary(I1_norte)
I1_all <- lm(meanBP ~ meanCSP + ARSf, data = total, weights = meanPercent); summary(I1_all); acf(I1_all$resid)
# meanCSP is not significant in any of the models

rm("I1_alentejo", "I1_algarve", "I1_centro", "I1_lvt", "I1_norte")

# I2: meanCSPpresent
I2_alentejo <- lm(meanBP ~ meanCSPpresent, data = alentejo, weights=meanPercent); summary(I2_alentejo)
I2_algarve <- lm(meanBP ~ meanCSPpresent, data = algarve, weights = meanPercent); summary(I2_algarve)
I2_centro <- lm(meanBP ~ meanCSPpresent, data = centro, weights = meanPercent); summary(I2_centro)
I2_lvt <- lm(meanBP ~ meanCSPpresent, data = lvt, weights = meanPercent); summary(I2_lvt)
I2_norte <- lm(meanBP ~ meanCSPpresent, data = norte, weights = meanPercent); summary(I2_norte)
I2_all <- lm(meanBP ~ meanCSPpresent + ARSf, data = total, weights = meanPercent);summary(I2_all);acf(I2_all$resid)
# meanCSPpresent is significant at 5% and 10% significance levels

rm("I2_alentejo", "I2_algarve", "I2_centro", "I2_lvt", "I2_norte")

# I3: meanCSPabsent
I3_alentejo <- lm(meanBP ~ meanCSPabsent, data = alentejo, weights=meanPercent); summary(I3_alentejo)
I3_algarve <- lm(meanBP ~ meanCSPabsent, data = algarve, weights = meanPercent); summary(I3_algarve)
I3_centro <- lm(meanBP ~ meanCSPabsent, data = centro, weights = meanPercent); summary(I3_centro)
I3_lvt <- lm(meanBP ~ meanCSPabsent, data = lvt, weights = meanPercent); summary(I3_lvt)
I3_norte <- lm(meanBP ~ meanCSPabsent, data = norte, weights = meanPercent); summary(I3_norte)
I3_all <- lm(meanBP ~ meanCSPabsent + ARSf, data = total, weights = meanPercent);summary(I3_all);acf(I3_all$resid)
# meanCSPabsent is not significant in any of the models

rm("I3_alentejo", "I3_algarve", "I3_centro", "I3_lvt", "I3_norte")

# I4: meanCSPhome
I4_alentejo <- lm(meanBP ~ meanCSPhome, data = alentejo, weights=meanPercent); summary(I4_alentejo)
I4_algarve <- lm(meanBP ~ meanCSPhome, data = algarve, weights = meanPercent); summary(I4_algarve)
I4_centro <- lm(meanBP ~ meanCSPhome, data = centro, weights = meanPercent); summary(I4_centro)
I4_lvt <- lm(meanBP ~ meanCSPhome, data = lvt, weights = meanPercent); summary(I4_lvt)
I4_norte <- lm(meanBP ~ meanCSPhome, data = norte, weights = meanPercent); summary(I4_norte)
I4_all <- lm(meanBP ~ meanCSPhome + ARSf, data = total, weights = meanPercent);summary(I4_all); acf(I4_all$resid)
# meanCSPhome is is significant at 5% and 10% significance levels

rm("I4_alentejo", "I4_algarve", "I4_centro", "I4_lvt", "I4_norte")

# F: Full model
F_alentejo <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
                   med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
                   meanCSPabsent + meanCSPhome,
                 data = alentejo, weights = meanPercent); summary(F_alentejo); acf(F_alentejo$residuals)
# model is not significant + parameters do not improve model
F_algarve <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
                  med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
                  meanCSPabsent + meanCSPhome,
                data = alentejo, weights = meanPercent); summary(F_algarve); acf(F_algarve$residuals)
# model is not significant + parameters do not improve model
F_centro <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
                 med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
                 meanCSPabsent + meanCSPhome,
               data = alentejo, weights = meanPercent); summary(F_centro); acf(F_centro$residuals)
# model is not significant + parameters do not improve model
F_lvt <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
              med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
              meanCSPabsent + meanCSPhome,
            data = alentejo, weights = meanPercent); summary(F_lvt); acf(F_lvt$residuals)
# model is not significant + parameters do not improve model
F_norte <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
                med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
                meanCSPabsent + meanCSPhome,
              data = alentejo, weights = meanPercent); summary(F_norte); acf(F_norte$residuals)
# model is not significant + parameters do not improve model
rm("F_alentejo"); rm("F_algarve"); rm("F_centro"); rm("F_lvt"); rm("F_norte")
F_all <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
              med_internal_movement_restrictions + mean_stringency_index + meanCSP + meanCSPpresent +
              meanCSPabsent + meanCSPhome + ARSf,
            data = total, weights = meanPercent); summary(F_all)
# med_cancel_events + med_gatherings_restrictions + med_transport_closing not significant
# med_stay_home_restrictions2 + med_internal_movement_restrictions2 not significant
# model is significant but most of the variables could be discarded
plot(F_all)
anova(F_all)
extractAIC(F_all) # AIC: 701.9876
extractAIC(F_all, k = log(dim(total)[1])) #BIC: 733.4028
sum((F_all$residuals/( 1-hatvalues(F_all)))^2) #PRESS: 52241500

# Diagnostic of FULL MODEL
t.test(F_all$residuals, mu = 0) # p-value = 0.1163 (check after confirming residuals normality)
ggplot(F_all, aes(.fitted, .resid)) + 
  geom_point(color="blue", alpha = 0.4) +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) + 
  labs(x="Predicted Values", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)

# Breusch-pagan test for homocedasticity
bptest(F_all) # p = 0.01073 -> homocedasticity of residuals
ncvTest(F_all) # p = 0.33558 -> homocedasticity of residuals
ggplot(F_all, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(color="blue", alpha = 0.4) + 
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) + 
  geom_hline(yintercept = 0.8, linetype="dashed", color="black", alpha=0.8) +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  labs(x = "Predicted Values", y = expression(sqrt("|Standardized Residuals|")))

hist(F_all$residuals) # approximately normal
# Shapiro-Wilk test for normality
shapiro.test(F_all$residuals) # p = 0.0003072 -> Reject normality at 5% and 10% significance levels

# QQ-plot of the residuals
ggplot(F_all, mapping=aes(sample=.stdresid)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="blue", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) 

durbinWatsonTest(F_all, max.lag = 12) # p = 0.004 -> Residuals are autocorrelated
Box.test(F_all$residuals, lag = 3, type = "Ljung-Box") # there exists autocorrelation
require(FitAR)
LBQPlot(F_all$residuals, lag.max = 15)
acf(F_all$residuals)
pacf(F_all$residuals)
bgtest(F_all, order = 6) # autocorrelation exists at <= order 6
ggplot(data = total, aes(Date, F_all$residuals)) + 
  geom_point(aes(color = year.f), alpha = 0.8) +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Date", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)

# Leverage vs Standardized Residuals
ggplot(F_all, aes(.hat, .stdresid)) + 
  geom_point(aes(size=.cooksd), color="#b22222", alpha = 0.4) +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  scale_size_continuous("Cook's Distance", range=c(1,5)) + 
  labs(x = "Leverage", y = "Standardized Residuals") +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12), legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent", colour = "transparent"))

# Best subset selection - REGSUBSETS
search_output <- regsubsets(meanBP ~ med_workplace_closing + med_stay_home_restrictions + 
                              med_internal_movement_restrictions + mean_stringency_index + meanCSP +
                              meanCSPpresent + meanCSPabsent + meanCSPhome + ARSf,
                            nbest = 1, nvmax = NULL, data = total, method="exhaustive")
summary(search_output)
as.data.frame(summary(search_output)$outmat)
# Using only one variable the best model uses: med_stay_home_restrictions1
which.max(summary(search_output)$adjr2) # 9 variables
summary(search_output)$which[9,]

# Stepwise Regression (FORWARD, BACKWARD, BOTH)
model_base <- lm(meanBP ~ ARSf, data = total)
fow_res <- step(model_base, scope = list(upper = F_all, lower = ~1), direction = "forward", trace = FALSE)
fow_res
summary(fow_res)
extractAIC(fow_res) # AIC = 828.4353
back_res <- step(F_all, direction = "backward", trace = FALSE)
back_res
summary(back_res)
extractAIC(back_res) # AIC = 722.6736
both_res <- step(model_base, scope = list(upper = F_all, lower = ~1), direction = "both", trace = FALSE)
both_res
summary(both_res)
extractAIC(both_res) # AIC = 828.4353

glmulti.out <- glmulti(meanBP ~ med_workplace_closing + med_stay_home_restrictions + 
                         med_internal_movement_restrictions + mean_stringency_index + meanCSP +
                         meanCSPpresent + meanCSPabsent + meanCSPhome + ARSf, data = total,
                       level = 1, method = "h", crit = "aic", confsetsize = 5, plotty = F, report = F,
                       fitfunction = "lm" )
glmulti.out@formulas
summary(glmulti.out@objects[[1]])

rm("back_res", "both_res", "glmulti.out", "search_output", "fow_res", "model_base", "hist.response")

vif  <- function (fit) {
  v <- vcov(fit, regcoef.only = TRUE)
  nam <- dimnames(v)[[1]]
  ns <- num.intercepts(fit)
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif(F_all)
mean(vif(F_all))

# Removed ARSf and meanCSP
F_all <- lm(meanBP ~ med_school_closing + med_workplace_closing + med_stay_home_restrictions +
              med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
              meanCSPabsent + meanCSPhome,
            data = total, weights = meanPercent); summary(F_all)
vif(F_all)
F_all <- lm(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
              med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
              meanCSPabsent + meanCSPhome,
            data = total, weights = meanPercent); summary(F_all)
vif(F_all)

# GENERALIZED LEAST SQUARES ----------------------------------------------------------------------------------------
arima.M0 <- auto.arima(M0_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.M0 # ARIMA(4,0,0)
arima.M1 <- auto.arima(M1_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.M1 # ARIMA(4,0,0)
arima.M5 <- auto.arima(M5_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.M5 # ARIMA(5,0,0)
arima.M6 <- auto.arima(M6_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.M6 # ARIMA(5,0,0)
arima.M7 <- auto.arima(M7_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.M7 # ARIMA(5,0,0)
arima.I1 <- auto.arima(I1_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.I1 # ARIMA(5,0,0)
arima.I2 <- auto.arima(I2_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.I2 # ARIMA(4,0,0)
arima.I3 <- auto.arima(I3_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.I3 # ARIMA(5,0,0)
arima.I4 <- auto.arima(I4_all$residuals, seasonal = T, approximation = F, stepwise = F); arima.I4 # ARIMA(4,0,0)
# We obtain several results, some of which suggest differencing data which is not possible due to its nature
# The reduced amount of data may hamper the estimation of the correct ARIMA model
# Therefore, we will apply a ARMAMA(2,0,1) to every model
arima.fit <- auto.arima(F_all$residuals, seasonal = F, approximation = F, stepwise = F); arima.fit # ARIMA(0,0,2)

rm("arima.M0", "arima.M1", "arima.M5", "arima.M6", "arima.M7", "arima.I1", "arima.I2", "arima.I3", "arima.I4",
   "arima.fit")

# Experimenting using AR(p) or MA(q)
F_all.GLS.AR2 <- gls(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
                       med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                       meanCSPabsent + meanCSPhome, data = total, correlation=corARMA(p=2), method="ML")
summary(F_all.GLS.AR2) # AIC, BIC, logLik = 944.674 982.3722 -454.337
F_all.GLS.0 <- update(F_all.GLS.AR2, correlation=NULL)
anova(F_all.GLS.AR2, F_all.GLS.0) # AR(2) vs uncorrelated errors -> p <.0001
F_all.GLS.AR1 <- update(F_all.GLS.AR2, correlation=corARMA(p=1))
anova(F_all.GLS.AR2, F_all.GLS.AR1) # AR(2) vs AR(1) -> p = 0.2039
F_all.GLS.AR3 <- update(F_all.GLS.AR2, correlation=corARMA(p=3))
anova(F_all.GLS.AR2, F_all.GLS.AR3) # AR(2) vs AR(3) -> p = 0.0459

F_all.GLS.AR4 <- gls(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
                       med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                       meanCSPabsent + meanCSPhome, data = total, correlation=corARMA(p=4), method="ML")
summary(F_all.GLS.AR4) # AIC, BIC, logLik = 944.674 982.3722 -454.337

F_all.GLS.MA1 <- gls(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
                       med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                       meanCSPabsent + meanCSPhome, data = total, correlation=corARMA(q=1), method="ML")
summary(F_all.GLS.MA1) # AIC, BIC, logLik = 969.8141, 1005.418, -467.9071
F_all.GLS.0 <- update(F_all.GLS.MA1, correlation=NULL)
anova(F_all.GLS.MA1, F_all.GLS.0) # MA(2) vs uncorrelated errors -> p = 2e-04
F_all.GLS.MA2 <- update(F_all.GLS.MA1, correlation=corARMA(q=2))
anova(F_all.GLS.MA1, F_all.GLS.MA2) # MA(2) vs MA(1) -> p = 8e-04
F_all.GLS.MA3 <- update(F_all.GLS.MA1, correlation=corARMA(q=3))
anova(F_all.GLS.MA2, F_all.GLS.MA3) # MA(2) vs MA(3) -> p  = 0.1291 (no need for MA3)

F_all.GLS.ARMA <- gls(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
                        med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                        meanCSPabsent + meanCSPhome, data = total, correlation=corARMA(p=2,q=1), method="ML")
summary(F_all.GLS.ARMA) # AIC, BIC, logLik = 945.8648, 985.6573, -453.9324
F_all.GLS.ARMA0 <- update(F_all.GLS.ARMA, correlation=NULL)
anova(F_all.GLS.ARMA, F_all.GLS.ARMA0) # ARMA vs uncorrelated errors -> p <.0001
F_all.GLS.ARMA1 <- update(F_all.GLS.ARMA, correlation=corARMA(p=2,q=2))
anova(F_all.GLS.ARMA, F_all.GLS.ARMA1) # ARMA(2,0,1) vs ARMA(2,0,2) -> p = 0.0015
F_all.GLS.ARMA2 <- update(F_all.GLS.ARMA, correlation=corARMA(p=4,q=2))
anova(F_all.GLS.ARMA, F_all.GLS.ARMA2) # AR(2) vs AR(3) -> p = 0.0058

# Order dataframes by Date
alentejo <- alentejo[order(alentejo$Date), ] 
algarve <- algarve[order(algarve$Date), ] 
centro <- centro[order(centro$Date), ] 
lvt <- lvt[order(lvt$Date), ] 
norte <- norte[order(norte$Date), ] 
total <- total[order(total$Date), ]

# ARMA(2,0,1) models (check if fixed effects and correlation are significant)
# Procedure: 1. compute model with desired correlation structure using REML (unbiased variance estimates)
#            2. intervals(model) to obtain the value parameters (estimates)
#            3. fit model without the predictors and with the same correlation structure as model 1 -> model0
#            4. update the models to use ML estimation to test the significance of fixed effects -> model.ml
#            5. correct the degrees of freedom of model1.ml -> model1.ml1
#            6. anova(model0,ml, model1.ml1) to compare the significance of the fixed effects
#            7. compute model with no correlation structure -> model2
#            8. anova(model2, model1) to compare the significance of the correlation

M0_all.GLS <- gls(meanBP ~ med_school_closing + ARSf, data = total, correlation = corARMA(form = ~ 1, p=2, q=1))
summary(M0_all.GLS) #AIC = 902.3406
intervals(M0_all.GLS) # 0.1353935, 0.6733884, 0.5906978
M0_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1353935, 0.6733884, 0.5906978), form = ~ 1,
                                                       p=2, q=1, fixed = T))
M0_all.0.ml <- update(M0_all.0, . ~ ., method = "ML")
M0_all.GLS.ml <- update(M0_all.GLS, . ~ ., method = "ML")
M0_all.GLS.ml1 <- update(M0_all.GLS.ml,.~.,corr = corARMA(value = c(0.1353935, 0.6733884, 0.5906978), form = ~ 1,
                                                          p=2, q=1, fixed = T))
anova(M0_all.0.ml, M0_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
M0_all.2 <- update(M0_all.GLS, correlation = NULL)
anova(M0_all.2, M0_all.GLS) # p < 0.0001 -> correlation structure is significant
M0_r2 <- nagelkerke(M0_all.GLS)
M0_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("M0_all.0", "M0_all.0.ml", "M0_all.GLS.ml", "M0_all.GLS.ml1", "M0_all.2")

M1_all.GLS <- gls(meanBP ~ med_workplace_closing + ARSf, data = total, correlation = corARMA(form = ~ 1, p=2, q=1))
summary(M1_all.GLS) #AIC = 911.8844
intervals(M1_all.GLS) # 0.1751174, 0.6429621, 0.5525204
M1_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1751174, 0.6429621, 0.5525204), form = ~ 1,
                                                       p=2, q=1, fixed = T))
M1_all.0.ml <- update(M1_all.0, . ~ ., method = "ML")
M1_all.GLS.ml <- update(M1_all.GLS, . ~ ., method = "ML")
M1_all.GLS.ml1 <- update(M1_all.GLS.ml,.~.,corr = corARMA(value = c(0.1751174, 0.6429621, 0.5525204), form = ~ 1,
                                                          p=2, q=1, fixed = T))
anova(M1_all.0.ml, M1_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
M1_all.2 <- update(M1_all.GLS, correlation = NULL)
anova(M1_all.2, M1_all.GLS) # p < 0.0001 -> correlation structure is significant
M1_r2 <- nagelkerke(M1_all.GLS)
M1_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("M1_all.0", "M1_all.0.ml", "M1_all.GLS.ml", "M1_all.GLS.ml1", "M1_all.2")


M5_all.GLS <- gls(meanBP ~ med_stay_home_restrictions + ARSf, data = total, correlation = corARMA(form =~1,p=2,q=1))
summary(M5_all.GLS) #AIC = 903.5076
intervals(M5_all.GLS) # 0.1125966, 0.6284437, 0.5992050
M5_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1125966, 0.6284437, 0.5992050), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
M5_all.0.ml <- update(M5_all.0, . ~ ., method = "ML")
M5_all.GLS.ml <- update(M5_all.GLS, . ~ ., method = "ML")
M5_all.GLS.ml1 <- update(M5_all.GLS.ml,.~.,corr = corARMA(value = c(0.1125966, 0.6284437, 0.5992050), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(M5_all.0.ml, M5_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
M5_all.2 <- update(M5_all.GLS, correlation = NULL)
anova(M5_all.2, M5_all.GLS) # p < 0.0001 -> correlation structure is significant
M5_r2 <- nagelkerke(M5_all.GLS)
M5_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("M5_all.0", "M5_all.0.ml", "M5_all.GLS.ml", "M5_all.GLS.ml1", "M5_all.2")

M6_all.GLS <- gls(meanBP ~ med_internal_movement_restrictions + ARSf, data = total,
                  correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(M6_all.GLS) #AIC = 912.0205
intervals(M6_all.GLS) # 0.1553762, 0.6694512, 0.5864528 
M6_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1553762, 0.6694512, 0.5864528 ), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
M6_all.0.ml <- update(M6_all.0, . ~ ., method = "ML")
M6_all.GLS.ml <- update(M6_all.GLS, . ~ ., method = "ML")
M6_all.GLS.ml1 <- update(M6_all.GLS.ml,.~.,corr = corARMA(value = c(0.1553762, 0.6694512, 0.5864528 ), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(M6_all.0.ml, M6_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
M6_all.2 <- update(M6_all.GLS, correlation = NULL)
anova(M6_all.2, M6_all.GLS) # p < 0.0001 -> correlation structure is significant
M6_r2 <- nagelkerke(M6_all.GLS)
M6_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("M6_all.0", "M6_all.0.ml", "M6_all.GLS.ml", "M6_all.GLS.ml1", "M6_all.2")

M7_all.GLS <- gls(meanBP ~ mean_stringency_index + ARSf, data = total, correlation = corARMA(form = ~ 1, p=2, q=1))
summary(M7_all.GLS) #AIC = 917.7673
intervals(M7_all.GLS) # 0.1284458, 0.6315902, 0.5813426 
M7_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1284458, 0.6315902, 0.5813426), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
M7_all.0.ml <- update(M7_all.0, . ~ ., method = "ML")
M7_all.GLS.ml <- update(M7_all.GLS, . ~ ., method = "ML")
M7_all.GLS.ml1 <- update(M7_all.GLS.ml,.~.,corr = corARMA(value = c(0.1284458, 0.6315902, 0.5813426), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(M7_all.0.ml, M7_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
M7_all.2 <- update(M7_all.GLS, correlation = NULL)
anova(M7_all.2, M7_all.GLS) # p < 0.0001 -> correlation structure is significant
M7_r2 <- nagelkerke(M7_all.GLS) 
M7_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("M7_all.0", "M7_all.0.ml", "M7_all.GLS.ml", "M7_all.GLS.ml1", "M7_all.2")

I1_all.GLS <- gls(meanBP ~ meanCSP + ARSf, data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(I1_all.GLS) #AIC = 929.9014 
intervals(I1_all.GLS) # 0.1745233, 0.5963119, 0.5068872 
I1_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1745233, 0.5963119, 0.5068872), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
I1_all.0.ml <- update(I1_all.0, . ~ ., method = "ML")
I1_all.GLS.ml <- update(I1_all.GLS, . ~ ., method = "ML")
I1_all.GLS.ml1 <- update(I1_all.GLS.ml,.~.,corr = corARMA(value = c(0.1745233, 0.5963119, 0.5068872), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(I1_all.0.ml, I1_all.GLS.ml1) # p <.0001 -> fixed effects are significant
I1_all.2 <- update(I1_all.GLS, correlation = NULL)
anova(I1_all.2, I1_all.GLS) # p <.0001 -> correlation structure is significant
I1_r2 <- nagelkerke(I1_all.GLS) 
I1_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("I1_all.0", "I1_all.0.ml", "I1_all.GLS.ml", "I1_all.GLS.ml1", "I1_all.2")

I2_all.GLS <- gls(meanBP ~ meanCSPpresent + ARSf, data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(I2_all.GLS) #AIC = 931.5747
intervals(I2_all.GLS, which = "coef") # 0.1291910, 0.6351870, 0.5815485
I2_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1291910, 0.6351870, 0.5815485), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
I2_all.0.ml <- update(I2_all.0, . ~ ., method = "ML")
I2_all.GLS.ml <- update(I2_all.GLS, . ~ ., method = "ML")
I2_all.GLS.ml1 <- update(I2_all.GLS.ml,.~.,corr = corARMA(value = c(0.1291910, 0.6351870, 0.5815485), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(I2_all.0.ml, I2_all.GLS.ml1) # p < .0001 -> fixed effects are significant
I2_all.2 <- update(I2_all.GLS, correlation = NULL)
anova(I2_all.2, I2_all.GLS) # p < .0001 -> correlation structure is significant
I2_r2 <- nagelkerke(I2_all.GLS) 
I2_r2$Pseudo.R.squared.for.model.vs.null[[1]] # McFadden Pseudo = 0.0388252
rm("I2_all.0", "I2_all.0.ml", "I2_all.GLS.ml", "I2_all.GLS.ml1", "I2_all.2")

I3_all.GLS <- gls(meanBP ~ meanCSPabsent + ARSf, data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(I3_all.GLS) #AIC = 925.7546
intervals(I3_all.GLS) # 0.1931575, 0.5954806, 0.4692580
I3_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1931575, 0.5954806, 0.4692580), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
I3_all.0.ml <- update(I3_all.0, . ~ ., method = "ML")
I3_all.GLS.ml <- update(I3_all.GLS, . ~ ., method = "ML")
I3_all.GLS.ml1 <- update(I3_all.GLS.ml,.~.,corr = corARMA(value = c(0.1931575, 0.5954806, 0.4692580), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(I3_all.0.ml, I3_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
I3_all.2 <- update(I3_all.GLS, correlation = NULL)
anova(I3_all.2, I3_all.GLS) # p < .0001 -> correlation structure is significant
I3_r2 <- nagelkerke(I3_all.GLS) 
I3_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("I3_all.0", "I3_all.0.ml", "I3_all.GLS.ml", "I3_all.GLS.ml1", "I3_all.2")

I4_all.GLS <- gls(meanBP ~ meanCSPhome + ARSf, data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(I4_all.GLS) # AIC = 920.9862
intervals(I4_all.GLS) # 0.1282084, 0.6247931, 0.6249825  
I4_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.1282084, 0.6247931, 0.6249825), form = ~ 1,
                                                       p = 2, q = 1, fixed = T))
I4_all.0.ml <- update(I4_all.0, . ~ ., method = "ML")
I4_all.GLS.ml <- update(I4_all.GLS, . ~ ., method = "ML")
I4_all.GLS.ml1 <- update(I4_all.GLS.ml,.~.,corr = corARMA(value = c(0.1282084, 0.6247931, 0.6249825), form = ~ 1,
                                                          p = 2, q = 1, fixed = T))
anova(I4_all.0.ml, I4_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
I4_all.2 <- update(I4_all.GLS, correlation = NULL)
anova(I4_all.2, I4_all.GLS) # p < .0001 -> correlation structure is significant
I4_r2 <- nagelkerke(I4_all.GLS) 
I4_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("I4_all.0", "I4_all.0.ml", "I4_all.GLS.ml", "I4_all.GLS.ml1", "I4_all.2")

F_all.GLS <- gls(meanBP ~ med_workplace_closing + med_stay_home_restrictions +
                   med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                   meanCSPabsent + meanCSPhome,
                 data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(F_all.GLS) # AIC =  820.7467
intervals(F_all.GLS) # 0.5145695, 0.4755923, 0.2844737
F_all.0 <- gls(meanBP ~ 1,data = total,corr = corARMA(value = c(0.5145695, 0.4755923, 0.2844737), form = ~ 1, 
                                                      p = 2, q = 1, fixed = T))
F_all.0.ml <- update(F_all.0, . ~ ., method = "ML")
F_all.GLS.ml <- update(F_all.GLS, . ~ ., method = "ML")
F_all.GLS.ml1 <- update(F_all.GLS.ml,.~.,corr = corARMA(value = c(0.5145695, 0.4755923, 0.2844737), form = ~ 1, 
                                                        p = 2, q = 1, fixed = T))
anova(F_all.0.ml, F_all.GLS.ml1) # p < 0.0001 -> fixed effects are significant
F_all.2 <- update(F_all.GLS, correlation = NULL)
anova(F_all.2, F_all.GLS) # p < .0001 -> correlation structure is significant
F_r2 <- nagelkerke(F_all.GLS) 
F_r2$Pseudo.R.squared.for.model.vs.null[[1]]
rm("F_all.0", "F_all.0.ml", "F_all.GLS.ml", "F_all.GLS.ml1", "F_all.2")

# BOX-PLOT OF MEASURES ---------------------------------------------------------------------------------------------
Model <- c("M0", "M1", "M2 ", "M3", "M4", "I1", "I2", "I3", "I4", "F")
AIC <- c(AIC(M0_all.GLS), AIC(M1_all.GLS), AIC(M5_all.GLS), AIC(M6_all.GLS), AIC(M7_all.GLS), AIC(I1_all.GLS),
         AIC(I2_all.GLS), AIC(I3_all.GLS), AIC(I4_all.GLS), AIC(F_all.GLS))
AIC.df <- data.frame(Model, AIC)
R2McFadden <- c(M0_r2$Pseudo.R.squared.for.model.vs.null[[1]], M1_r2$Pseudo.R.squared.for.model.vs.null[[1]],
                M5_r2$Pseudo.R.squared.for.model.vs.null[[1]], M6_r2$Pseudo.R.squared.for.model.vs.null[[1]],
                M7_r2$Pseudo.R.squared.for.model.vs.null[[1]], I1_r2$Pseudo.R.squared.for.model.vs.null[[1]],
                I2_r2$Pseudo.R.squared.for.model.vs.null[[1]], I3_r2$Pseudo.R.squared.for.model.vs.null[[1]],
                I4_r2$Pseudo.R.squared.for.model.vs.null[[1]], F_r2$Pseudo.R.squared.for.model.vs.null[[1]])
R2McFadden.df <- data.frame(Model, R2McFadden)
R2Nagelkerke <- c(M0_r2$Pseudo.R.squared.for.model.vs.null[[3]], M1_r2$Pseudo.R.squared.for.model.vs.null[[3]],
                  M5_r2$Pseudo.R.squared.for.model.vs.null[[3]], M6_r2$Pseudo.R.squared.for.model.vs.null[[3]],
                  M7_r2$Pseudo.R.squared.for.model.vs.null[[3]], I1_r2$Pseudo.R.squared.for.model.vs.null[[3]],
                  I2_r2$Pseudo.R.squared.for.model.vs.null[[3]], I3_r2$Pseudo.R.squared.for.model.vs.null[[3]],
                  I4_r2$Pseudo.R.squared.for.model.vs.null[[3]], F_r2$Pseudo.R.squared.for.model.vs.null[[3]])
R2Nagelkerke.df <- data.frame(Model, R2Nagelkerke)

boxplt1 <- ggplot(data=AIC.df, aes(x= "", y = AIC)) +
  geom_point(data = AIC.df, mapping = aes(x = "", y = AIC, color = Model), size = 3, alpha = 1) +
  geom_boxplot(aes(y = AIC), alpha = 0.2, size = 0.7, notch = FALSE, width = 0.2, fatten = 0) +
  ylab("AIC") +
  scale_y_continuous(breaks = c(820,830,840,850,860,870,880,890,900,910,920,930)) + 
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.y=element_blank(), axis.text.y=element_blank()) +
  coord_flip()
#scale_fill_manual(values = c("M1"="#E41A1C", "M5"="#377EB8", "M6"="#4DAF4A", "M7"="#984EA3", "I1"="#FF7F00",
#"I2"="#FFFF33", "I3"="#A65628", "I4"="#F781BF", "F"="#999999"))

boxplt2 <- ggplot(data=R2McFadden.df, aes(x= "", y = R2McFadden)) +
  geom_point(data = R2McFadden.df, mapping = aes(x = "", y = R2McFadden, color = Model), size = 3, alpha = 1) +
  geom_boxplot(aes(y = R2McFadden), alpha = 0.2, size = 0.7, notch = FALSE, width = 0.2, fatten = 0) +
  ylab(expression(paste("McFadden Pseudo-",R^2))) +
  scale_y_continuous(breaks = c(0.05,0.055,0.06,0.065,0.07,0.075,0.08,0.085,0.09,0.095,0.1,0.105,0.110)) + 
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.y=element_blank(), axis.text.y=element_blank()) +
  coord_flip()

boxplt3 <- ggplot(data=R2Nagelkerke.df, aes(x= "", y = R2Nagelkerke)) +
  geom_point(data = R2Nagelkerke.df, mapping = aes(x = "", y = R2Nagelkerke, color = Model), size = 3, alpha = 1) +
  geom_boxplot(aes(y = R2Nagelkerke), alpha = 0.2, size = 0.7, notch = FALSE, width = 0.2, fatten = 0) +
  ylab(expression(paste("Nagelkerke Pseudo-", R^2))) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.y=element_blank(), axis.text.y=element_blank()) +
  coord_flip()

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(boxplt1)

grid.arrange(arrangeGrob(boxplt1 + theme(legend.position="none"),
                         boxplt2 + theme(legend.position="none"),
                         boxplt3 + theme(legend.position="none"), nrow = 3),
             mylegend, ncol = 2, widths = c(8,1))

# DIAGNOSTIC PLOTS FOR FULL MODEL ------------------------------------------------------------------------------
summary(F_all.GLS)
anova(F_all.GLS)
intervals(F_all.GLS)
# QQ-plot of residuals
qqnorm(F_all.GLS, abline = c(0,1), pch = 16, cex = 0.8)
shapiro.test(F_all.GLS$residuals) # p = 0.0005722 -> Reject normality at all usual significance levels
# Residuals vs Fitted values
ggplot(, aes(F_all.GLS$fitted, F_all.GLS$residuals)) + 
  geom_point(color="blue", alpha = 0.4) +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) + 
  labs(x="Fitted Values", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8) +
  scale_x_continuous(breaks = c(-3000,-2000,-1000,0,1000,2000,3000,4000,5000)) +
  scale_y_continuous(breaks = c(-3000,-2000,-1000,0,1000,2000,3000,4000,5000))

# Histogram of residuals
ggplot(,aes(x = resid(F_all.GLS))) + 
  geom_histogram(color="black", fill="dodgerblue", alpha = 0.6, binwidth = 300) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x="Residuals", y = "Count") +
  scale_x_continuous(breaks = c(-1000,0,1000,2000,3000,4000,5000))

# Residual autocorrelation
Box.test(F_all.GLS$residuals, lag = 1, type = "Ljung-Box") # there exists autocorrelation
require(FitAR)
LBQPlot(F_all.GLS$residuals, lag.max = 15)
acf(F_all.GLS$residuals)
pacf(F_all.GLS$residuals)
bgtest(F_all.GLS, order = 6) # autocorrelation exists at <= order 6

# Residuals vs Order Plot
ggplot(data = total, aes(Date, F_all.GLS$residuals)) + 
  geom_point(alpha = 0.8, color = "dodgerblue") +
  theme(text = element_text(size = 14, face = "italic"),axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Date", y = "Residuals", fill = "Year") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8) +
  scale_x_date(limits = as.Date(c("2020-03-01","2021-02-02")), date_breaks = "1 months", date_labels =  "%b %y", 
               expand = c(0.04,0))
# Durbin-Watson test
v <- F_all.GLS$residuals
attr(v,"std") <- NULL      # get rid of the additional attribute
car::durbinWatsonTest(v)

# Colinearity
require(Hmisc)
vif  <- function (fit) {
  v <- vcov(fit, regcoef.only = TRUE)
  nam <- dimnames(v)[[1]]
  ns <- num.intercepts(fit)
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif(F_all.GLS)
mean(vif(F_all.GLS))

F_all.GLS <- gls(meanBP ~ med_school_closing + med_stay_home_restrictions +
                   med_internal_movement_restrictions + mean_stringency_index + meanCSPpresent +
                   meanCSPhome,
                 data = total, correlation = corARMA(form = ~ 1, p = 2, q = 1))
summary(F_all.GLS) # AIC =  820.7467
intervals(F_all.GLS)

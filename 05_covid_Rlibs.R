####################################################
###### 05. SCRIPT TO ANALYSE COVID R PACKAGES ######
####################################################

# --------------------------------------------------------------------------------------------------------------------
# 1. Analyse package COVID19
#    Adapted from Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376,
#    doi:10.21105/joss.02376. in https://covid19datahub.io/
# 2. Analyse package covid19.analytics
#    Adapted from Marcelo Ponce, Amit Sandhel (2020). covid19.analytics: An R Package to Obtain, Analyze and Visualize
#    Data from the Corona Virus Disease Pandemic. URL https://arxiv.org/abs/2009.01091
#
# Miguel Farinha (MMA BioStatistics 2020-2021)
# --------------------------------------------------------------------------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
library(RColorBrewer)
library(VIM)
library(DataExplorer)
library(lubridate)
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
library(zoo)

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

# 1. Package COVID19
library(COVID19)

covid_lvl1 <- covid19(country = "Portugal")
colnames(covid_lvl1)
# columns of interest: school_closing, workplace_closing, cancel_events, gathering_restrictions, transport_closing,
#                      stay_home_restrictions, internal_movement_restrictions, information_campaigns, stringency_index
# check https://covid19datahub.io/articles/doc/data.html
summary(covid_lvl1)

covid_lvl2 <- covid19(country = "Portugal", level = 2)
colnames(covid_lvl2)
head(covid_lvl2)
covid_lvl2$administrative_area_level_2 # ARS information
unique(covid_lvl2$administrative_area_level_2) 
# Select only ARS Lisboa, Centro, Norte, Alentejo, Algarve (exclude Açores and Madeira)
covid_lvl2 <- filter(covid_lvl2, administrative_area_level_2 != "Madeira" & administrative_area_level_2 != "Açores")
covid_lvl2 <- covid_lvl2[which(covid_lvl2$date < "2021-03-01"),]
min(covid_lvl2$date) # "2020-02-26"
max(covid_lvl2$date) # "2021-02-28"

# Stringency index calculated as in https://www.bsg.ox.ac.uk/sites/default/files/2020-04/BSG-WP-2020-032-v5.0.pdf
unique(covid_lvl1$stringency_index)

# Construct dataset with variables of interest (monthly data due to hypertension dataset)
# Select the median for categorical variables
summary(covid_lvl2$workplace_closing)
summary(covid_lvl2$gatherings_restrictions)
summary(covid_lvl2$stringency_index)
summary(covid_lvl2$information_campaigns) # uninformative since level = 2for every entry
summary(covid_lvl1$information_campaigns)
covid_supp <-
  covid_lvl2 %>%
  group_by(administrative_area_level_2, month(covid_lvl2$date, label = TRUE, abbr = FALSE), year(covid_lvl2$date)) %>% 
  summarise(med_workplace_closing = median(workplace_closing, na.rm = TRUE),
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
covid_supp[which(covid_supp$Month == "February" & covid_supp$Year == "2020"), c(4:10)] <- 0
covid_final <- na.locf(covid_supp, na.rm=FALSE)
sum(is.na(covid_final))

# PACKAGE covid19.analytics (NOT USED) -----------------------------------------------------------------------------

library(covid19.analytics)
citation("covid19.analytics")
covid19.confirmed.cases <- covid19.data("ts-confirmed")
tots.per.location(covid19.confirmed.cases,geo.loc="Portugal")
e <- tots.per.location(covid19.confirmed.cases,geo.loc="Portugal")
e <- covid19.data.ALLcases[which(covid19.data.ALLcases$Country_Region == "Portugal"),]
tots.per.location(covid19.confirmed.cases,geo.loc="Portugal")

all.data <- covid19.data('ts-ALL')
tots.per.location(all.data,"Portugal")
tots.per.location(covid19.data("ts-confirmed"))
TS.data <- covid19.data("ts-confirmed")

# compute changes and growth rates per location for 'Portugal'
growth.rate(TS.data,geo.loc="Portugal")

single.trend(TS.data[ TS.data$Country.Region=="Portugal",])
itrends(covid19.data("ts-confirmed"), geo.loc=c("Portugal"), fileName="itrends_ex")
totals.plt(TS.data)
totals.plt(TS.data, c("Portugal"), with.totals=TRUE,one.plt.per.page=FALSE)
live.map(covid19.data("ts-confirmed"))
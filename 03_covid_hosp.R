###################################################################
###### 03. SCRIPT TO ORGANIZE COVID DATA BY HOSPITALIZATIONS ######
###################################################################

# -----------------------------------------------------------------
# 1. Import the necessary data (covid_main.csv)
# 2. Explore the number of hospitalizations and ICU hospitalizations
# 3. Plots with different levels of detail (daily, weekly, monthly)
#
# Miguel Farinha 
# (MMA BioStatistics May 2021)
# -----------------------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
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
library(tidyverse)

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

# Import the dataset covid_main
covid_main <- read.table("covid_main.csv", sep = ",", header = TRUE)
head(covid_main)

# Set the datatype Date for the variable data
covid_main$data <- as.Date(covid_main$data, format="%d-%m-%Y", tz="GMT")
head(covid_main$data)
class(covid_main$data) # class: "Date"
min(covid_main$data) # oldest registration: "2020-02-26"
max(covid_main$data) # newest registration: "2021-06-06" (dependent on data download date)
covid_main <- covid_main[-which(covid_main$data > "2021-05-31"),]
month(covid_main$data, label = TRUE, abbr = FALSE) # obtain the months of each cell in data
epiweek((covid_main$data)) # obtain the epidemiological week in data

# Function to assert the quality of the data for each ARS
quality_confirmed_intern <- function(df) {
  df$internados <- as.numeric(df$internados)
  df$internados_uci <- as.numeric(df$internados_uci)
  df$internados_enfermaria <- as.numeric(df$internados_enfermaria)
}
quality_confirmed_intern(covid_main) 

# Create dataset with only the variables of interest
covid_hosp <- covid_main[,c("data", "internados", "internados_uci", "internados_enfermaria")]

# Check the number of missing values (NA) in the dataset covid_hosp
sum(is.na(covid_hosp$internados)) # 8
sum(is.na(covid_hosp$internados_uci)) # 17
sum(is.na(covid_hosp$internados_enfermaria)) # 5
which(is.na(covid_hosp$internados)) # rows: 1 2 3 4 5 6 7 8 with NA
which(is.na(covid_hosp$internados_uci)) # rows: 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
which(is.na(covid_hosp$internados_enfermaria)) # rows: 1 2 3 4 5
# Only the first rows of each dataset have missing values
# Until row 13 it is unkown whether the patients interned were in ICU or Nursery
covid_hosp <- covid_hosp[which(covid_hosp$data > "2020-03-09"),]
covid_hosp[is.na(covid_hosp)] <- 0

# 1. Obtain the total number of hospitalizations for each month of each year of the pandemic
covid_hosp_my <-
  covid_hosp %>%
  group_by(month(covid_hosp$data, label = TRUE, abbr = FALSE), year(covid_hosp$data)) %>% 
  summarise(uci = sum(internados_uci, na.rm = TRUE),
            enfermaria = sum(internados_enfermaria, na.rm = TRUE))
colnames(covid_hosp_my)[1] <- "Month"
colnames(covid_hosp_my)[2] <- "Year"
covid_hosp_my <- covid_hosp_my[order(covid_hosp_my[,2]), ]

# 2. Obtain the total number of hospitalizations for each week of a given year
covid_hosp_wy <-
  covid_hosp %>%
  group_by(week(covid_hosp$data), year(covid_hosp$data)) %>% 
  summarise(uci = sum(internados_uci, na.rm = TRUE),
            enfermaria = sum(internados_enfermaria, na.rm = TRUE))
colnames(covid_hosp_wy)[1] <- "Week"
colnames(covid_hosp_wy)[2] <- "Year"
covid_hosp_wy <- covid_hosp_wy[order(covid_hosp_wy[,2],
                                     covid_hosp_wy[,1]), ]

# 3. Obtain the total number of hospitalizations for each week for each month of a given year of the pandemic
covid_hosp_wm <-
  covid_hosp %>%
  group_by(ceiling(day((covid_hosp$data)) / 7), month(covid_hosp$data, label = TRUE, abbr = FALSE),
           year(covid_hosp$data)) %>% 
  summarise(uci = sum(internados_uci, na.rm = TRUE),
            enfermaria = sum(internados_enfermaria, na.rm = TRUE))
colnames(covid_hosp_wm)[1] <- "WeekofMonth"
colnames(covid_hosp_wm)[2] <- "Month"
colnames(covid_hosp_wm)[3] <- "Year"
covid_hosp_wm <- covid_hosp_wm[order(covid_hosp_wm[,3],
                                     covid_hosp_wm[,2]), ]

### Barplots for the Total Number of Hospitalizations --------------------------------------------------------

# The labels should be updated everytime a new dataset is imported
label_my <- c("Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20",
              "Nov 20", "Dec 20", "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21")
label_my <- factor(label_my, levels = label_my)

label_wy <- c("W10 20", "W11 20", "W12 20", "W13 20", "W14 20", "W15 20", "W16 20", "W17 20",
              "W18 20", "W19 20", "W20 20", "W21 20", "W22 20", "W23 20", "W24 20", "W25 20", "W26 20",
              "W27 20", "W28 20", "W29 20", "W30 20", "W31 20", "W32 20", "W33 20", "W34 20", "W35 20",
              "W36 20", "W37 20", "W38 20", "W39 20", "W40 20", "W41 20", "W42 20", "W43 20", "W44 20",
              "W45 20", "W46 20", "W47 20", "W48 20", "W49 20", "W50 20", "W51 20", "W52 20", "W53 20",
              "W1 21", "W2 21", "W3 21", "W4 21", "W5 21", "W6 21", "W7 21", "W8 21", "W9 21", "W10 21",
              "W11 21", "W12 21", "W13 21", "W14 21", "W15 21", "W16 21", "W17 21", "W18 21", "W19 21",
              "W20 21", "W21 21",  "W22 21")
label_wy <- factor(label_wy, levels = label_wy)

label_wmy <- c("W2 Mar 20", "W3 Mar 20", "W4 Mar 20", "W5 Mar 20", "W1 Apr 20", "W2 Apr 20", "W3 Apr 20",
               "W4 Apr 20", "W5 Apr 20", "W1 May 20", "W2 May 20", "W3 May 20", "W4 May 20", "W5 May 20",
               "W1 Jun 20", "W2 Jun 20", "W3 Jun 20", "W4 Jun 20", "W5 Jun 20", "W1 Jul 20", "W2 Jul 20",
               "W3 Jul 20", "W4 Jul 20", "W5 Jul 20", "W1 Aug 20", "W2 Aug 20", "W3 Aug 20", "W4 Aug 20",
               "W5 Aug 20", "W1 Sep 20", "W2 Sep 20", "W3 Sep 20", "W4 Sep 20", "W5 Sep 20", "W1 Oct 20",
               "W2 Oct 20", "W3 Oct 20", "W4 Oct 20", "W5 Oct 20", "W1 Nov 20", "W2 Nov 20", "W3 Nov 20",
               "W4 Nov 20", "W5 Nov 20", "W1 Dec 20", "W2 Dec 20", "W3 Dec 20", "W4 Dec 20", "W5 Dec 20",
               "W1 Jan 21", "W2 Jan 21", "W3 Jan 21", "W4 Jan 21", "W5 Jan 21", "W1 Fev 21", "W2 Fev 21",
               "W3 Fev 21", "W4 Fev 21", "W1 Mar 21", "W2 Mar 21", "W3 Mar 21", "W4 Mar 21", "W5 Mar 21",
               "W1 Apr 21", "W2 Apr 21", "W3 Apr 21", "W4 Apr 21", "W5 Apr 21", "W1 May 21", "W2 May 21",
               "W3 May 21", "W4 May 21", "W5 May 21")
label_wmy <- factor(label_wmy, levels = label_wmy)               


# df$date <- with(df, sprintf("%d-%02d", df$Year, df$Month)) --> build new column with aggregated date


# 1. Barplot of the total number of hospitalizations per month of the pandemic
covid_hosp_my$label_my <- label_my
covid_hosp_my_2 <- covid_hosp_my %>%
  gather(hosp, total, -Month, -Year, -label_my )

ggplot(data = covid_hosp_my_2, aes(x = label_my, y = total, fill = hosp)) +
  geom_bar(stat = "identity", width = 0.8, position = position_stack(vjust = 0.5)) +
  #geom_text(aes(label = total), position = position_dodge(width=0.95), vjust=-0.7, size = 3) +
  xlab("Date") +
  ylab("Total number of hospitalizations") +
  scale_y_continuous(breaks = c(0,20000, 40000, 60000, 80000, 100000,120000,140000)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 1))]) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  # legend.position = c(0.9, 0.8)
  labs(fill = "Hospitalization") +
  scale_fill_discrete(labels = c("Nursery", "ICU"))

# 2. Lineplot of the total number of hospitalizations per month of the pandemic
ggplot(data = covid_hosp_my_2, aes(x = label_my, y = total, group = hosp)) +
  geom_line(aes(color = hosp), size = 0.8) +
  geom_point(aes(color = hosp), size = 2) +
  geom_text(aes(label=total), vjust=-0.8, size = 3.5) +
  xlab("Date") +
  ylab("Total Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0,25000, 50000, 75000, 100000, 125000, 150000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_color_discrete(name = "Hospitalization", labels = c("Nursery", "ICU"))

# 3. Barplot of the total number of hospitalizations for each week of a given year
covid_hosp_wy$label_wy <- label_wy
covid_hosp_wy_2 <- covid_hosp_wy %>%
  gather(hosp, total, -Week, -Year, -label_wy )

ggplot(data = covid_hosp_wy_2, aes(x = label_wy, y = total, fill = hosp)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000, 50000)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Hospitalization") +
  scale_fill_discrete(labels = c("Nursery", "ICU"))

# 4. Lineplot of the total number of hospitalizations for each week of a given year
ggplot(data = covid_hosp_wy_2, aes(x = label_wy, y = total, group = hosp)) +
  geom_line(aes(color = hosp), size = 0.8) +
  geom_point(aes(color = hosp), size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  scale_color_discrete(name = "Hospitalization", labels = c("Nursery", "ICU"))

# 5. Barplot of the total number of hospitalizations for each week of the month on a given year
covid_hosp_wm$label_wmy <- label_wmy
covid_hosp_wm_2 <- covid_hosp_wm %>%
  gather(hosp, total, -WeekofMonth, -Month, -Year, -label_wmy )

ggplot(data = covid_hosp_wm_2, aes(x = label_wmy, y = total, fill = hosp)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 6))]) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Hospitalization") +
  scale_fill_discrete(labels = c("Nursery", "ICU"))

# 6. Lineplot of the total number of hospitalizations for each week of the month on a given year
ggplot(data = covid_hosp_wm_2, aes(x = label_wmy, y = total, group = hosp)) +
  geom_line(aes(color = hosp), size = 0.8) +
  geom_point(aes(color = hosp), size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 5))]) +
  scale_color_discrete(name = "Hospitalization", labels = c("Nursery", "ICU"))

# 7. Barplot of the number of hospitalizations for each day during the pandemic
covid_hosp_2 <- covid_hosp[,-c(2)] %>%
  gather(hosp, total, -data)

ggplot(data = covid_hosp_2, aes(x = data, y = total, fill = hosp)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Hospitalization") +
  scale_fill_discrete(labels = c("Nursery", "ICU"))

# 8. Lineplot of the number of hospitalizations for each day during the pandemic
ggplot(data = covid_hosp_2, aes(x = data, y = total, group = hosp)) +
  geom_line(aes(color = hosp), size = 0.7) +
  geom_point(aes(color = hosp), size = 0.9) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Number of Hospitalizations") +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_color_discrete(name = "Hospitalization", labels = c("Nursery", "ICU"))

###################################################################################
###### 02. SCRIPT TO ORGANIZE COVID DATA BY NEW CONFIRMED CASES FOR EACH ARS ######
###################################################################################

# -----------------------------------------------------------------
# 1. Import the necessary data (covid_main.csv)
# 2. Create covariate with new confirmed cases for each ARS
# 3. Explore the number of new confirmed cases for each ARS
# 4. Plots with different levels of detail (daily, weekly, monthly)
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
library(tidyverse)
library(tidyr)

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
quality_confirmed <- function(df) {
  df$confirmados_arsnorte <- as.numeric(df$confirmados_arsnorte)
  df$confirmados_arscentro <- as.numeric(df$confirmados_arscentro)
  df$confirmados_arslvt <- as.numeric(df$confirmados_arslvt)
  df$confirmados_arsalentejo <- as.numeric(df$confirmados_arsalentejo)
  df$confirmados_arsalgarve <- as.numeric(df$confirmados_arsalgarve)
}
quality_confirmed(covid_main)

# Compute the total of new confirmed cases for each ARS
covid_main$confirmados_novos_arsnorte <- with(covid_main, c(0, diff(covid_main$confirmados_arsnorte)))
covid_main$confirmados_novos_arsnorte
covid_main$confirmados_novos_arscentro <- with(covid_main, c(0, diff(covid_main$confirmados_arscentro)))
covid_main$confirmados_novos_arscentro
covid_main$confirmados_novos_arslvt <- with(covid_main, c(0, diff(covid_main$confirmados_arslvt)))
covid_main$confirmados_novos_arslvt
covid_main$confirmados_novos_arsalentejo <- with(covid_main, c(0, diff(covid_main$confirmados_arsalentejo)))
covid_main$confirmados_novos_arsalentejo
covid_main$confirmados_novos_arsalgarve <- with(covid_main, c(0, diff(covid_main$confirmados_arsalgarve)))
covid_main$confirmados_novos_arsalgarve

# Create dataset with only the variables of interest
covid_ars <- covid_main[,c("data", "confirmados_novos_arsnorte", "confirmados_novos_arscentro",
                           "confirmados_novos_arslvt", "confirmados_novos_arsalentejo",
                           "confirmados_novos_arsalgarve")]

# 1. Obtain the total number of new cases for each month of each year of the pandemic by ARS
covid_newcases_my_ars <-
  covid_ars %>%
  group_by(month(covid_ars$data, label = TRUE, abbr = FALSE), year(covid_ars$data)) %>% 
  summarise(newcases_arsnorte = sum(confirmados_novos_arsnorte, na.rm = TRUE),
            newcases_arscentro = sum(confirmados_novos_arscentro, na.rm = TRUE),
            newcases_arslvt = sum(confirmados_novos_arslvt, na.rm = TRUE),
            newcases_arsalentejo = sum(confirmados_novos_arsalentejo, na.rm = TRUE),
            newcases_arsalgarve = sum(confirmados_novos_arsalgarve, na.rm = TRUE))
colnames(covid_newcases_my_ars)[1] <- "Month"
colnames(covid_newcases_my_ars)[2] <- "Year"
covid_newcases_my_ars <- covid_newcases_my_ars[order(covid_newcases_my_ars[,2]), ]

# 2. Obtain the total number of new cases for each week of a given year by ARS
covid_newcases_wy_ars <-
  covid_ars %>%
  group_by(week(covid_ars$data), year(covid_ars$data)) %>% 
  summarise(new_cases_arsnorte = sum(confirmados_novos_arsnorte, na.rm = TRUE),
            newcases_arscentro = sum(confirmados_novos_arscentro, na.rm = TRUE),
            newcases_arslvt = sum(confirmados_novos_arslvt, na.rm = TRUE),
            newcases_arsalentejo = sum(confirmados_novos_arsalentejo, na.rm = TRUE),
            newcases_arsalgarve = sum(confirmados_novos_arsalgarve, na.rm = TRUE))
colnames(covid_newcases_wy_ars)[1] <- "Week"
colnames(covid_newcases_wy_ars)[2] <- "Year"
covid_newcases_wy_ars <- covid_newcases_wy_ars[order(covid_newcases_wy_ars[,2],
                                                     covid_newcases_wy_ars[,1]), ]

# 3. Obtain the total number of new cases for each week for each month of a given year of the pandemic by ARS
covid_newcases_wm_ars <-
  covid_ars %>%
  group_by(ceiling(day((covid_ars$data)) / 7), month(covid_ars$data, label = TRUE, abbr = FALSE),
           year(covid_ars$data)) %>% 
  summarise(new_cases_arsnorte = sum(confirmados_novos_arsnorte, na.rm = TRUE),
            newcases_arscentro = sum(confirmados_novos_arscentro, na.rm = TRUE),
            newcases_arslvt = sum(confirmados_novos_arslvt, na.rm = TRUE),
            newcases_arsalentejo = sum(confirmados_novos_arsalentejo, na.rm = TRUE),
            newcases_arsalgarve = sum(confirmados_novos_arsalgarve, na.rm = TRUE))
colnames(covid_newcases_wm_ars)[1] <- "WeekofMonth"
colnames(covid_newcases_wm_ars)[2] <- "Month"
colnames(covid_newcases_wm_ars)[3] <- "Year"
covid_newcases_wm_ars <- covid_newcases_wm_ars[order(covid_newcases_wm_ars[,3],
                                                     covid_newcases_wm_ars[,2]), ]

### Barplots for the Total Number of New Cases by ARS ---------------------------------------------------------

# The labels should be updated everytime a new dataset is imported
label_my <- c("Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20",
              "Nov 20", "Dec 20", "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21")
label_my <- factor(label_my, levels = label_my)

label_wy <- c("W9 20", "W10 20", "W11 20", "W12 20", "W13 20", "W14 20", "W15 20", "W16 20", "W17 20",
              "W18 20", "W19 20", "W20 20", "W21 20", "W22 20", "W23 20", "W24 20", "W25 20", "W26 20",
              "W27 20", "W28 20", "W29 20", "W30 20", "W31 20", "W32 20", "W33 20", "W34 20", "W35 20",
              "W36 20", "W37 20", "W38 20", "W39 20", "W40 20", "W41 20", "W42 20", "W43 20", "W44 20",
              "W45 20", "W46 20", "W47 20", "W48 20", "W49 20", "W50 20", "W51 20", "W52 20", "W53 20",
              "W1 21", "W2 21", "W3 21", "W4 21", "W5 21", "W6 21", "W7 21", "W8 21", "W9 21", "W10 21",
              "W11 21", "W12 21", "W13 21", "W14 21", "W15 21", "W16 21", "W17 21", "W18 21", "W19 21",
              "W20 21", "W21 21",  "W22 21")
label_wy <- factor(label_wy, levels = label_wy)

label_wmy <- c("W4 Fev 20", "W5 Fev 20", "W1 Mar 20", "W2 Mar 20", "W3 Mar 20", "W4 Mar 20", "W5 Mar 20",
               "W1 Apr 20", "W2 Apr 20", "W3 Apr 20", "W4 Apr 20", "W5 Apr 20", "W1 May 20", "W2 May 20",
               "W3 May 20", "W4 May 20", "W5 May 20", "W1 Jun 20", "W2 Jun 20", "W3 Jun 20", "W4 Jun 20",
               "W5 Jun 20", "W1 Jul 20", "W2 Jul 20", "W3 Jul 20", "W4 Jul 20", "W5 Jul 20", "W1 Aug 20",
               "W2 Aug 20", "W3 Aug 20", "W4 Aug 20", "W5 Aug 20", "W1 Sep 20", "W2 Sep 20", "W3 Sep 20",
               "W4 Sep 20", "W5 Sep 20", "W1 Oct 20", "W2 Oct 20", "W3 Oct 20", "W4 Oct 20", "W5 Oct 20",
               "W1 Nov 20", "W2 Nov 20", "W3 Nov 20", "W4 Nov 20", "W5 Nov 20", "W1 Dec 20", "W2 Dec 20",
               "W3 Dec 20", "W4 Dec 20", "W5 Dec 20", "W1 Jan 21", "W2 Jan 21", "W3 Jan 21", "W4 Jan 21",
               "W5 Jan 21", "W1 Fev 21", "W2 Fev 21", "W3 Fev 21", "W4 Fev 21", "W1 Mar 21", "W2 Mar 21",
               "W3 Mar 21", "W4 Mar 21", "W5 Mar 21", "W1 Apr 21", "W2 Apr 21", "W3 Apr 21", "W4 Apr 21",
               "W5 Apr 21", "W1 May 21", "W2 May 21", "W3 May 21", "W4 May 21", "W5 May 21")
label_wmy <- factor(label_wmy, levels = label_wmy)               

# df$date <- with(df, sprintf("%d-%02d", df$Year, df$Month)) --> build new column with aggregated date


# 1. Barplot of the total number of new cases per month of the pandemic by ARS
covid_newcases_my_ars$label_my <- label_my
covid_newcases_my_ars_2 <- covid_newcases_my_ars %>%
  gather(ARS, newcases, -Month, -Year, -label_my )

ggplot(data = covid_newcases_my_ars_2, aes(x = label_my, y = newcases, fill = ARS)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label = newcases), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,20000, 40000, 60000, 80000, 100000, 120000, 140000)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        legend.position = c(0.9, 0.8), legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "ARS (Portugal)") +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte"))

# 2. Lineplot of the total number of new cases per month of the pandemic by ARS
ggplot(data = covid_newcases_my_ars_2, aes(x = label_my, y = newcases, group = ARS)) +
  geom_line(aes(color=ARS), size = 0.8) +
  geom_point(aes(color=ARS), size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Month-Year)") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,20000, 40000, 60000, 80000, 100000, 120000, 140000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_color_discrete(name = "ARS (Portugal)", labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro",
                                                           "ARS LVT", "ARS Norte"))

# 3. Barplot of the total number of new cases for each week of a given year by ARS
covid_newcases_wy_ars$label_wy <- label_wy
covid_newcases_wy_ars_2 <- covid_newcases_wy_ars %>%
  gather(ARS, newcases, -Week, -Year, -label_wy )

ggplot(data = covid_newcases_wy_ars_2, aes(x = label_wy, y = newcases, fill = ARS)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Year)") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "ARS (Portugal)") +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte"))

# 4. Lineplot of the total number of new cases for each week of a given year by ARS
ggplot(data = covid_newcases_wy_ars_2, aes(x = label_wy, y = newcases, group = ARS)) +
  geom_line(aes(color=ARS), size = 0.8) +
  geom_point(aes(color=ARS), size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Year)") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  scale_color_discrete(name = "ARS (Portugal)", labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro",
                                                           "ARS LVT", "ARS Norte"))

# 5. Barplot of the total number of new cases for each week of the month on a given year by ARS
covid_newcases_wm_ars$label_wmy <- label_wmy
covid_newcases_wm_ars_2 <- covid_newcases_wm_ars %>%
  gather(ARS, newcases, -WeekofMonth, -Month, -Year, -label_wmy )

ggplot(data = covid_newcases_wm_ars_2, aes(x = label_wmy, y = newcases, fill = ARS)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Month-Year)") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 6))]) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "ARS (Portugal)") +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte"))

# 6. Lineplot of the total number of new cases for each week of the month on a given year by ARS
ggplot(data = covid_newcases_wm_ars_2, aes(x = label_wmy, y = newcases, group = ARS)) +
  geom_line(aes(color=ARS), size = 0.8) +
  geom_point(aes(color=ARS), size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Month-Year)") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 3))]) +
  scale_color_discrete(name = "ARS (Portugal)", labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro",
                                                           "ARS LVT", "ARS Norte"))

# 7. Barplot of the total number of cases for each day during the pandemic by ARS
covid_ars_2 <- covid_ars %>%
  gather(ARS, newcases, -data)

ggplot(data = covid_ars_2, aes(x = data, y = newcases, fill = ARS)) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of New Confirmed Cases by ARS") +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"), legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "ARS (Portugal)") +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte"))

# 8. Lineplot of the total number of cases for each day during the pandemic by ARS
ggplot(data = covid_ars_2, aes(x = data, y = newcases, group = ARS)) +
  geom_line(aes(color=ARS), size = 0.7) +
  #geom_point(aes(color=ARS), size = 0.9) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total number of new daily confirmed cases by ARS") +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000)) +
  scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y", expand = c(0, 0)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        legend.text = element_text(size = 14), legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "ARS", labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro",
                                                           "ARS LVT", "ARS Norte"))

#######################################################################
###### 06. SCRIPT TO EXPLORE HYPERTENSION DATA FROM 2019 TO 2021 ######
#######################################################################

# ----------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Explore the data from 2019 to 2020
# 3. Draw different plots:
#    - Barplots by year and month for each ARS
#    - Boxplots by year and month for each ARS
# 4. Check for statistical differences
#
# Miguel Farinha 
# (MMA BioStatistics May 2021)
# ----------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
library(RColorBrewer)
library(VIM)
library(DataExplorer)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(reshape2)
library(hrbrthemes)
library(ggpubr)
#library(plyr)
library(gridExtra)

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

# Import the dataset covid_main
hyper <- read.table("hipertensao.csv", sep = ";", header = TRUE)
head(hyper)

# Change the names of columns and set the appropriate datatype for each column of the data
colnames(hyper)
colnames(hyper)[1] <- "Period"
colnames(hyper)[2] <- "Region"
colnames(hyper)[3] <- "Entity"
colnames(hyper)[4] <- "GPS"
colnames(hyper)[5] <- "TotalBP"
colnames(hyper)[6] <- "Under65BP"

# as.Date requires a day, thus we consider a new registry occurs on the 1st day of each month
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("RegiÃ£o", "Região", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("SaÃºde", "Saúde", x)
}))
hyper$Region <- as.character(hyper$Region)
hyper$TotalBP <- as.numeric(hyper$TotalBP)
hyper$Under65BP <- as.numeric(hyper$Under65BP)

# Check datatypes and summary of data
head(hyper)
summary(hyper)
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")
min(hyper$Period) # oldest registration: "2014-01"
max(hyper$Period) # newest registration: "2021-02"

# Check if there are missing values (NA)
sum(is.na(hyper$Period)) # 0
sum(is.na(hyper$Region)) # 0
sum(is.na(hyper$Entity)) # 0
sum(is.na(hyper$GPS)) # 0
sum(is.na(hyper$TotalBP)) # 0
sum(is.na(hyper$Under65BP)) # 0

# Create a new column with ones to obtain the number of registrations ahead
hyper <- cbind(hyper, regs = 1)

# Count the number of registrations for each year
sum(hyper[(hyper$Period > "2013-01-01" & hyper$Period < "2014-12-31"), c("regs")]) # 660
sum(hyper[(hyper$Period > "2014-12-31" & hyper$Period < "2015-12-31"), c("regs")]) # 660
sum(hyper[(hyper$Period > "2015-12-31" & hyper$Period < "2016-12-31"), c("regs")]) # 645
sum(hyper[(hyper$Period > "2016-12-31" & hyper$Period < "2017-12-31"), c("regs")]) # 660
sum(hyper[(hyper$Period > "2017-12-31" & hyper$Period < "2018-12-31"), c("regs")]) # 660
sum(hyper[(hyper$Period > "2018-12-31" & hyper$Period < "2019-12-31"), c("regs")]) # 656
sum(hyper[(hyper$Period > "2019-12-31" & hyper$Period < "2020-12-31"), c("regs")]) # 660
sum(hyper[(hyper$Period > "2020-12-31" & hyper$Period < "2021-12-31"), c("regs")]) # 110

# Select only registrations concerning the period from 2019 to 2020
hyper_from19_to21 <- hyper[which(year(hyper$Period) > "2018-12-31"),]
min(hyper_from19_to21$Period) # "2019-01-01"
max(hyper_from19_to21$Period) # "2021-02-01"
label_ars <- c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")
hyper_from19_to21$Period <- as.Date(hyper_from19_to21$Period, format="%d-%m-%Y", tz="GMT")

# ---- BARPLOTS TO SUMMARIZE DATA ---------------------------------------------------------------------------------

# 1. Summary data from 2019 to 2020 by ARS
hyper_from19_to21_ars <- 
  hyper_from19_to21 %>%
  group_by(year(hyper_from19_to21$Period), hyper_from19_to21$Region) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_from19_to21_ars)[1] <- "Year"
colnames(hyper_from19_to21_ars)[2] <- "ARS"
hyper_from19_to21_ars <- hyper_from19_to21_ars[order(hyper_from19_to21_ars[,2], hyper_from19_to21_ars[,1]), ]

ggplot(data = hyper_from19_to21_ars, aes(x = ARS, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300)) +
  scale_x_discrete(labels = label_ars) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data = hyper_from19_to21_ars, aes(x = ARS, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)) +
  scale_x_discrete(labels = label_ars) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data = hyper_from19_to21_ars, aes(x = ARS, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.6)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  scale_x_discrete(labels = label_ars) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2. Summary data, for each individual ARS, for each month by year

hyper_from19_to21_my_ars <- 
  hyper_from19_to21 %>%
  group_by(month(hyper_from19_to21$Period, label = TRUE, abbr = FALSE), year(hyper_from19_to21$Period),
           hyper_from19_to21$Region) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_from19_to21_my_ars)[1] <- "Month"
colnames(hyper_from19_to21_my_ars)[2] <- "Year"
colnames(hyper_from19_to21_my_ars)[3] <- "ARS"
hyper_from19_to21_my_ars <- hyper_from19_to21_my_ars[order(hyper_from19_to21_my_ars[,3],
                                                           hyper_from19_to21_my_ars[,1],
                                                           hyper_from19_to21_my_ars[,2]), ]

hyper_from19_to21_alentejo <- hyper_from19_to21_my_ars[which(hyper_from19_to21_my_ars$ARS == "Região de Saúde do Alentejo"),]
hyper_from19_to21_algarve <- hyper_from19_to21_my_ars[which(hyper_from19_to21_my_ars$ARS == "Região de Saúde do Algarve"),]
hyper_from19_to21_centro <- hyper_from19_to21_my_ars[which(hyper_from19_to21_my_ars$ARS == "Região de Saúde do Centro"),]
hyper_from19_to21_lvt <- hyper_from19_to21_my_ars[which(hyper_from19_to21_my_ars$ARS == "Região de Saúde LVT"),]
hyper_from19_to21_norte <- hyper_from19_to21_my_ars[which(hyper_from19_to21_my_ars$ARS == "Região de Saúde Norte"),]

# 2.1 Alentejo
ggplot(data = hyper_from19_to21_alentejo, aes(x = Month, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Alentejo)") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.2 Algarve
ggplot(data = hyper_from19_to21_algarve, aes(x = Month, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Algarve)") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,1,2,3)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.3 Centro
ggplot(data = hyper_from19_to21_centro, aes(x = Month, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Centro)") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.4 LVT
ggplot(data = hyper_from19_to21_lvt, aes(x = Month, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS LVT)") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.5 Norte
ggplot(data = hyper_from19_to21_norte, aes(x = Month, y = total_regs, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Norte)") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.6 Alentejo - BP
ggplot(data = hyper_from19_to21_alentejo, aes(x = Month, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Alentejo)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

hyper[which(hyper$Region == "Região de Saúde do Alentejo" & year(hyper$Period) == "2019"
      & month(hyper$Period) == "05"),] # No registrations in May 2019 in ARS Alentejo

# 2.7 Algarve - BP
ggplot(data = hyper_from19_to21_algarve, aes(x = Month, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Algarve)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.8 Centro - BP
ggplot(data = hyper_from19_to21_centro, aes(x = Month, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Centro)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.9 LVT - BP
ggplot(data = hyper_from19_to21_lvt, aes(x = Month, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS LVT)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.10 Norte - BP
ggplot(data = hyper_from19_to21_norte, aes(x = Month, y = meanBP, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.8)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Month (ARS Norte)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.11 Alentejo - BP under 65
ggplot(data = hyper_from19_to21_alentejo, aes(x = Month, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.7)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS (Alentejo)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.12 Algarve - BP under 65
ggplot(data = hyper_from19_to21_algarve, aes(x = Month, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.7)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS (Algarve)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.13 Centro
ggplot(data = hyper_from19_to21_centro, aes(x = Month, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.7)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS (Centro)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.14 LVT - BP under 65
ggplot(data = hyper_from19_to21_lvt, aes(x = Month, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.7)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS (LVT)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# 2.15 Norte - BP under 65
ggplot(data = hyper_from19_to21_norte, aes(x = Month, y = meanBP65, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.7)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS (Norte)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 13), legend.title = element_text(size = 16)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

# ----------------------------------------------------------------------------------------------------------------
# ---- BOXPLOTS FOR EACH ARS BY YEAR AND MONTH -------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# 3. ARS Alentejo ------------------------------------------------------------------------------------------------
summary(hyper_from19_to21)
alentejo1921 <- hyper_from19_to21[which(hyper_from19_to21$Region == "Região de Saúde do Alentejo"),]
Sys.setlocale("LC_TIME", "C")
alentejo1921$mes <- month(alentejo1921$Period, label = TRUE, abbr = FALSE)
alentejo1921$mes <- as.factor(alentejo1921$mes)
alentejo1921$mesabbr <- month(alentejo1921$Period, label = TRUE, abbr = TRUE)
alentejo1921$mesabbr <- as.factor(alentejo1921$mesabbr)
alentejo1921$ano <- year(alentejo1921$Period)
alentejo1921$ano <- as.factor(alentejo1921$ano)
summary(alentejo1921)
alentejo1920 <- alentejo1921[-which(year(alentejo1921$Period) == "2021"),] # remove year 2021

ggplot(data=alentejo1921, aes(x = ano, y = TotalBP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Alentejo") +
  #scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=alentejo1920, aes(x = mesabbr, y = TotalBP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Alentejo") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

ggplot(data=alentejo1921, aes(x = ano, y = Under65BP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Alentejo") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=alentejo1920, aes(x = mesabbr, y = Under65BP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Alentejo") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

# 4. ARS Algarve --------------------------------------------------------------------------------------------------
summary(hyper_from19_to21)
algarve1921 <- hyper_from19_to21[which(hyper_from19_to21$Region == "Região de Saúde do Algarve"),]
Sys.setlocale("LC_TIME", "C")
algarve1921$mes <- month(algarve1921$Period, label = TRUE, abbr = FALSE)
algarve1921$mes <- as.factor(algarve1921$mes)
algarve1921$mesabbr <- month(algarve1921$Period, label = TRUE, abbr = TRUE)
algarve1921$mesabbr <- as.factor(algarve1921$mesabbr)
algarve1921$ano <- year(algarve1921$Period)
algarve1921$ano <- as.factor(algarve1921$ano)
summary(algarve1921)
algarve1920 <- algarve1921[-which(year(algarve1921$Period) == "2021"),] # remove year 2021

ggplot(data=algarve1921, aes(x = ano, y = TotalBP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Algarve") +
  #scale_y_continuous(breaks = c(0,500, 1000,2000,3000,4000,5000,6000,7000)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=algarve1920, aes(x = mesabbr, y = TotalBP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Algarve") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

ggplot(data=algarve1921, aes(x = ano, y = Under65BP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Algarve") +
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=algarve1920, aes(x = mesabbr, y = Under65BP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Algarve") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

# 5. ARS Centro --------------------------------------------------------------------------------------------------
summary(hyper_from19_to21)
centro1921 <- hyper_from19_to21[which(hyper_from19_to21$Region == "Região de Saúde do Centro"),]
Sys.setlocale("LC_TIME", "C")
centro1921$mes <- month(centro1921$Period, label = TRUE, abbr = FALSE)
centro1921$mes <- as.factor(centro1921$mes)
centro1921$mesabbr <- month(centro1921$Period, label = TRUE, abbr = TRUE)
centro1921$mesabbr <- as.factor(centro1921$mesabbr)
centro1921$ano <- year(centro1921$Period)
centro1921$ano <- as.factor(centro1921$ano)
summary(centro1921)
centro1920 <- centro1921[-which(year(centro1921$Period) == "2021"),] # remove year 2021

ggplot(data=centro1921, aes(x = ano, y = TotalBP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Centro") +
  #scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=centro1920, aes(x = mesabbr, y = TotalBP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Centro") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

ggplot(data=centro1921, aes(x = ano, y = Under65BP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Centro") +
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=centro1920, aes(x = mesabbr, y = Under65BP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Centro") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

# 5. ARS LVT ------------------------------------------------------------------------------------------------------
summary(hyper_from19_to21)
lvt1921 <- hyper_from19_to21[which(hyper_from19_to21$Region == "Região de Saúde LVT"),]
Sys.setlocale("LC_TIME", "C")
lvt1921$mes <- month(lvt1921$Period, label = TRUE, abbr = FALSE)
lvt1921$mes <- as.factor(lvt1921$mes)
lvt1921$mesabbr <- month(lvt1921$Period, label = TRUE, abbr = TRUE)
lvt1921$mesabbr <- as.factor(lvt1921$mesabbr)
lvt1921$ano <- year(lvt1921$Period)
lvt1921$ano <- as.factor(lvt1921$ano)
summary(lvt1921)
lvt1920 <- lvt1921[-which(year(lvt1921$Period) == "2021"),] # remove year 2021

ggplot(data=lvt1921, aes(x = ano, y = TotalBP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS LVT") +
  #scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=lvt1920, aes(x = mesabbr, y = TotalBP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS LVT") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

ggplot(data=lvt1921, aes(x = ano, y = Under65BP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS LVT") +
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=lvt1920, aes(x = mesabbr, y = Under65BP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS LVT") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

# 6. ARS Norte ------------------------------------------------------------------------------------------------------
summary(hyper_from19_to21)
norte1921 <- hyper_from19_to21[which(hyper_from19_to21$Region == "Região de Saúde Norte"),]
Sys.setlocale("LC_TIME", "C")
norte1921$mes <- month(norte1921$Period, label = TRUE, abbr = FALSE)
norte1921$mes <- as.factor(norte1921$mes)
norte1921$mesabbr <- month(norte1921$Period, label = TRUE, abbr = TRUE)
norte1921$mesabbr <- as.factor(norte1921$mesabbr)
norte1921$ano <- year(norte1921$Period)
norte1921$ano <- as.factor(norte1921$ano)
summary(norte1921)
norte1920 <- norte1921[-which(year(norte1921$Period) == "2021"),] # remove year 2021

ggplot(data=norte1921, aes(x = ano, y = TotalBP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Norte") +
  #scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=norte1920, aes(x = mesabbr, y = TotalBP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  xlab("ARS Norte") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

ggplot(data=norte1921, aes(x = ano, y = Under65BP, group = ano)) +
  geom_boxplot(aes(fill = factor(year(Period))), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Norte") +
  #scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) + 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~mes, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2019", "2020", "2021"))

ggplot(data=norte1920, aes(x = mesabbr, y = Under65BP, fill = mesabbr)) +
  geom_boxplot(aes(fill = factor(mesabbr)), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  ylab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  xlab("ARS Norte") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) +
  facet_wrap(~ano) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=1.2, linetype="solid"),
        strip.text = element_text(size = 13)) +
  labs(fill = "Month")

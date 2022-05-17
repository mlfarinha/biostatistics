#####################################################
###### 06. SCRIPT TO EXPLORE HYPERTENSION DATA ######
#####################################################

# ----------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Explore the data
# 3. Draw different plots:
#    - Barplots and lineplots by year, month, ARS
#    - Boxplots
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
library(forecast)

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
# The number of registrations was kept relatively constant even throughout the pandemic

# 1. Obtain summary data for each year
hyper_y <- 
  hyper %>%
  group_by(year(hyper$Period)) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_y)[1] <- "Year"
hyper_y <- hyper_y[order(hyper_y[,1]), ]

# 2. Obtain summary data for each month of each year
hyper_my <- 
  hyper %>%
  group_by(month(hyper$Period, label = TRUE, abbr = FALSE), year(hyper$Period)) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_my)[1] <- "Month"
colnames(hyper_my)[2] <- "Year"
hyper_my <- hyper_my[order(hyper_my[,2]), ]

# 3. Obtain summary data for each ARS
hyper_ars <- 
  hyper %>%
  group_by(hyper$Region) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_ars)[1] <- "ARS"
hyper_ars <- hyper_ars[order(hyper_ars[,1]), ]

# 4. Obtain summary data for each year by ARS
hyper_y_ars <- 
  hyper %>%
  group_by(year(hyper$Period), hyper$Region) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_y_ars)[1] <- "Year"
colnames(hyper_y_ars)[2] <- "ARS"
hyper_y_ars <- hyper_y_ars[order(hyper_y_ars[,2], hyper_y_ars[,1]), ]

# 5. Obtain summary data for each month of each year by ARS
hyper_my_ars <- 
  hyper %>%
  group_by(month(hyper$Period, label = TRUE, abbr = FALSE), year(hyper$Period), hyper$Region) %>% 
  summarise(total_regs = sum(regs, na.rm = TRUE),
            meanBP = mean(TotalBP, na.rm = TRUE),
            sdBP = sd(TotalBP, na.rm = TRUE),
            meanBP65 = mean(Under65BP, na.rm = TRUE),
            sdBP65 = sd(Under65BP, na.rm = TRUE))
colnames(hyper_my_ars)[1] <- "Month"
colnames(hyper_my_ars)[2] <- "Year"
colnames(hyper_my_ars)[3] <- "ARS"
hyper_my_ars <- hyper_my_ars[order(hyper_my_ars[,3], hyper_my_ars[,1], hyper_my_ars[,2]), ]


### Barplots for the Summary Measures --------------------------------------------------------------------------

# The labels should be updated everytime a new dataset is imported
label_y <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
label_y <- factor(label_y, levels = label_y)

label_my <- c("Jan 14", "Feb 14", "Mar 14", "Apr 14", "May 14", "Jun 14", "Jul 14", "Aug 14", "Sep 14", "Oct 14",
              "Nov 14", "Dec 14", "Jan 15", "Feb 15", "Mar 15", "Apr 15", "May 15", "Jun 15", "Jul 15", "Aug 15",
              "Sep 15", "Oct 15", "Nov 15", "Dec 15", "Jan 16", "Feb 16", "Mar 16", "Apr 16", "May 16", "Jun 16",
              "Jul 16", "Aug 16", "Sep 16", "Oct 16", "Nov 16", "Dec 16", "Jan 17", "Feb 17", "Mar 17", "Apr 17",
              "May 17", "Jun 17", "Jul 17", "Aug 17", "Sep 17", "Oct 17", "Nov 17", "Dec 17", "Jan 18", "Feb 18",
              "Mar 18", "Apr 18", "May 18", "Jun 18", "Jul 18", "Aug 18", "Sep 18", "Oct 18", "Nov 18", "Dec 18",
              "Jan 19", "Feb 19", "Mar 19", "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19",
              "Nov 19", "Dec 19", "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20",
              "Sep 20", "Oct 20", "Nov 20", "Dec 20", "Jan 21", "Feb 21")
label_my <- factor(label_my, levels = label_my)

label_ars <- c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")
#label_ars <- factor(label_ars, levels = label_ars)

# df$date <- with(df, sprintf("%d-%02d", df$Year, df$Month)) --> build new column with aggregated date

# ---- YEAR -----------------------------------------------------------------------------

# 1. Barplot of the total number of registrations per year
ggplot(data = hyper_y, aes(x = label_y, y = total_regs)) +
  geom_bar(stat = "identity",color = "dodgerblue", fill = "dodgerblue", width = 0.8) +
  geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Year") +
  ylab("Total number of registrations") +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank())

# 2. Barplot and Lineplot of the mean number of patients with Hypertension with BP < 150/90 mmHg (last 6 months)
ggplot(data = hyper_y, aes(x = label_y, y = meanBP)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.7) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.2, position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Date (Year)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_y, aes(x = label_y, y = meanBP, group = 1)) +
  geom_line(color = "blue", size = 0.8) +
  geom_point(color = "blue", size = 2) +
  geom_text(aes(label=round(meanBP,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.2, position = position_dodge(0.3)) +
  xlab("Date (Year)") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 3. Barplot of the mean of patients with hypertension age < 65 with BP < 150/90 mmHg
ggplot(data = hyper_y, aes(x = label_y, y = meanBP65)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.7) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.2,
                position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Date (Year)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_y, aes(x = label_y, y = meanBP65, group = 1)) +
  geom_line(color = "blue", size = 0.8) +
  geom_point(color = "blue", size = 2) +
  geom_text(aes(label=round(meanBP65,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.2,
                position = position_dodge(0.3)) +
  xlab("Date (Year)") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))


# ---- MONTH/YEAR ------------------------------------------------------------------------------------------------

# 4. Barplot of the total number of registrations per month of each year
ggplot(data = hyper_my, aes(x = label_my, y = total_regs)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.8) +
  #geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Date") +
  ylab("Total Number of Registrations") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 5))]) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 5. Barplot and Lineplot of the mean number of patients with Hypertension with BP < 150/90 mmHg (last 6 months)
ggplot(data = hyper_my, aes(x = label_my, y = meanBP)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.6) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Date") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 5))]) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_my, aes(x = label_my, y = meanBP, group = 1)) +
  geom_line(color = "blue", size = 0.6) +
  geom_point(color = "blue", size = 1.5) +
  #geom_text(aes(label=round(meanBP,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.3)) +
  xlab("Date") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 5))]) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 6. Barplot and Lineplot of the mean of patients with hypertension age < 65 with BP < 150/90 mmHg (month and year)
ggplot(data = hyper_my, aes(x = label_my, y = meanBP65)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.6) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("Date") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 5))]) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_my, aes(x = label_my, y = meanBP65, group = 1)) +
  geom_line(color = "blue", size = 0.6) +
  geom_point(color = "blue", size = 1.5) +
  #geom_text(aes(label=round(meanBP65,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3,
                position = position_dodge(0.3)) +
  xlab("Date") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80)) +
  scale_x_discrete(breaks = levels(label_my)[c(T, rep(F, 5))]) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# ---- ARS -------------------------------------------------------------------------------------------------------

# 7. Barplot of the total number of registrations per ARS
ggplot(data = hyper_ars, aes(x = label_ars, y = total_regs)) +
  geom_bar(stat = "identity",color = "dodgerblue", fill = "dodgerblue", width = 0.8) +
  geom_text(aes(label = total_regs), position = position_dodge(width=1), vjust=-0.7, size = 3.7) +
  xlab("ARS") +
  ylab("Total number of registrations") +
  scale_y_continuous(breaks = c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,2200)) +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank())

# 8. Barplot and Lineplot of the mean number of patients with Hypertension with BP < 150/90 mmHg per ARS
ggplot(data = hyper_ars, aes(x = label_ars, y = meanBP)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.6) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.2, position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_ars, aes(x = label_ars, y = meanBP, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 1.7) +
  #geom_text(aes(label=round(meanBP,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.2, position = position_dodge(0.3)) +
  xlab("ARS") +
  ylab("Mean Number of Patients with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 9. Barplot and Lineplot of the mean of patients with hypertension age < 65 with BP < 150/90 mmHg per ARS
ggplot(data = hyper_ars, aes(x = label_ars, y = meanBP65)) +
  geom_bar(stat = "identity",color = "blue", fill = "blue", width = 0.6) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.2,
                position = position_dodge(0.3)) +
  #geom_text(aes(label = meanBP), position = position_dodge(width=1), vjust=-0.7, size = 3.5) +
  xlab("ARS") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

ggplot(data = hyper_ars, aes(x = label_ars, y = meanBP65, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 1.5) +
  #geom_text(aes(label=round(meanBP65,2)), vjust=-1, size = 3.5) +
  geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.2,
                position = position_dodge(0.3)) +
  xlab("ARS") +
  ylab("Mean of Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80)) +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(angle=90, vjust=1.5),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# ---- ARS-YEAR --------------------------------------------------------------------------------------------------

# 10. Barplot of the total number of registrations per ARS for each year
ggplot(data = hyper_y_ars, aes(x = ARS, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 11. Barplot and Lineplot of the mean number of patients with Hypertension with BP < 150/90 mmHg per year and ARS
ggplot(data = hyper_y_ars, aes(x = ARS, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

ggplot(data = hyper_y_ars, aes(x = ARS, y = meanBP, group = factor(Year))) +
  geom_line(aes(color=factor(Year)), size = 1) +
  geom_point(aes(color=factor(Year)), size = 1.5) +
  #geom_text(aes(label=round(meanBP,2)), vjust=-1, size = 3.5) +
  #geom_errorbar(aes(ymin=meanBP-sdBP, ymax=meanBP+sdBP), size = 0.8, width = 0.3, position = position_dodge(0.1)) +
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
  scale_color_discrete(name = "Year", labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 12. Barplot and Lineplot of the mean of patients with hypertension age < 65 with BP < 150/90 mmHg by ARS and year
ggplot(data = hyper_y_ars, aes(x = ARS, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

ggplot(data = hyper_y_ars, aes(x = ARS, y = meanBP65, group = factor(Year))) +
  geom_line(aes(color=factor(Year)), size = 1) +
  geom_point(aes(color=factor(Year)), size = 1.5) +
  #geom_text(aes(label=round(meanBP65,2)), vjust=-1, size = 3.5) +
  #geom_errorbar(aes(ymin=meanBP65-sdBP65, ymax=meanBP65+sdBP65), size = 0.8, width = 0.3, 
  #position = position_dodge(0.3)) +
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
  scale_color_discrete(name = "Year", labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# ---- ARS-MONTH-YEAR -------------------------------------------------------------------------------------------
# Since the plots would be unreadable, we plot each ARS individually and analyse the monthly changes

hyper_alentejo <- hyper_my_ars[which(hyper_my_ars$ARS == "Região de Saúde do Alentejo"),]
hyper_algarve <- hyper_my_ars[which(hyper_my_ars$ARS == "Região de Saúde do Algarve"),]
hyper_centro <- hyper_my_ars[which(hyper_my_ars$ARS == "Região de Saúde do Centro"),]
hyper_lvt <- hyper_my_ars[which(hyper_my_ars$ARS == "Região de Saúde LVT"),]
hyper_norte <- hyper_my_ars[which(hyper_my_ars$ARS == "Região de Saúde Norte"),]

# 13. Barplot of the total number of registrations per ARS:
# 13.1 Alentejo
ggplot(data = hyper_alentejo, aes(x = Month, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 13.2 Algarve
ggplot(data = hyper_algarve, aes(x = Month, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 13.3 Centro
ggplot(data = hyper_centro, aes(x = Month, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 13.4 LVT
ggplot(data = hyper_lvt, aes(x = Month, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 13.5 Norte
ggplot(data = hyper_norte, aes(x = Month, y = total_regs, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 14. Barplot of the mean number of patients with Hypertension with BP < 150/90 mmHg
# 14.1 Alentejo
ggplot(data = hyper_alentejo, aes(x = Month, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 14.2 Algarve
ggplot(data = hyper_algarve, aes(x = Month, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 14.3 Centro
ggplot(data = hyper_centro, aes(x = Month, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 14.4 LVT
ggplot(data = hyper_lvt, aes(x = Month, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 14.5 Norte
ggplot(data = hyper_norte, aes(x = Month, y = meanBP, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 15. Barplot of the mean of patients with hypertension age < 65 with BP < 150/90 mmHg
# 15.1 Alentejo
ggplot(data = hyper_alentejo, aes(x = Month, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 15.2 Algarve
ggplot(data = hyper_algarve, aes(x = Month, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 15.3 Centro
ggplot(data = hyper_centro, aes(x = Month, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 15.4 LVT
ggplot(data = hyper_lvt, aes(x = Month, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# 15.5 Norte
ggplot(data = hyper_norte, aes(x = Month, y = meanBP65, fill = factor(Year))) +
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
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# MONTHLY TREND PLOT -----------------------------------------------------------------------------------------------
hyper_my <- transform(hyper_my, Date = as.Date(paste(Year, Month, "15", sep = "-"), format = "%Y-%b-%d"))
ggplot(data = hyper_my, aes(x = Date, y = meanBP65)) +
  geom_line(color = "dodgerblue", size = 0.7) +
  xlab("Date") +
  ylab("Mean proportion of patients with condition") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55)) +
  scale_x_date(limits = as.Date(c("2014-01-01","2021-02-15")), date_breaks = "6 months", date_labels =  "%b %y",
               expand = c(0,0)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1))

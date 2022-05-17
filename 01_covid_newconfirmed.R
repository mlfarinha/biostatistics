######################################################################
###### 01. SCRIPT TO ORGANIZE COVID DATA BY NEW CONFIRMED CASES ######
######################################################################

# -----------------------------------------------------------------
# 1. Import the necessary data (covid_main.csv)
# 2. Explore the number of new confirmed cases
# 3. Plots with different levels of detail (daily, weekly, monthly)
#
# Miguel Farinha (MMA BioStatistics 2020-2021)
# -----------------------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
library(RColorBrewer)
library(VIM)
library(DataExplorer)
library(lubridate)
library(scales)

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

# Import the dataset covid_main
covid_main <- read.table("covid_main.csv", sep = ",", header = TRUE)
head(covid_main)
colnames(covid_main)

# Set the datatype Date for the variable data
covid_main$data <- as.Date(covid_main$data, format="%d-%m-%Y", tz="GMT")
covid_main$confirmados_novos <- as.numeric((covid_main$confirmados_novos))
head(covid_main$data)
class(covid_main$data) # class: "Date"
min(covid_main$data) # oldest registration: "2020-02-26"
max(covid_main$data) # newest registration: "2021-06-06" (dependent on data download date)
covid_main <- covid_main[-which(covid_main$data > "2021-05-31"),]
month(covid_main$data, label = TRUE, abbr = FALSE) # obtain the months of each cell in data
epiweek((covid_main$data)) # obtain the epidemiological week in data

# 1. Obtain the total number of new cases for each month of each year of the pandemic
covid_newcases_monthyear <-
  covid_main %>%
  group_by(month(covid_main$data, label = TRUE, abbr = FALSE), year(covid_main$data)) %>% 
                                  summarise(total_new_cases = sum(confirmados_novos, na.rm = TRUE))
colnames(covid_newcases_monthyear)[1] <- "Month"
colnames(covid_newcases_monthyear)[2] <- "Year"
colnames(covid_newcases_monthyear)[3] <- "Total New Cases"
covid_newcases_monthyear <- covid_newcases_monthyear[order(covid_newcases_monthyear[,2]), ]

# 2. Obtain the total number of new cases for each week, month and year (weeks may belong to different months)
covid_newcases_weekmonthyear <-
 covid_main %>%
 group_by(week(covid_main$data), month(covid_main$data, label = TRUE, abbr = FALSE), year(covid_main$data)) %>% 
                                 summarise(total_new_cases = sum(confirmados_novos, na.rm = TRUE))
colnames(covid_newcases_weekmonthyear)[1] <- "Week"
colnames(covid_newcases_weekmonthyear)[2] <- "Month"
colnames(covid_newcases_weekmonthyear)[3] <- "Year"
colnames(covid_newcases_weekmonthyear)[4] <- "Total New Cases"
covid_newcases_weekmonthyear <- covid_newcases_weekmonthyear[order(covid_newcases_weekmonthyear[,3],
                                                             covid_newcases_weekmonthyear[,1]), ]

# 3. Obtain the total number of new cases for each week of a given year
covid_newcases_weekyear <-
  covid_main %>%
  group_by(week(covid_main$data), year(covid_main$data)) %>% 
                                  summarise(total_new_cases = sum(confirmados_novos, na.rm = TRUE))
colnames(covid_newcases_weekyear)[1] <- "Week"
colnames(covid_newcases_weekyear)[2] <- "Year"
colnames(covid_newcases_weekyear)[3] <- "Total New Cases"
covid_newcases_weekyear <- covid_newcases_weekyear[order(covid_newcases_weekyear[,2],
                                                         covid_newcases_weekyear[,1]), ]

# 4. Obtain the total number of new cases for each week for each month of a given year of the pandemic
covid_newcases_weekofmonth <-
  covid_main %>%
  group_by(ceiling(day((covid_main$data)) / 7), month(covid_main$data, label = TRUE, abbr = FALSE),
           year(covid_main$data)) %>% 
                                  summarise(total_new_cases = sum(confirmados_novos, na.rm = TRUE))
colnames(covid_newcases_weekofmonth)[1] <- "WeekofMonth"
colnames(covid_newcases_weekofmonth)[2] <- "Month"
colnames(covid_newcases_weekofmonth)[3] <- "Year"
colnames(covid_newcases_weekofmonth)[4] <- "Total New Cases"
covid_newcases_weekofmonth <- covid_newcases_weekofmonth[order(covid_newcases_weekofmonth[,3],
                                                               covid_newcases_weekofmonth[,2]), ]

### Barplots for the Total Number of New Cases ----------------------------------------------------------------

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


# 1. Barplot of the total number of new cases per month of the pandemic
colnames(covid_newcases_monthyear)[3] <- "total"
ggplot(data = covid_newcases_monthyear, aes(x = label_my, y = total)) + # aes(fill=factor(label_dates))
  geom_bar(stat = "identity", color = "dodgerblue", fill = "dodgerblue", width = 0.7) +
  geom_text(aes(label = total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total number of new confirmed cases") +
  scale_y_continuous(breaks = c(0,50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())

# 2. Barplot of the total number of new cases for each week of a given year
colnames(covid_newcases_weekyear)[3] <- "total"
ggplot(data = covid_newcases_weekyear, aes(x = label_wy, y = total)) +
  geom_bar(stat = "identity", color = "dodgerblue", fill = "dodgerblue", width = 0.5, position = position_dodge(width = 0.9)) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Year)") +
  ylab("Total Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 3. Lineplot of the total number of new cases for each week of a given year
ggplot(data = covid_newcases_weekyear, aes(x = label_wy, y = total, group = 1)) +
  geom_line(color = "dodgerblue", size = 1) +
  geom_point(size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Year)") +
  ylab("Total Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000)) +
  scale_x_discrete(breaks = levels(label_wy)[c(T, rep(F, 3))]) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 4. Barplot of the total number of new cases for each week of the month on a given year
colnames(covid_newcases_weekofmonth)[4] <- "total"
ggplot(data = covid_newcases_weekofmonth, aes(x = label_wmy, y = total)) +
  geom_bar(stat = "identity", color = "dodgerblue", fill = "dodgerblue", width = 0.5,
           position = position_dodge(width = 0.9)) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Month-Year)") +
  ylab("Total Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 6))]) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 5. Lineplot of the total number of new cases for each week of the month on a given year
ggplot(data = covid_newcases_weekofmonth, aes(x = label_wmy, y = total, group = 1)) +
  geom_line(color = "dodgerblue", size = 1) +
  geom_point(size = 2) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date (Week-Month-Year)") +
  ylab("Total Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000)) +
  scale_x_discrete(breaks = levels(label_wmy)[c(T, rep(F, 6))]) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 6. Barplot of the total number of cases for each day during the pandemic
covid_by_day <- covid_main[,c("data", "confirmados_novos")]
ggplot(data = covid_by_day, aes(x = data, y = confirmados_novos)) +
  geom_bar(stat = "identity", color = "dodgerblue", fill = "dodgerblue", width = 0.5,
           position = position_dodge(width = 0.9)) +
  #geom_text(aes(label=total), vjust=-0.5, size = 3.7) +
  xlab("Date") +
  ylab("Total Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0))

# 7. Lineplot of the total number of cases for each day during the pandemic
ggplot(data = covid_by_day, aes(x = data, y = confirmados_novos, group = 1)) +
  geom_line(color = "dodgerblue", size = 0.7) +
  #geom_point(size = 0.2) +
  xlab("Date") +
  ylab("Total number of new daily confirmed cases") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) +
  scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y", expand = c(0, 0)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = as.numeric(covid_by_day$data[c(23,69,325,430)]), linetype=4,
             color = c("red","red", "green", "green"), size = 1.1)


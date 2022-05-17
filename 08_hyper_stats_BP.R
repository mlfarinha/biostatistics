########################################################################################
###### 07. SCRIPT FOR STATISTICAL ANALYSIS OF HYPERTENSION DATA FROM 2019 TO 2020 ######
########################################################################################

# ----------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Check for statistical differences in 2019 vs 2020
# 3. Patients with Hypertension With BP < 150/90 mmHg
#
# Miguel Farinha 
# (MMA BioStatistics May 2021)
# ----------------------------------------------------

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
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

# Select only registrations relative to the period from 2019 to 2020
hyper_19to20 <- hyper[which(year(hyper$Period) > "2018-12-31" & year(hyper$Period) < "2020-12-31"),]
min(hyper_19to20$Period) # "2019-01-01"
max(hyper_19to20$Period) # "2020-12-01"
label_ars <- c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")
summary(hyper_19to20)
hyper_19to20$ano <- year(hyper_19to20$Period)
hyper_19to20$ano <- as.factor(hyper_19to20$ano)
hyper_19to20$mes <- month(hyper_19to20$Period, abbr = TRUE, label = TRUE)
hyper_19to20$mes <- as.factor(hyper_19to20$mes)
summary(hyper_19to20)


# -----------------------------------------------------------------------------------------------------------------
# Statistical Tests -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

# 2019 VS 2020 (Yearly Comparison) --------------------------------------------------------------------------------

require(plyr)
# Density plots for the number of patients with hypertension with BP < 150/90 mmHg
mu1 <- ddply(hyper_19to20, "ano", summarise, grp.mean=mean(TotalBP))
ggplot(hyper_19to20, aes(x=TotalBP, color = ano)) +
  geom_density(size = 1) +
  geom_vline(data=mu1, aes(xintercept=grp.mean, color = ano), linetype="dashed", size=1) +
  xlab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  ylab("Density") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.position = c(0.67, 0.75),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "Year", labels = c("2019", "2020"))

# Check equality of the mean Number of Patients with Hypertension with BP < 150/90 mmHg
# Assumption 1: The measurements from different years are independent
# Assumption 2: Normality ---> Reject
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano == "2019"), c("TotalBP")], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano == "2020"), c("TotalBP")], color = "blue"),
             ncol=2)
# Shapiro-Wilk test for normality
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019"), c("TotalBP")]) # p < 2.2e-16
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020"), c("TotalBP")]) # p < 2.2e-16
# Rej H0 -> sample doesn't follow normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Reject
var1.ftest <- var.test(TotalBP ~ hyper_19to20$ano, data = hyper_19to20)
var1.ftest # p-value < 2.2e-16

# Non-parametric tests should be considered, since the assumptions of normality and equality of variances were Rej

# Non-Parametric tests
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019"), c("TotalBP")],
            hyper_19to20[which(hyper_19to20$ano == "2020"), c("TotalBP")], alternative = "two.sided",
            paired = FALSE, exact = FALSE, correct = TRUE)
# Reject the hypothesis of equality of means at 5%, 10% significance level ---> p < 2.2e-16

# Effect size Hedge's
n1 <- length(hyper_19to20[hyper_19to20[,c("ano")]==2019,c("TotalBP")])
n2 <- length(hyper_19to20[hyper_19to20[,c("ano")]==2020,c("TotalBP")])
m1 <- mean(hyper_19to20[hyper_19to20[,c("ano")]==2019,c("TotalBP")])
m2 <- mean(hyper_19to20[hyper_19to20[,c("ano")]==2020,c("TotalBP")])
sd1 <- sd(hyper_19to20[hyper_19to20[,c("ano")]==2019,c("TotalBP")])
sd2 <- sd(hyper_19to20[hyper_19to20[,c("ano")]==2020,c("TotalBP")])
s1 <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))
eff1 <- abs(m1-m2)/s1;eff1 # 0.5292477 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP ~ ano, data = hyper_19to20, method = "wilcox.test") # p = 8.53e-17

# Monthly Comparisons (Number of Patients with Hypertension with BP < 150/90 mmHg) --------------------------------

# Density plots for the number of patients with hypertension with BP < 150/90 mmHg
mu3 <- ddply(hyper_19to20[which(hyper_19to20$ano == "2019"),], "mes", summarise, grp.mean=mean(TotalBP))
den1 <- ggplot(hyper_19to20, aes(x=TotalBP, color = mes)) +
  geom_density(size = 1) +
  geom_vline(data=mu3, aes(xintercept=grp.mean, color = mes), linetype="dashed", size=1) +
  xlab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  ylab("Density") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.position = c(0.67, 0.75),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "Month (2019)", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                         "Aug", "Sep", "Oct", "Nov", "Dec"))

mu4 <- ddply(hyper_19to20[which(hyper_19to20$ano == "2020"),], "mes", summarise, grp.mean=mean(TotalBP))
den2 <- ggplot(hyper_19to20, aes(x=TotalBP, color = mes)) +
  geom_density(size = 1) +
  geom_vline(data=mu4, aes(xintercept=grp.mean, color = mes), linetype="dashed", size=1) +
  xlab("Number of Patients with Hypertension with BP < 150/90 mmHg") +
  ylab("Density") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.position = c(0.67, 0.75),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "Month (2020)", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                         "Aug", "Sep", "Oct", "Nov", "Dec"))
grid.arrange(den1, den2, ncol = 2)

# January 2019 vs January 2020 ------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jan"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jan"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"), 5]) # p = 0.02719
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"), 5]) # p = 0.008385
# Rej H0 -> sample does NOT follow normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Jan"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jan"),])
var3.ftest # p-value = 0.8483

# Non-parametric tests should be considered, since the assumption of normality was Rejected
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.7764 ---> Not Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In January 2020 the pandemic had not started. Result makes sense since primary care was regular

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.043543 ---> small effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jan"),], method="wilcox.test") # p = 0.776

# February 2019 vs February 2020 ----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Feb"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Feb"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"), 5]) # p = 0.0505
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"), 5]) # p = 0.03091
# Rej H0 -> sample does NOT follow normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Feb"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Feb"),])
var3.ftest # p-value = 0.8743
# Non-parametric tests should be considered, since the assumption of normality was Rejected
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.8812 ---> Not Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In February 2020 the pandemic had not started. Result makes sense since primary care was regular

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.02166224 ---> small effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Feb"),], method="wilcox.test") # p = 0.881

# March 2019 vs March 2020 ----------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Mar"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Mar"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 5]) # p = 0.09322
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 5]) # p = 0.03443
# Rej H0 -> sample does NOT follow normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Mar"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Mar"),])
var3.ftest # p-value = 0.2606 (evidence is lower than for the previous months, effect of lockdown?)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.08621 ---> Reject the hypothesis of equality of means at 10% significance level 
# In March 2020 the lockdown started in mid March so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=0.04311 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.2811276 ---> small to medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Mar"),], method="wilcox.test") # p = 0.0862

# April 2019 vs April 2020 ----------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Apr"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Apr"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 5]) # p = 0.1744
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 5]) # p = 0.04121
# Not Rej for 2019 and Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Apr"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Apr"),])
var3.ftest # p-value = 0.03367 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.001354 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In April 2020 there was full lockdown so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=0.000677 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.5689181 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Apr"),], method="wilcox.test") # p = 0.00135

# May 2019 vs May 2020 --------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="May"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="May"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 5]) # p = 0.3552
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 5]) # p = 0.0617
# Not Rej for 2019 and Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "May"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "May"),])
var3.ftest # p-value = 0.0617 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.0001934 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In May 2020 there was full lockdown, so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=9.67e-05 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.6856092 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="May"),], method="wilcox.test") # p = 0.000193

# June 2019 vs June 2020 ------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jun"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jun"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 5]) # p = 0.1806
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 5]) # p = 0.06979
# Not Rej for 2019 and Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Jun"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jun"),])
var3.ftest # p-value = 0.03224 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.0006999 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In June 2020 there was full lockdown, so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=0.0003499 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.6046632 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jun"),], method="wilcox.test") # p = 0.0007

# July 2019 vs July 2020 ------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jul"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jul"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 5]) # p = 0.01089
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 5]) # p = 0.02162
# Reject normality assumption. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Jul"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jul"),])
var3.ftest # p-value = 0.004143 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 4.665e-06 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in July
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=2.333e-06 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.7690859 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jul"),], method="wilcox.test") # p = 4.67e-06

# August 2019 vs August 2020 --------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Aug"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Aug"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 5]) # p = 0.06154
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 5]) # p = 0.01916
# Reject normality assumption. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Aug"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Aug"),])
var3.ftest # p-value = 0.0132 (variance is different which makes sense due to lockdown, although less evidence)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 1.039e-05 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=5.195e-06 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.7592757 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Aug"),], method="wilcox.test") # p = 1.04e-05

# September 2019 vs September 2020 --------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Sep"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Sep"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 5]) # p = 0.06476
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 5]) # p = 0.02048
# Reject normality assumption. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Sep"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Sep"),])
var3.ftest # p-value = 0.00855 (variance is different)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 2.818e-05 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.409e-05 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.7329282 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Sep"),], method="wilcox.test") # p = 2.82e-05

# October 2019 vs October 2020 ------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Oct"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Oct"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 5]) # p = 0.1109
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 5]) # p = 0.02617
# Reject normality assumption. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Oct"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Oct"),])
var3.ftest # p-value = 0.00384 (variance is different)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 2.468e-06 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.234e-06 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.8192892 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Oct"),], method="wilcox.test") # p = 2.47e-06

# November 2019 vs November 2020 ----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Nov"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Nov"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 5]) # p = 0.1485
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 5]) # p = 0.04467
# Reject normality assumption for 2020 but not for 2019. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Nov"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Nov"),])
var3.ftest # p-value = 0.04467 (variance is different)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 4.666e-06 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=2.333e-06 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.8094075 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Nov"),], method="wilcox.test") # p = 4.67e-06

# December 2019 vs December 2020 ----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Dec"), 5], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Dec"), 5], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 5]) # p = 0.1541
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 5]) # p = 0.04413
# Reject normality assumption for 2020 but not for 2019. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(TotalBP ~ hyper_19to20[which(hyper_19to20$mes == "Dec"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Dec"),])
var3.ftest # p-value = 0.00564 (variance is different)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 5],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 5.536e-06 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 5],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 5],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=2.768e-06 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),5])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"),5])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),5])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"),5])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),5])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),5])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.8069443 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(TotalBP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Dec"),], method="wilcox.test") # p = 5.5e-06

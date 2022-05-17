#################################################################################################
###### 08. SCRIPT FOR STATISTICAL ANALYSIS OF HYPERTENSION DATA FROM 2019 TO 2020 UNDER 65 ######
#################################################################################################

# -------------------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Check for statistical differences in 2019 vs 2020
# 3. Patients with Hypertension Aged < 65 with BP < 150/90 mmHg
# Miguel Farinha 
# (MMA BioStatistics May 2021)
# -------------------------------------------------------------

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
summary(hyper_19to20)
hyper_19to20$ano <- year(hyper_19to20$Period)
hyper_19to20$ano <- as.factor(hyper_19to20$ano)
hyper_19to20$mes <- month(hyper_19to20$Period, abbr = TRUE, label = TRUE)
hyper_19to20$mes <- as.factor(hyper_19to20$mes)
summary(hyper_19to20)


# ----------------------------------------------------------------------------------------------------------------
# Statistical Tests ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

require(plyr)

# 2019 VS 2020 (Yearly Comparison) -------------------------------------------------------------------------------

# Density plots for the patients aged < 65 with hypertension with BP < 150/90 mmHg
mu2 <- ddply(hyper_19to20, "ano", summarise, grp.mean=mean(Under65BP))
ggplot(hyper_19to20, aes(x=Under65BP, color = ano)) +
  geom_density(size = 1) +
  geom_vline(data=mu2, aes(xintercept=grp.mean, color = ano), linetype="dashed", size=1) +
  xlab("Patients with Hypertension Aged < 65 with BP < 150/90 mmHg") +
  ylab("Density") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.position = c(0.65, 0.8),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "Year", labels = c("2019", "2020"))


# Check equality of the mean Number of Patients with Hypertension with BP < 150/90 mmHg
# Assumption 1: The measurements from different years are independent
# Assumption 2: Normality ---> Reject
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano == "2019"), c("Under65BP")], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano == "2020"), c("Under65BP")], color = "blue"),
             ncol = 2)
# Shapiro-Wilk test for normality
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019"), c("Under65BP")]) # p = 1.809e-11
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020"), c("Under65BP")]) # p = 6.683e-08
# Thus, we Rej H0 -> not normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Reject
var2.ftest <- var.test(Under65BP ~ hyper_19to20$ano, data = hyper_19to20)
var2.ftest # p-value < 2.2e-16

# Non-Parametric tests
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019"), c("Under65BP")],
            hyper_19to20[which(hyper_19to20$ano == "2020"), c("Under65BP")], alternative = "two.sided",
            paired = FALSE, exact = FALSE, correct = TRUE)
# Reject equality of means hypothesis for the usual significance levels, p < 2.2e-16

# Effect size Hedge's
n1 <- length(hyper_19to20[hyper_19to20[,c("ano")]==2019, c("Under65BP")])
n2 <- length(hyper_19to20[hyper_19to20[,c("ano")]==2020, c("Under65BP")])
m3 <- mean(hyper_19to20[hyper_19to20[,c("ano")]==2019, c("Under65BP")])
m4 <- mean(hyper_19to20[hyper_19to20[,c("ano")]==2020, c("Under65BP")])
sd3 <- sd(hyper_19to20[hyper_19to20[,c("ano")]==2019, c("Under65BP")])
sd4 <- sd(hyper_19to20[hyper_19to20[,c("ano")]==2020, c("Under65BP")])
s2 <- sqrt(((n1-1)*sd3^2+(n2-1)*sd4^2)/(n1+n2-2))
eff2 <- abs(m3-m4)/s2;eff2 # 0.6557687 ---> medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP ~ ano, data = hyper_19to20, method = "wilcox.test") # p = 6.42e-24

# Monthly Comparisons (Number of Patients with Hypertension with BP < 150/90 mmHg) -------------------------------

# Density plots for the number of patients with hypertension with BP < 150/90 mmHg
mu3 <- ddply(hyper_19to20[which(hyper_19to20$ano == "2019"),], "mes", summarise, grp.mean=mean(Under65BP))
den1 <- ggplot(hyper_19to20, aes(x=Under65BP, color = mes)) +
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

mu4 <- ddply(hyper_19to20[which(hyper_19to20$ano == "2020"),], "mes", summarise, grp.mean=mean(Under65BP))
den2 <- ggplot(hyper_19to20, aes(x=Under65BP, color = mes)) +
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

# January 2019 vs January 2020 -----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jan"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jan"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"), 6]) # p = 0.2911
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"), 6]) # p = 0.1956
# Not Rej H0 -> sample follows app. normal distribution. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Jan"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jan"),])
var3.ftest # p-value = 0.6135
# T-test for the equality of the means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"), 6], var.equal = TRUE)
# p = 0.7467 ---> Not Reject H0, so both samples have the same mean value (no COVID in January)
# Non-parametric tests should be considered, since the assumption of normality was Rejected
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.7741 ---> Not Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In January 2020 the pandemic had not started. Result makes sense since primary care was regular

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jan"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jan"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.05971639 ---> small effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jan"),], method="wilcox.test") #p = 0.774

# February 2019 vs February 2020 ---------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Feb"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Feb"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"), 6]) # p = 0.1938
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"), 6]) # p = 0.08652
# Rej 2020 and Not Rej 2019. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Feb"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Feb"),])
var3.ftest # p-value = 0.7587
# Non-parametric tests should be considered, since the assumption of normality was Rejected
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.9285 ---> Not Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In February 2020 the pandemic had not started. Result makes sense since primary care was regular

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Feb"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Feb"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.02360035 ---> small effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Feb"),], method="wilcox.test") #p = 0.929

# March 2019 vs March 2020 ----------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Mar"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Mar"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 6]) # p = 0.1296
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 6]) # p = 0.05227
# Rej 2020 and Not Rej 2019. Therefore, nonparametric tests should be conducted
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Mar"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Mar"),])
var3.ftest # p-value = 0.1364 (evidence is lower than for the previous months, effect of lockdown?)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 0.01522 ---> Reject the hypothesis of equality of means at 5 and 10% significance level 
# In March 2020 the lockdown started in mid March so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=0.007609 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Mar"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Mar"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.4681966 ---> small to medium effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Mar"),], method="wilcox.test") #p = 0.0152

# April 2019 vs April 2020 ----------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Apr"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Apr"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 6]) # p = 0.05402
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 6]) # p = 0.1047
# Rej for 2019 and Not Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject (different variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Apr"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Apr"),])
var3.ftest # p-value = 0.008355 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 8.654e-07 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In April 2020 there was full lockdown so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=4.327e-07 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Apr"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Apr"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 0.9750412 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Apr"),], method="wilcox.test") # p=8.65e-07

# May 2019 vs May 2020 --------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="May"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="May"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 6]) # p = 0.02352
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 6]) # p = 0.3794
# Rej for 2019 and Not Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject (different variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "May"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "May"),])
var3.ftest # p-value = 0.006211 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 3.774e-08 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In May 2020 there was full lockdown, so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.887e-08 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "May"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "May"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.131457 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="May"),], method="wilcox.test") #p = 3.8e-08

# June 2019 vs June 2020 ------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jun"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jun"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 6]) # p = 0.03732
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 6]) # p = 0.4289
# Rej for 2019 and Not Rej for 2020. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Reject at 10% (different variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Jun"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jun"),])
var3.ftest # p-value = 0.07263 (variance is different which makes sense due to lockdown)
# Non-parametric tests should be considered
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 2.183e-07 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# In June 2020 there was full lockdown, so this may have had an effect on primary care
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.091e-07 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jun"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jun"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.059082 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jun"),], method="wilcox.test") #p = 2.18e-07

# July 2019 vs July 2020 ------------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Jul"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Jul"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 6]) # p = 0.1559
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 6]) # p = 0.5957
# Not Reject normality assumption. Perform t-test and non-parametric tests
# Assumption 3: Equality of variance ---> Not Reject
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Jul"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Jul"),])
var3.ftest # p-value = 0.3405 ---> Do not reject equality of variances
# T-test for the equality of the means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 1.058e-11 ---> Reject H0 so that difference in means in different from 0
# Non-parametric
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 1.358e-10 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in July
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=6.79e-11 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Jul"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Jul"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.366337 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Jul"),], method="wilcox.test") # p = 1.36e-10

# August 2019 vs August 2020 --------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Aug"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Aug"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 6]) # p = 0.08511
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 6]) # p = 0.4533
# Rej at 10% for 2019 and Not Rej 2020. Perform t-test as well as non-parametric test
# Assumption 3: Equality of variance
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Aug"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Aug"),])
var3.ftest # p-value = 0.5732 ---> Not Rej the hypotheses of equal variances
# t-test for equality of means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 1.174e-10 ---> Rej the equality of means at the usual significance levels
# Non-parametric
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 1.117-09 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=5.585e-10 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Aug"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Aug"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.310809 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Aug"),], method="wilcox.test") # p = 1.12e-09

# September 2019 vs September 2020 --------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Sep"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Sep"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 6]) # p = 0.08054
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 6]) # p = 0.653
# Rej at 10% for 2019 and Not Rej 2020. Perform t-test and non-parametric test
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Sep"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Sep"),])
var3.ftest # p-value = 0.3591 ---> Not Rej hypothesis of different variances
# t-test for equality of means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 1.148e-10 ---> Rej the equality of means at the usual significance levels
# Non-parametric test
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 2.613e-09 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.306e-09 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Sep"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Sep"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.283404 ---> large effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Sep"),], method="wilcox.test") # p = 2.61e-09

# October 2019 vs October 2020 ------------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Oct"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Oct"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 6]) # p = 0.05816
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 6]) # p = 0.788
# Rej at 10% 2019 and Not Rej 2020. Perform t-test and non-parametric test
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Oct"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Oct"),])
var3.ftest # p-value = 0.1552 ---> Not Rej the hypothesis of equal variances
# t-test for equality of means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 1.509e-12 ---> Rej the equality of means at the usual significance levels
# Non-parametric test
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 7.81e-11 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=3.905e-11 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Oct"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Oct"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.397011 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Oct"),], method="wilcox.test") # p = 7.81e-11

# November 2019 vs November 2020 ----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Nov"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Nov"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 6]) # p = 0.03795
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 6]) # p = 0.9794
# Not Reject normality assumption for 2020 but Rej for 2019. Non-parametric tests may be more appropriate
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Nov"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Nov"),])
var3.ftest # p-value = 0.1297 ---> Not Rej hypothesis of equal variances
# t-test for equality of means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 3.903e-12 ---> Rej the equality of means at the usual significance levels
# Non-parametric test
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 3.451e-10 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.725e-10 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Nov"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Nov"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.357293 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Nov"),], method="wilcox.test") # p = 3.45e-10

# December 2019 vs December 2020 ----------------------------------------------------------------------------------

# Assumption 1: The measurements from different months are independent
# Assumption 2: QQ Plots + Shapiro-Wilk test for normality
grid.arrange(ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2019" & hyper_19to20$mes=="Dec"), 6], color = "blue"),
             ggqqplot(hyper_19to20[which(hyper_19to20$ano=="2020" & hyper_19to20$mes=="Dec"), 6], color = "blue"),
             ncol=2)
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 6]) # p = 0.04849
shapiro.test(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 6]) # p = 0.9757
# Rej Ho for 2019 and Not Rej Ho for 2020. Perform t-test and non-parametric test
# Assumption 3: Equality of variance ---> Not Reject (equal variances)
var3.ftest <- var.test(Under65BP ~ hyper_19to20[which(hyper_19to20$mes == "Dec"),]$ano,
                       data = hyper_19to20[which(hyper_19to20$mes == "Dec"),])
var3.ftest # p-value = 0.1402 ---> Not Rej hypothesis of equal variances
# t-test for equality of means
t.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 6],
       hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 6],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)
# p = 4.341e-12 ---> Rej the equality of means at the usual significance levels
# Non-parametric test
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 6],
            alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE)
# p = 3.451e-10 ---> Reject the hypothesis of equality of means at 1, 5, 10% significance levels
# Despite some "normality" the monitoring of patients with hypertension did not return to normal values in August
wilcox.test(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"), 6],
            hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"), 6],
            alternative = "greater", paired = FALSE, exact = FALSE, correct = TRUE) # p=1.725e-10 -> mu(19)>mu(20)

# Effect size Hedge's
n1 <- length(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),6])
n2 <- length(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"),6])
m5 <- mean(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),6])
m6 <- mean(hyper_19to20[which(hyper_19to20$ano == "2020" & hyper_19to20$mes == "Dec"),6])
sd5 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),6])
sd6 <- sd(hyper_19to20[which(hyper_19to20$ano == "2019" & hyper_19to20$mes == "Dec"),6])
s3 <- sqrt(((n1-1)*sd5^2+(n2-1)*sd6^2)/(n1+n2-2))
eff3 <- abs(m5-m6)/s3;eff3 # 1.356645 ---> significant effect

# Alternative function to test the hypothesis of equality of means
compare_means(Under65BP~ano, data=hyper_19to20[which(hyper_19to20$mes=="Dec"),], method="wilcox.test") # p = 3.45e-10

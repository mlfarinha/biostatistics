#####################################################
###### 10. SCRIPT TO EXPLORE HYPERTENSION DATA ######
#####################################################

# ----------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Differences between years
# 3. Detrending data
# 4. Comparison of lockdown months between years
# 5. Comparison of ARS in April 2020 and February 2021
#
# Miguel Farinha 
# (MMA BioStatistics June 2021)
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
library(pracma)
library(zoo)
library(mgcv)
library(mgcViz)
library(nlme)
library(lmtest)

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

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
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("RegiÃ£o", "Região", x)
}))
hyper <- data.frame(lapply(hyper, function(x) {
  gsub("SaÃºde", "Saúde", x)
}))
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
hyper$Year <- factor(hyper$Year)

# Consider only period from 2014 - 2020
hyper <- hyper[-which(hyper$Year == "2021"),]

# Differences between years ---------------------------------------------------------------------------------------

# One-Way ANOVA
anova_year <- aov(BP ~ Year, data = hyper)
summary(anova_year) # p < 2e-16 --> Reject H0 of equality of means
# anova diagnostic plots: plot(anova_year,cex.lab=0.8,cex=0.8)
# Check assumptions:
# 1. Normality
plot(anova_year, 2) # cannot assume normality
aov_residuals <- residuals(object = anova_year)
shapiro.test(x = aov_residuals ) # p < 2.2e-16 -> Reject normality
shapiro.test(hyper[, c("BP")]) # p < 2.2e-16 ---> Reject normality assumption
# 2. Homogeneity of variances
tbartlett <- bartlett.test(hyper$BP ~ hyper$Year, data = hyper); tbartlett # p<2.2e-16 -> Rej equality of variance
tlevene <- leveneTest(hyper$BP ~ hyper$Year, data = hyper); tlevene # p<2.865e-14 -> Reject equality of variance

# Welch One-Way Test (No assumption of equal variances)
oneway.test(BP ~ Year, data = hyper)
pairwise.t.test(hyper$BP, hyper$Year, p.adj = "bonf", pool.sd = FALSE) 
# We obtin significant differences between the year 2020 and all other years
# The other years are not significantly different between themselves

# Kruskal-Wallis test
kruskal_year <- kruskal.test(hyper$BP ~ hyper$Year, data = hyper)
kruskal_year # p < 2.2e-16 --> Reject H0 (there are significant differences between years)
pairwise.wilcox.test(hyper$BP, hyper$Year, p.adjust.method = "bonf", alternative = "less")
hyper$comp <- 0
hyper[which(hyper$Year == "2020"), ]$comp <- 1
wilcox.test(BP ~ comp, data = hyper, p.adjust.methods = "bonf", alternative = "greater") # W =1608522
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2014" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # p = 1.444e-15
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2015" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # 3.223e-13
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2016" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # 4.428e-11
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2017" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # 8.534e-13
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2018" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # p = 8.398e-15
wilcox.test(BP ~ Year, data = hyper[which(hyper$Year == "2019" | hyper$Year == "2020"),],
            p.adjust.method = "bonf", alternative = "greater") # p < 2.2e-16


# Games-Howell test (source: https://rpubs.com/aaronsc32/games-howell-test)
games.howell <- function(grp, obs) {
  #Create combinations
  combs <- combn(unique(grp), 2)
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) +
                                                           (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 7)
  
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit','lower limit')
  
  return(results)
}

games.howell(hyper$Year, hyper$BP)

# Boxplot with data for each year
ggplot(data=hyper, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Monthly mean number of patients with condition") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000,12500,15000,17500,20000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 19500, label.x = 3.2, aes(group = Year), size = 4.5) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "2020", size = 4.5, label.y = 18500)

###################################################################################################################

# Detrend data ----------------------------------------------------------------------------------------------------
BP <- hyper[['BP']]
class(BP)
BP.dt <- detrend(BP, "linear") # detrended data
df.dt <- as.data.frame(BP.dt)
colnames(df.dt)[1] <- "det"
df.dt$year <- hyper$Year

# One-Way ANOVA
anova_det <- aov(det ~ year, data = df.dt)
summary(anova_det) # p < 2e-16 --> Reject H0 of equality of means
# anova diagnostic plots: plot(anova_year,cex.lab=0.8,cex=0.8)
# Check assumptions:
# 1. Normality
plot(anova_det, 2) # cannot assume normality
aov_residuals <- residuals(object = anova_det)
shapiro.test(x = aov_residuals ) # p < 2.2e-16 ---> Reject normality
shapiro.test(df.dt[, c("det")]) # p < 2.2e-16 ---> Reject normality assumption
# 2. Homogeneity of variances
tbartlett <- bartlett.test(df.dt$det ~ df.dt$year, data = df.dt); tbartlett # p<2.2e-16 -> Rej equality of variance
tlevene <- leveneTest(df.dt$det ~ df.dt$year, data = df.dt); tlevene # p<2.839e-14 -> Reject equality of variance

# Welch One-Way Test (No assumption of equal variances)
oneway.test(det ~ year, data = df.dt) # p < 2.2e-16
pairwise.t.test(df.dt$det, df.dt$year, p.adj = "bonf", pool.sd = FALSE) 
# Same results for detrended data

# Kruskal-Wallis test
kruskal_det <- kruskal.test(df.dt$det ~ df.dt$year, data = df.dt)
kruskal_det # p < 2.2e-16 --> Reject H0 (there are significant differences between years)
pairwise.wilcox.test(df.dt$det, df.dt$year, p.adjust.method = "bonf")

games.howell(df.dt$year, df.dt$det)

det_y <- 
  df.dt %>%
  group_by(year) %>% 
  summarise(meandt = mean(det, na.rm = TRUE),
            sddt = sd(det, na.rm = TRUE))
colnames(det_y)[1] <- "Year"

ggplot(data=reshape2::melt(df.dt)) +
  geom_point(data = det_y, mapping = aes(x = Year, y = meandt, color = Year), size = 2) +
  ylab("Detrended mean number of patients with condition") +
  xlab("Year") +
  scale_y_continuous(breaks = c(-1100,-900,-700,-500,-300,-100,0,100,300,500)) + 
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 550, label.x = 3.2,
                     aes(x = year, y = BP, group = year), size = 4.5) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", aes(x = year, y = BP, group = year), 
                     ref.group = "2020", size = 5, label.y = 430)

###################################################################################################################

# LOCKDOWN COMPARISON ---------------------------------------------------------------------------------------------
work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

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
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
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
hyper$Year <- factor(hyper$Year)

# Consider only period from 2014 - 2020
hyper <- hyper[-which(hyper$Year == "2021"),]

# Lockdown months (18 March to 02 May -> consider only April since data is collected monthly)
hyper_lock <- hyper %>%
  filter((month(hyper$Period) == 4))
hyper_lock$Month <- month(hyper_lock$Period, abbr = TRUE, label = TRUE)
hyper_lock <- hyper_lock[,c("Period", "Region", "BP", "Year", "Month")]
# unbalanced design
length(which(hyper_lock$Region == "Alentejo")) # 28
length(which(hyper_lock$Region == "Algarve")) # 21
length(which(hyper_lock$Region == "Centro")) # 63
length(which(hyper_lock$Region == "LVT")) # 105
length(which(hyper_lock$Region == "Norte")) # 168


# Differences between years in April -----------------------------------------------------------------------------

# One-Way ANOVA
anova_lock <- aov(BP ~ Year, data = hyper_lock)
summary(anova_lock) # p = 0.0324 (Year)  --> Reject H0 of equality of means for both factors
# no interaction between year and region (deleted this)
# anova diagnostic plots: plot(anova_lock,cex.lab=0.8,cex=0.8)
# Check assumptions:
# 1. Normality
plot(anova_lock, 2) # approximately normal (except tails)
aov_residuals <- residuals(object = anova_lock)
shapiro.test(x = aov_residuals ) # p = 6.58e-07 -> Reject normality
shapiro.test(hyper_lock[, c("BP")]) # p = 2.207e-07 ---> Reject normality assumption
# 2. Homogeneity of variances
plot(anova_lock, 1) # imbalanced measures per region account for a lot of variability
tbartlett <- bartlett.test(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock); tbartlett # p = 0.3365
tlevene <- leveneTest(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock); tlevene # p = 0.5169 -> Equal variances

# Tukey and t-test multiple pairwise-comparisons
TukeyHSD(anova_lock) # No differences between years (2014-2019) except 2020
pairwise.t.test(hyper_lock$BP, hyper_lock$Year,p.adjust.method="bonf",pool.sd=F) # 2020 different from all yrs

# Anova for unbalanced designs
#anova_lock_unbal <- aov(BP ~ Year + Region, data = hyper_lock)
#Anova(anova_lock_unbal, type = "II") # similar results
 
# Kruskal-Wallis test for Year and Region
kruskal_lock1 <- kruskal.test(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock)
kruskal_lock1 # p =0.01545 --> Reject H0 (there are significant differences between years)
pairwise.wilcox.test(hyper_lock$BP, hyper_lock$Year, p.adjust.method = "bonf") # 2020 significantly different 

# Games-Howell test (source: https://rpubs.com/aaronsc32/games-howell-test)
games.howell <- function(grp, obs) {
  #Create combinations
  combs <- combn(unique(grp), 2)
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) +
                                                           (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 4)
  
  # Rename data frame columns
  colnames(results) <- c('groups','Mean Difference','Standard Error','t','df','p','upper limit','lower limit')
  
  return(results)
}

games.howell(hyper_lock$Year, hyper_lock$BP) # similar results


# Boxplot with data for each year
ggplot(data=hyper_lock, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in April") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 14500, label.x = 3.2, aes(group = Year), size = 4.5)
  #stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "2020", size = 4.5, label.y = 14200)

# Boxplot with data for each region
ggplot(data=hyper_lock, aes(x = Region, y = BP, group = Region)) +
  geom_boxplot(aes(fill = Region), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Patients with HT < 65yrs, BP < 150/90 mmHg") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_x_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")) +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")) +
  stat_compare_means(method = "kruskal.test", label.y = 15000, label.x = 2.5, aes(group = Region), size = 4.5) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Região de Saúde LVT",
                     size = 4.5, label.y = 14200)

###################################################################################################################

# LOCKDOWN COMPARISON FOR FEBRUARY --------------------------------------------------------------------------------
work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

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
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
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
hyper$Year <- factor(hyper$Year)

# Consider only period from 2014 - 2020
#hyper <- hyper[-which(hyper$Year == "2021"),]

# Lockdown months February 2021
hyper_lock <- hyper %>%
  filter((month(hyper$Period) == 2))
hyper_lock$Month <- month(hyper_lock$Period, abbr = TRUE, label = TRUE)
hyper_lock <- hyper_lock[,c("Period", "Region", "BP", "Year", "Month")]
# unbalanced design
length(which(hyper_lock$Region == "Alentejo")) # 32
length(which(hyper_lock$Region == "Algarve")) # 24
length(which(hyper_lock$Region == "Centro")) # 72
length(which(hyper_lock$Region == "LVT")) # 120
length(which(hyper_lock$Region == "Norte")) # 192


# Differences between years in February -----------------------------------------------------------------------------

# One-Way ANOVA
anova_lock <- aov(BP ~ Year, data = hyper_lock)
summary(anova_lock) # p = 2.26e-08 *** (Year)  --> Reject H0 of equality of means for both factors
# no interaction between year and region (deleted this)
# anova diagnostic plots: plot(anova_lock,cex.lab=0.8,cex=0.8)
# Check assumptions:
# 1. Normality
plot(anova_lock, 2) # approximately normal (except tails)
aov_residuals <- residuals(object = anova_lock)
shapiro.test(x = aov_residuals ) # p = 4.103e-09 -> Reject normality
shapiro.test(hyper_lock[, c("BP")]) # p = 2.207e-07 ---> Reject normality assumption
# 2. Homogeneity of variances
plot(anova_lock, 1) # imbalanced measures per region account for a lot of variability
tbartlett <- bartlett.test(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock); tbartlett # p = 0.3365
tlevene <- leveneTest(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock); tlevene # p = 0.5169 -> Equal variances

# Tukey and t-test multiple pairwise-comparisons
TukeyHSD(anova_lock) # No differences between years (2014-2020) except 2021
pairwise.t.test(hyper_lock$BP, hyper_lock$Year,p.adjust.method="bonf",pool.sd=F) # 2021 different from all yrs

# Anova for unbalanced designs
#anova_lock_unbal <- aov(BP ~ Year + Region, data = hyper_lock)
#Anova(anova_lock_unbal, type = "II") # similar results

# Kruskal-Wallis test for Year and Region
kruskal_lock1 <- kruskal.test(hyper_lock$BP ~ hyper_lock$Year, data = hyper_lock)
kruskal_lock1 # p = 6.966e-10 --> Reject H0 (there are significant differences between years)
pairwise.wilcox.test(hyper_lock$BP, hyper_lock$Year, p.adjust.method = "bonf") # 2021 significantly different 

# Games-Howell test (source: https://rpubs.com/aaronsc32/games-howell-test)
games.howell <- function(grp, obs) {
  #Create combinations
  combs <- combn(unique(grp), 2)
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) +
                                                           (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 4)
  
  # Rename data frame columns
  colnames(results) <- c('groups','Mean Difference','Standard Error','t','df','p','upper limit','lower limit')
  
  return(results)
}

games.howell(hyper_lock$Year, hyper_lock$BP) # similar results

# Boxplot with data for each year
ggplot(data=hyper_lock, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in February") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) +
  stat_compare_means(method = "kruskal.test", label.y = 8700, label.x = 3.2, aes(group = Year), size = 4.5)

##################################################################################################################

work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

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
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
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
hyper$Year <- factor(hyper$Year)

# Consider only period from 2014 - 2020
hyper <- hyper[-which(hyper$Year == "2021"),]

# Lockdown months (18 March to 02 May -> consider only April since data is collected monthly)
hyper_lock <- hyper %>%
  filter((month(hyper$Period) == 4))
hyper_lock$Month <- month(hyper_lock$Period, abbr = TRUE, label = TRUE)
hyper_lock <- hyper_lock[,c("Period", "Region", "BP", "Year", "Month")]

# Comparison between APRIL for the same ARS ----------------------------------------------------------------------

lock_alentejo <- hyper_lock[which(hyper_lock$Region == "Alentejo"), ]
lock_algarve <- hyper_lock[which(hyper_lock$Region == "Algarve"), ]
lock_centro <- hyper_lock[which(hyper_lock$Region == "Centro"), ]
lock_lvt <- hyper_lock[which(hyper_lock$Region == "LVT"), ]
lock_norte <- hyper_lock[which(hyper_lock$Region == "Norte"), ]

# One-Way ANOVA test
anova_alentejo <- aov(BP ~ Year, data = lock_alentejo); summary(anova_alentejo) # p = 0.721 -> not rej H0
anova_algarve <- aov(BP ~ Year, data = lock_algarve); summary(anova_algarve) # p = 0.938 -> not rej H0
anova_centro <- aov(BP ~ Year, data = lock_centro); summary(anova_centro) # p = 0.992 -> not rej H0
anova_lvt <- aov(BP ~ Year, data = lock_lvt); summary(anova_lvt) # p = 0.453 -> not rej H0
anova_norte <- aov(BP ~ Year, data = lock_norte); summary(anova_norte) # p = 0.027 -> rej H0 at 5% and 10%
# Check assumptions:
# 1. Normality
plot(anova_alentejo, 2) # approximately normal (except tails)
plot(anova_algarve, 2)
plot(anova_centro, 2)
plot(anova_lvt, 2)
plot(anova_norte, 2)
aov_residuals1 <- residuals(object = anova_alentejo); shapiro.test(x = aov_residuals1) # p = 0.2416 -> not rej norm
aov_residuals2 <- residuals(object = anova_algarve); shapiro.test(x = aov_residuals2) # p = 0.004121 -> rej norm
aov_residuals3 <- residuals(object = anova_centro); shapiro.test(x = aov_residuals3) # p = 0.0001628 -> rej norm
aov_residuals4 <- residuals(object = anova_lvt); shapiro.test(x = aov_residuals4) # p = 2.02e-06 -> rej norm
aov_residuals5 <- residuals(object = anova_norte); shapiro.test(x = aov_residuals5) # p = 1.959e-07 -> rej norm
# 2. Homogeneity of variances
plot(anova_alentejo, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.9714 -> equality of variances
leveneTest(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.9519 -> equality of variances
plot(anova_algarve, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.9583 -> equality of variances
leveneTest(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.9913 -> equality of variances
plot(anova_centro, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.9879 -> equality of variances
leveneTest(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.9839 -> equality of variances
plot(anova_lvt, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 0.9647 -> equality of variances
leveneTest(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 0.9963 -> equality of variances
plot(anova_norte, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 0.7883 -> equality of variances
leveneTest(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 0.8159 -> equality of variances

# Tukey pairwise-comparisons
TukeyHSD(anova_alentejo) # No differences between years
TukeyHSD(anova_algarve) # No differences between years
TukeyHSD(anova_centro) # No differences between years
TukeyHSD(anova_lvt) # No differences between years
TukeyHSD(anova_norte) # significant differences between 2020 and previous years

# Anova for unbalanced designs
anova_alentejo_unbal <- aov(BP ~ Year, data = lock_alentejo); Anova(anova_alentejo_unbal, type = "III") # 0.7206
anova_algarve_unbal <- aov(BP ~ Year, data = lock_algarve); Anova(anova_algarve_unbal, type = "III") # 0.9385
anova_centro_unbal <- aov(BP ~ Year, data = lock_centro); Anova(anova_centro_unbal, type = "III") # 0.9915
anova_lvt_unbal <- aov(BP ~ Year, data = lock_lvt); Anova(anova_lvt_unbal, type = "III") # 0.4529
anova_norte_unbal <- aov(BP ~ Year, data = lock_norte); Anova(anova_norte_unbal, type = "III") # 0.027

# Kruskal-Wallis test for Year and Region
kruskal.test(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.686 -> not rej H0
kruskal.test(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.8574 -> not rej H0
kruskal.test(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.9865 -> not rej H0
kruskal.test(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 0.2729 -> not rej H0
kruskal.test(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 0.017 -> rej H0
pairwise.wilcox.test(lock_norte$BP,lock_norte$Year, p.adjust.method = "bonf")

# Games-Howell test (source: https://rpubs.com/aaronsc32/games-howell-test)
games.howell <- function(grp, obs) {
  #Create combinations
  combs <- combn(unique(grp), 2)
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) +
                                                           (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
  
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  
  return(results)
}

games.howell(lock_alentejo$Year, lock_alentejo$BP) # similar results
games.howell(lock_algarve$Year, lock_algarve$BP) # similar results
games.howell(lock_centro$Year, lock_centro$BP) # similar results
games.howell(lock_lvt$Year, lock_lvt$BP) # similar results
games.howell(lock_norte$Year, lock_norte$BP) # similar results

ggplot(data=lock_alentejo, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Alentejo") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,1500,3000,4500,6000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 7000, label.x = 3.2, aes(group = Year), size = 4.5)
  #stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "2020", size = 4.5, label.y = 6500)

ggplot(data=lock_algarve, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Algarve") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1500,3000,4500,6000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 5300, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_centro, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Centro") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1000,3000,5000,7000,9000,1112000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 12700, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_lvt, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS LVT") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1000,3000,5000,7000,9000,1112000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 10800, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_norte, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Norte") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 14500, label.x = 3.2, aes(group = Year), size = 4.5)

####################################################################################################################

# FEBRUARY SAME ARS -----------------------------------------------------------------------------------------------
work_dir <- "D:/IST/5º Ano/2º Semestre/BioStat/Project/data"
setwd(work_dir)
Sys.setlocale("LC_TIME", "C")

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
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")

# Correct the unrecognized characters
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
hyper$Year <- factor(hyper$Year)

# Consider only period from 2014 - 2020
#hyper <- hyper[-which(hyper$Year == "2021"),]

# Lockdown months February
hyper_lock <- hyper %>%
  filter((month(hyper$Period) == 2))
hyper_lock$Month <- month(hyper_lock$Period, abbr = TRUE, label = TRUE)
hyper_lock <- hyper_lock[,c("Period", "Region", "BP", "Year", "Month")]

# Comparison between APRIL for the same ARS ----------------------------------------------------------------------

lock_alentejo <- hyper_lock[which(hyper_lock$Region == "Alentejo"), ]
lock_algarve <- hyper_lock[which(hyper_lock$Region == "Algarve"), ]
lock_centro <- hyper_lock[which(hyper_lock$Region == "Centro"), ]
lock_lvt <- hyper_lock[which(hyper_lock$Region == "LVT"), ]
lock_norte <- hyper_lock[which(hyper_lock$Region == "Norte"), ]

# One-Way ANOVA test
anova_alentejo <- aov(BP ~ Year, data = lock_alentejo); summary(anova_alentejo) # p = 0.135 -> not rej H0
anova_algarve <- aov(BP ~ Year, data = lock_algarve); summary(anova_algarve) # p = 0.903 -> not rej H0
anova_centro <- aov(BP ~ Year, data = lock_centro); summary(anova_centro) # p = 0.855 -> not rej H0
anova_lvt <- aov(BP ~ Year, data = lock_lvt); summary(anova_lvt) # p = 1.69e-05 -> not rej H0
anova_norte <- aov(BP ~ Year, data = lock_norte); summary(anova_norte) # p = 5.16e-05 -> rej H0 at 5% and 10%
# Check assumptions:
# 1. Normality
plot(anova_alentejo, 2) # approximately normal (except tails)
plot(anova_algarve, 2)
plot(anova_centro, 2)
plot(anova_lvt, 2)
plot(anova_norte, 2)
aov_residuals1 <- residuals(object = anova_alentejo); shapiro.test(x = aov_residuals1) # p = 0.4861 -> not rej norm
aov_residuals2 <- residuals(object = anova_algarve); shapiro.test(x = aov_residuals2) # p = 0.005269 -> rej norm
aov_residuals3 <- residuals(object = anova_centro); shapiro.test(x = aov_residuals3) # p = 0.000142 -> rej norm
aov_residuals4 <- residuals(object = anova_lvt); shapiro.test(x = aov_residuals4) # p = 5.722e-06 -> rej norm
aov_residuals5 <- residuals(object = anova_norte); shapiro.test(x = aov_residuals5) # p = 1.502e-09 -> rej norm
# 2. Homogeneity of variances
plot(anova_alentejo, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.8197 -> equality of variances
leveneTest(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.5459 -> equality of variances
plot(anova_algarve, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.9698 -> equality of variances
leveneTest(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.9931 -> equality of variances
plot(anova_centro, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.9283 -> equality of variances
leveneTest(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.9373 -> equality of variances
plot(anova_lvt, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 0.7432 -> equality of variances
leveneTest(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 0.9645 -> equality of variances
plot(anova_norte, 1) # imbalanced measures per region account for a lot of variability
bartlett.test(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 0.5322 -> equality of variances
leveneTest(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 0.677 -> equality of variances

# Tukey pairwise-comparisons
TukeyHSD(anova_alentejo) # No differences between years
TukeyHSD(anova_algarve) # No differences between years
TukeyHSD(anova_centro) # No differences between years
TukeyHSD(anova_lvt) # No differences between years
TukeyHSD(anova_norte) # significant differences between 2020 and previous years

# Anova for unbalanced designs
anova_alentejo_unbal <- aov(BP ~ Year, data = lock_alentejo); Anova(anova_alentejo_unbal, type = "III")
anova_algarve_unbal <- aov(BP ~ Year, data = lock_algarve); Anova(anova_algarve_unbal, type = "III") 
anova_centro_unbal <- aov(BP ~ Year, data = lock_centro); Anova(anova_centro_unbal, type = "III") 
anova_lvt_unbal <- aov(BP ~ Year, data = lock_lvt); Anova(anova_lvt_unbal, type = "III") 
anova_norte_unbal <- aov(BP ~ Year, data = lock_norte); Anova(anova_norte_unbal, type = "III") 

# Kruskal-Wallis test for Year and Region
kruskal.test(lock_alentejo$BP ~ lock_alentejo$Year, data = lock_alentejo) # p = 0.1718 -> not rej H0
kruskal.test(lock_algarve$BP ~ lock_algarve$Year, data = lock_algarve) # p = 0.8698 -> not rej H0
kruskal.test(lock_centro$BP ~ lock_centro$Year, data = lock_centro) # p = 0.8228 -> not rej H0
kruskal.test(lock_lvt$BP ~ lock_lvt$Year, data = lock_lvt) # p = 5.685e-05 -> not rej H0
kruskal.test(lock_norte$BP ~ lock_norte$Year, data = lock_norte) # p = 5.757e-06 -> rej H0
pairwise.wilcox.test(lock_norte$BP,lock_norte$Year, p.adjust.method = "bonf")

# Games-Howell test (source: https://rpubs.com/aaronsc32/games-howell-test)
games.howell <- function(grp, obs) {
  #Create combinations
  combs <- combn(unique(grp), 2)
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) +
                                                           (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 6)
  
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  
  return(results)
}

games.howell(lock_alentejo$Year, lock_alentejo$BP) # similar results
games.howell(lock_algarve$Year, lock_algarve$BP) # similar results
games.howell(lock_centro$Year, lock_centro$BP) # similar results
games.howell(lock_lvt$Year, lock_lvt$BP) # similar results
games.howell(lock_norte$Year, lock_norte$BP) # similar results

ggplot(data=lock_alentejo, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Alentejo") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,1500,3000,4500,6000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 7000, label.x = 3.2, aes(group = Year), size = 4.5)
#stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "2020", size = 4.5, label.y = 6500)

ggplot(data=lock_algarve, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Algarve") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1500,3000,4500,6000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 5300, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_centro, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Centro") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1000,3000,5000,7000,9000,1112000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 12700, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_lvt, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS LVT") +
  xlab("Year") +
  #scale_y_continuous(breaks = c(0,1000,3000,5000,7000,9000,1112000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 10800, label.x = 3.2, aes(group = Year), size = 4.5)

ggplot(data=lock_norte, aes(x = Year, y = BP, group = Year)) +
  geom_boxplot(aes(fill = Year), alpha = 0.5, size = 0.8, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.6, notch = FALSE, width = 0.8) +
  ylab("Mean number of patients with condition in ARS Norte") +
  xlab("Year") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000)) + 
  #geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(fill = "Year") +
  scale_fill_discrete(labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  stat_compare_means(method = "kruskal.test", label.y = 14500, label.x = 3.2, aes(group = Year), size = 4.5)

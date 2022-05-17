####################################################################
###### 04. SCRIPT TO ANALYSE RELATIONSHIPS BETWEEN COVID DATA ######
####################################################################

# ---------------------------------------------------
# 1. Import the necessary data (covid_main.csv)
# 2. Explore the relationships between variables
# 3. Boxplots and Correaltions plots of the variables
#
# Miguel Farinha (MMA BioStatistics May 2021)
# ---------------------------------------------------

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
covid_main <- read.table("covid_main.csv", sep = ",", header = TRUE)
head(covid_main)
colnames(covid_main)

# Set the datatype Date for the variable data
covid_main$data <- as.Date(covid_main$data, format="%d-%m-%Y", tz="GMT")
covid_main$confirmados_novos <- as.numeric((covid_main$confirmados_novos))
head(covid_main$data)
class(covid_main$data) # class: "Date"
covid_main <- covid_main[-which(covid_main$data > "2021-05-31"),]

# 1. Boxplot for the variable: confirmados_novos
ggplot(covid_main, aes(x = "", y = confirmados_novos)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Daily Number of New Confirmed Cases") +
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000,12500,15000,175000)) + 
  #scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9)

# Outliers confirmados_novos
summary(covid_main$confirmados_novos)
is_over_novos <- covid_main$confirmados_novos > 2418 + 1.5 * (2418 - 286)
head(is_over_novos)
sum(is_over_novos) # 47
covid_main[is_over_novos,]
which_over_novos <- which(is_over_novos)
covid_main[which_over_novos,]
which_over_novos

# 2. Boxplot for the variable: internados_uci
ggplot(covid_main, aes(x = "", y = internados_uci)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Daily Number of Patients in ICU") +
  scale_y_continuous(breaks = c(0,200,400,600,800,1000,1200)) + 
  #scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9)

# 3. Boxplot for the variable: internados_enfermaria
ggplot(covid_main, aes(x = "", y = internados_enfermaria)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Daily Number of Patients in Nursery") +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000)) + 
  #scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9)

# Outliers internados_enfermaria
summary(covid_main$internados_enfermaria)
is_over_enfermaria <- covid_main$internados_enfermaria > 2083 + 1.5 * (2083 - 350.2)
head(is_over_enfermaria)
sum(is_over_enfermaria) # 23
covid_main[is_over_enfermaria,]
which_over_enfermaria <- which(is_over_enfermaria)
covid_main[which_over_enfermaria,]
which_over_enfermaria

# 4. Boxplot of the variable: confirmados_novos vs ARS
quality_confirmed <- function(df) {
  df$confirmados_arsnorte <- as.numeric(df$confirmados_arsnorte)
  df$confirmados_arscentro <- as.numeric(df$confirmados_arscentro)
  df$confirmados_arslvt <- as.numeric(df$confirmados_arslvt)
  df$confirmados_arsalentejo <- as.numeric(df$confirmados_arsalentejo)
  df$confirmados_arsalgarve <- as.numeric(df$confirmados_arsalgarve)
}
quality_confirmed(covid_main)

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

covid_ars <- covid_main[,c("data", "confirmados_novos_arsnorte", "confirmados_novos_arscentro",
                           "confirmados_novos_arslvt", "confirmados_novos_arsalentejo",
                           "confirmados_novos_arsalgarve")]
covid_ars2 <- covid_ars %>%
  gather(ars, novos, -data)

ggplot(covid_ars2, aes(x = ars, y = novos)) +
  geom_boxplot(aes(fill=ars), alpha= 0.4, size = 0.75, outlier.colour = "red", outlier.shape = 16,
               outlier.size = 0.8, outlier.alpha = 0.8, notch = FALSE, width = 0.85 ) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  # legend.position = c(0.9, 0.85)
  ylab("Daily number of new confirmed cases") +
  xlab("ARS") + 
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000)) +
  scale_x_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")) +
  #scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.3) +
  labs(fill = "ARS") +
  scale_fill_discrete(labels = c("ARS Alentejo", "ARS Algarve", "ARS Centro", "ARS LVT", "ARS Norte")) +
  stat_compare_means(method = "kruskal.test", label.y = 9200, label.x = 2.5, aes(group = ars), size = 4.5) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "confirmados_novos_arslvt",
                     size = 4.5, label.y = 8600)

ggline(covid_ars2, x = "ars", y = "novos", add = "mean_se", ylab = "Daily New Confirmed Cases", xlab = "ARS") +
  scale_x_discrete(labels = c("ARS Norte", "ARS Centro", "ARS LVT", "ARS Alentejo", "ARS Algarve"))
plotmeans(covid_ars2$novos~covid_ars2$ars, xlab="ARS", ylab="Daily New Confirmed Cases", 
          main="Mean Plot\nwith 95% CI", cex.lab=0.8, cex.main=0.8) # there is a significant difference between means
# One-Way ANOVA
anova_ars <- aov(novos ~ ars, data = covid_ars2)
summary(anova_ars) # p < 2e-16 --> Rej H0 of equality of means
plot(anova_ars,cex.lab=0.8,cex=0.8)
TukeyHSD(anova_ars) # multiple pairwise comparison between the means of groups with Tukey Honest Significant Differences
pairwise.t.test(covid_ars2$novos, covid_ars2$ars, p.adj = "bonf")
# Check assumptions: Normality and Homogeneity of variances
plot(anova_ars, 1)
tbartett <- bartlett.test(covid_ars2$novos ~ covid_ars2$ars, data = covid_ars2);tbartett # p < 2e-16 --> Rej H0
tlevene <- leveneTest(covid_ars2$novos ~ as.factor(covid_ars2$ars), data = covid_ars2);tlevene # p < 2e-16 --> Rej H0
tfligner <- fligner.test(covid_ars2$novos ~ covid_ars2$ars, data = covid_ars2);tfligner # p < 2e-16 --> Rej H0
trob <- hov(covid_ars2$novos ~ covid_ars2$ars, data = covid_ars2);trob # p < 2e-16 --> Rej H0
plot(anova_ars, 2) # Cannot assume normality
# Welch One-Way Test (No assumption of equal variances)
oneway.test(novos ~ ars, data = covid_ars2)
pairwise.t.test(covid_ars2$novos, covid_ars2$ars, p.adj = "bonf", pool.sd = FALSE) # Same results
# Kruskal-Wallis test
kruskal_ars <- kruskal.test(covid_ars2$novos ~ covid_ars2$ars, data = covid_ars2)
kruskal_ars # p < 2e-16 --> Rej H0 so there are significant differences between ARS
pairwise.wilcox.test(covid_ars2$novos, covid_ars2$ars, p.adjust.method = "bonf")

# 5. Autocorrelation and Partial Autocorrelation for Time-Series Data
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
covid_corr <- covid_main[,c("data", "confirmados", "confirmados_novos", "recuperados", "obitos", "internados",
                            "internados_uci", "internados_enfermaria", "confirmados_novos_arsnorte",
                            "confirmados_novos_arsalentejo", "confirmados_novos_arsalgarve", 
                            "confirmados_novos_arscentro", "confirmados_novos_arslvt")]
covid_corr <- covid_corr[which(covid_corr$data > "2020-03-09"),]
covid_corr[is.na(covid_corr)] <- 0
sum(is.na(covid_corr))

time <- seq(as.Date("2020-03-10"), as.Date("2021-05-31"), by = "day")
covid_ts <- ts(data = covid_corr, frequency = 365, start = c(2020, as.numeric(format(time[1],"%j"))))
fltr <- c(1/2, rep(1, times = 11), 1/2)/12

# Trend and Seasonality for confirmados_novos, internados
# There are clear trends and seasonality effects in the data
covid_trend3 <- stats::filter(covid_ts[,3], filter = fltr, method = "convo", sides = 2)
plot.ts(covid_trend3, ylab = "Trend", cex = 1)
covid_seas3 <- covid_ts[,3] - covid_trend3
plot.ts(covid_seas3, ylab = "Seasonal effect", xlab = "Month", cex = 1)

covid_trend6 <- stats::filter(covid_ts[,6], filter = fltr, method = "convo", sides = 2)
plot.ts(covid_trend6, ylab = "Trend", cex = 1)
covid_seas6 <- covid_ts[,6] - covid_trend6
plot.ts(covid_seas6, ylab = "Seasonal effect", xlab = "Month", cex = 1)

# Autocorrelation represents the degree of similarity between a given time series and a lagged version of itself
# over successive time intervals.
acf(covid_ts, plot = FALSE, lag.max = 30) # There are significant correlations over time for most of the variables

alpha <- 0.95 # build confidence band for ACF plots
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf(covid_ts, plot = FALSE)$n.used)
# confirm the CI for ACF: getS3method("plot", "acf")

# 5.1. ACF for confirmados_novos
acf1 <- acf(covid_ts[,3], plot = FALSE, lag.max = 30) # consider the last 30 days
acfdf1 <- with(acf1, data.frame(lag, acf))

ggplot(data = acfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.15,0.3,0.45,0.6,0.75,0.90)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# There is a lot of persistence of new confirmed COVID-19 cases from one period to the next.
# These correlations are significant since the values surpass the 95% confidence bands
# There is a slight weekly seasonality pattern

pacf1 <- pacf(covid_ts[,c("confirmados_novos")], plot = FALSE, lag.max = 30)
pacfdf1 <- with(pacf1, data.frame(lag, acf))

ggplot(data = pacfdf1, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(-0.45,-0.3,-0.15,0,0.15,0.3,0.45,0.6,0.75,0.90)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# From this plot we may infer there is an autoregressive term in the data


# 5.2. ACF for internados
acf6 <- acf(covid_ts[,6], plot = FALSE, lag.max = 30)
acfdf6 <- with(acf6, data.frame(lag, acf))

ggplot(data = acfdf6[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)
# There is a lot of persistence of hospitalizations from COVID-19 cases from one period to the next.
# These correlations are significant since the values surpass the 95% confidence bands

pacf6 <- pacf(covid_ts[,c("internados")], plot = FALSE, lag.max = 30)
pacfdf6 <- with(pacf6, data.frame(lag, acf))

ggplot(data = pacfdf6, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# The 1st lag is the most significant suggesting an autoregressive term in the data

# 5.3. ACF for recuperados
acf4 <- acf(covid_ts[,4], plot = FALSE, lag.max = 30)
acfdf4 <- with(acf4, data.frame(lag, acf))

ggplot(data = acfdf4[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelations") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

pacf4 <- pacf(covid_ts[,c("recuperados")], plot = FALSE, lag.max = 30)
pacfdf4 <- with(pacf4, data.frame(lag, acf))

ggplot(data = pacfdf4, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

# 5.4. ACF for internados_uci
acf7 <- acf(covid_ts[,7], plot = FALSE, lag.max = 30)
acfdf7 <- with(acf7, data.frame(lag, acf))

ggplot(data = acfdf7[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelations") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

pacf7 <- pacf(covid_ts[,c("internados_uci")], plot = FALSE, lag.max = 30)
pacfdf7 <- with(pacf7, data.frame(lag, acf))

ggplot(data = pacfdf7, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

# 5.5. ACF for obitos
acf5 <- acf(covid_ts[,5], plot = FALSE, lag.max = 30)
acfdf5 <- with(acf5, data.frame(lag, acf))

ggplot(data = acfdf5[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelations") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

pacf5 <- pacf(covid_ts[,c("obitos")], plot = FALSE, lag.max = 30)
pacfdf5 <- with(pacf5, data.frame(lag, acf))

ggplot(data = pacfdf5, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)


# 5.6 CCF for internados vs confirmados_novos
ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("internados")], lag.max = 30)
# The number of new confirmed cases leads the number of hospitalizations, specially the last 20 days
ccf1 <- ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("internados")], lag.max = 30)
ccfdf1 <- with(ccf1, data.frame(lag, acf))

ggplot(data = ccfdf1, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Cross Correlation") +
  scale_x_continuous(breaks = seq(-0.08,0.08,0.02)) +
  scale_y_continuous(breaks = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# 5.7 CCF for obitos vs confirmados_novos
ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("obitos")], lag.max = 30)
ccf2 <- ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("obitos")], lag.max = 30)
ccfdf2 <- with(ccf2, data.frame(lag, acf))

ggplot(data = ccfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Cross Correlation") +
  scale_x_continuous(breaks = seq(-0.08,0.08,0.02)) +
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# The number of deaths is highly dependent on the number of new confirmed cases from previous days

# 5.8 CCF for recuperados vs confirmados_novos
ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("recuperados")], lag.max = 30)
ccf2 <- ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("recuperados")], lag.max = 30)
ccfdf2 <- with(ccf2, data.frame(lag, acf))

ggplot(data = ccfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Cross Correlation") +
  scale_x_continuous(breaks = seq(-0.08,0.08,0.02)) +
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# 5.9 CCF for internados vs obitos
ccf(covid_ts[,c("internados")], covid_ts[,c("obitos")], lag.max = 30)
ccf3 <- ccf(covid_ts[,c("internados")], covid_ts[,c("obitos")], lag.max = 30)
ccfdf3 <- with(ccf3, data.frame(lag, acf))

ggplot(data = ccfdf3, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.9) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Cross Correlation") +
  scale_x_continuous(breaks = seq(-0.08,0.08,0.02)) +
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.8)

# As expected, the number of deaths is explained by the number of hospitalizations from the previous periods

# 5.10 CCF for confirmados_novos vs confirmados_novos_arsnorte
ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("confirmados_novos_arsnorte")], lag.max = 30)
ccf4 <- ccf(covid_ts[,c("confirmados_novos")], covid_ts[,c("confirmados_novos_arsnorte")], lag.max = 30)
ccfdf4 <- with(ccf4, data.frame(lag, acf))

ggplot(data = ccfdf4, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Cross Correlation") +
  scale_x_continuous(breaks = seq(-0.08,0.08,0.02)) +
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.90,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)


# 6. Correlation Matrix for numeric variables
cor(covid_corr$confirmados, covid_corr$recuperados) # 0.9937532
cor(covid_corr$confirmados, covid_corr$obitos) # 0.9924554
cor(covid_corr$confirmados, covid_corr$internados) # 0.3264903
cor(covid_corr$confirmados_novos, covid_corr$internados) # 0.8282588
cor(covid_corr$confirmados_novos, covid_corr$confirmados_novos_arsnorte) # 0.9225033

cor(na.omit(covid_corr[,c(2:13)])) # correlation matrix
chart.Correlation(na.omit(covid_corr[,c(2:13)]), histogram = TRUE, pch = 19)

#CONSTRUCTION OF THE CORRELATION MATRIX IN A SUITABLE FORMAT
cor_mat <- round(cor(na.omit(covid_corr[,c(2:13)])),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

#HEATMAP OF THE CORRELATION MATRIX
ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)))

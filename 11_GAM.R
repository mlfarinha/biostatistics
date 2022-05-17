#####################################################
###### 11. SCRIPT TO EXPLORE HYPERTENSION DATA ######
#####################################################

# ----------------------------------------------------
# 1. Import the necessary data (hipertensao.csv)
# 2. Fitting model GAM to explain hypertension data
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
library(urca)

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

# Create month variables, variable with total of patients (denom) and percentage from proportion
hyper$month <- month(hyper$Period, abbr = FALSE, label = FALSE)
hyper <- transform(hyper, denom = BP/(Prop/100)) # Use formula at https://transparencia.sns.gov.pt/
hyper <- transform(hyper, percent = Prop/100) # Percentage variable from proportion (values between 0 and 1)
hyper <- transform(hyper, logBP = log(Prop)) # log of proportion data (use if needed)
summary(hyper)

# Create monthly dataset to be modelled
Sys.setlocale("LC_TIME", "C")
hyper_g <- 
  hyper %>%
  group_by(month(hyper$Period, label = TRUE, abbr = FALSE), year(hyper$Period)) %>% 
  summarise(meanBP = mean(BP, na.rm = TRUE),
            meanProp = mean(Prop, na.rm = TRUE),
            meanDenom = mean(denom, na.rm = TRUE),
            meanPercent = mean(percent, na.rm = TRUE),
            meanLog = mean(logBP, na.rm = TRUE))
colnames(hyper_g)[1] <- "Month"
colnames(hyper_g)[2] <- "Year"
hyper_g <- hyper_g[order(hyper_g[,2]), ]

# Add Date variable for future use
hyper_g <- transform(hyper_g, Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))
hyper_g$nMonth <- month(hyper_g$Date, abbr = FALSE, label = FALSE) # number of the month
hyper_g <- transform(hyper_g, Time = as.numeric(Date) / 1000) # time variable (numeric)

# Plot of the data
ggplot(data = hyper_g, aes(x = Date, y = meanPercent)) +
  geom_line(color = "dodgerblue", size = 0.8) +
  xlab("Date") +
  ylab("Monthly mean of patients") +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y", expand = c(0, 0)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())

# Create factor variables for categorical variables year and number of month
hyper_g$nmonth.f <- factor(hyper_g$nMonth)
is.factor(hyper_g$nmonth.f)
hyper_g$year.f <- factor(hyper_g$Year)
is.factor(hyper_g$year.f)

# The density of the data seems to follow a Binomial/Poisson distribution (or Beta distribution)
require(plyr)
mu1 <- ddply(hyper, "Year", summarise, grp.mean=mean(percent))
ggplot(hyper, aes(x=percent, color = Year)) +
  geom_density(size = 1) +
  geom_vline(data=mu1, aes(xintercept=grp.mean, color = Year), linetype="dashed", size=1) +
  xlab("Percentage of Patients with Hypertension with age<65 and BP<150/90 mmHg") +
  ylab("Density") +
  theme_minimal() +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.text = element_text(size = 14),
        legend.title = element_text(size = 17)) +
  scale_color_discrete(name = "Year", labels = c("2014","2015","2016","2017","2018","2019", "2020"))

# Analysing time series data
acf(hyper_g$meanPercent) # indicative of seasonal data
pacf(hyper_g$meanPercent)
Box.test(hyper_g$meanPercent, lag= 1, type="Ljung-Box") # there is correlation between data at 5%, 10% sig. levels
hyper_g$meanPercent %>% ur.kpss() %>% summary() # Unit-root test -> no differencing required
ndiffs(hyper_g$meanPercent) # number of first differences = 0
# Data will not be filtered
ma <- ma(hyper_g$meanPercent,12) # obtain yearly trend
ma <- data.frame(ma)
ma$Date <- hyper_g$Date
ggplot(data = hyper_g, aes(x = Date, y = meanPercent)) +
  geom_line(color = "dodgerblue", size = 0.8) +
  xlab("Date") +
  ylab("Monthly mean of patients") +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y", expand = c(0, 0)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  geom_line(aes(x = Date, y = ma), ma, color = "red", size = 1) +
  stat_smooth(method="loess", color="orange", fill = "#2d74b3", alpha=0.2)

# FITTING GAM MODEL (BINOMIAL FAMILY ASSUMING UNCORRELATED ERRORS) ----------------------------------------------------

# apply floor(meanDenom) since we are using count data; otherwise use quasibinomial

# GAM1
gam_1 <- gam(meanPercent ~ s(nMonth, bs = "cs", k = 12) + s(Year, bs="ps", k=7) + s(Time), weights = floor(meanDenom),
             family = binomial, data = hyper_g, method = "REML") # nmonth.f:bs=re;year.f:bs=re
layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)
# We can see the influence of the variables in percentage of patients with condition
# There is an increase until June and there an abrupt decrease in July which then increases
# The years have fairly similar values with the exception of 2020
summary(gam_1) # The model has interesting results, R-sq.(adj) =  0.985
AIC(gam_1) # AIC = 2268.382
gam_1$gcv.ubre # GCV.Cp = 16.23075
plot(gam_1, residuals = TRUE, pch = 1 , cex = 1)
require(data.table)
datas <- rbindlist(list(hyper_g[,c("meanPercent","Date")], data.table(meanPercent = gam_1$fitted.values,
                                                                      Date = hyper_g[,"Date"])))
datas[, Type := c(rep("Real", nrow(hyper_g)), rep("Fitted", nrow(hyper_g)))]

ggplot(data = datas, aes(Date, meanPercent, group = Type, colour = Type)) +
  geom_line(size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.text = element_text(size = 14),
        legend.title = element_text(size = 17)) +
  labs(x = "Date", y = "Mean percentage of patients with condition")

# GAM2 (replace "cs" by "re" to model nmonth.f as a random effect)
gam_2 <- gam(meanPercent ~ s(nMonth, bs = "cs", k = 12), weights = floor(meanDenom), family = binomial,
             data = hyper_g, method = "REML")
summary(gam_2) # Months seasonality is very important to the model, it is significant, R-sq.(adj) =  0.911
AIC(gam_2) # AIC = 8072.393
gam_2$gcv.ubre # REML = 4090.755 
plot(gam_2, residuals = TRUE, pch = 1 , cex = 1)

# GAM3
gam_3 <- gam(meanPercent ~ s(Year, bs="ps", k=7), weights = floor(meanDenom), family = binomial, data = hyper_g)
summary(gam_3) # Year alone is not a good predictor but it is significant and improves the model
plot(gam_3, residuals = TRUE, pch = 1 , cex = 1)

# GAM4
gam_4 <- gam(meanPercent ~ s(nMonth, Year), weights = floor(meanDenom), family = binomial,
             data = hyper_g, method = "REML")
summary(gam_4) # interaction term is significant, R-sq.(adj) =  0.853
AIC(gam_4) # AIC = 11503.29
gam_4$gcv.ubre # GCV.Cp = 126.1889
plot(gam_4, residuals = TRUE, pch = 1 , cex = 1)

# GAM5
gam_5 <- gam(meanPercent ~ te(nMonth, Year, bs = c("cs", "ps")),weights = floor(meanDenom),data=hyper_g, 
             family=binomial, method = "REML")
summary(gam_5) # R-sq.(adj) =  0.573
plot(gam_5, residuals = TRUE, pch = 1 , cex = 1)

# GAM6
gam_6 <- gam(meanPercent ~ te(nMonth, Year, k = c(12,7), bs = c("cs", "ps")), weights = floor(meanDenom),
             data = hyper_g, family = binomial, method = "REML")
summary(gam_6) # R-sq.(adj) =  0.999   Deviance explained =  100%
plot(gam_6, residuals = TRUE, pch = 1 , cex = 1)

# GAM7 (fix number of smooth basis)
gam_7fx <- gam(meanPercent ~ te(nMonth, Year, k = c(12,7), bs = c("cs", "ps"), fx = TRUE), weights = floor(meanDenom),
               data = hyper_g, family = binomial, method = "REML")
summary(gam_7fx) # R-sq.(adj) =  1   Deviance explained =  100%
plot(gam_7fx, residuals = TRUE, pch = 1 , cex = 1)

# GAM8
gam_8 <- gam(meanPercent ~ s(nMonth,bs="cs",k=12)+s(Year,bs="ps",k=7)+ti(nMonth,Year,k=c(12,7),bs=c("cs","ps")),
             weights = floor(meanDenom), family = binomial, data = hyper_g, method = "REML")
summary(gam_8) # R-sq.(adj) =  0.999   Deviance explained =  100%
AIC(gam_8) # AIC = 972.3117
gam_8$gcv.ubre # GCV.Cp = 0.7970711
plot(gam_8, residuals = TRUE, pch = 1 , cex = 1)
datas <- rbindlist(list(hyper_g[,c("meanPercent","Date")], data.table(meanPercent = gam_8$fitted.values,
                                                                      Date = hyper_g[,"Date"])))
datas[, Type := c(rep("Real", nrow(hyper_g)), rep("Fitted", nrow(hyper_g)))]

ggplot(data = datas, aes(Date, meanPercent, group = Type, colour = Type)) +
  geom_line(size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.text = element_text(size = 14),
        legend.title = element_text(size = 17)) +
  labs(x = "Date", y = "Mean percentage of patients with condition")

# GAM10 (use year as categorical variable)
gam_10 <- gam(meanPercent ~ year.f + s(nMonth, bs="cs", k=12), weights = floor(meanDenom), data = hyper_g,
              family = binomial) # bs = tp gives identical results
summary(gam_10) # R-sq.(adj) =  0.985   Deviance explained = 98.6%
AIC(gam_10) # 2268.402
anova(gam_1, gam_10,  test="LRT") # using a smooth function on year or using year as factor is significantly different
plot(gam_10, residuals = TRUE, pch = 1 , cex = 1)

# GAM11 (use by variables to adjust for year)
gam_11 <- gam(meanPercent ~ year.f + s(nMonth, bs="tp", k=12, by = year.f), weights = floor(meanDenom), data = hyper_g,
              family = binomial, method = "REML") # bs = tp gives identical results
summary(gam_11) # R-sq.(adj) =  0.999   Deviance explained =  100%
require(data.table)
datas <- rbindlist(list(hyper_g[,c("meanPercent","Date")], data.table(meanPercent = gam_11$fitted.values,
                                                                      Date = hyper_g[,"Date"])))
datas[, Type := c(rep("Real", nrow(hyper_g)), rep("Fitted", nrow(hyper_g)))]

ggplot(data = datas, aes(Date, meanPercent, group = Type, colour = Type)) +
  geom_line(size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.text = element_text(size = 14),
        legend.title = element_text(size = 17)) +
  labs(x = "Date", y = "Mean percentage of patients with condition")


# MODEL ANALYSIS ----------------------------------------------------------------------------------------------------

# We further explore model gam_1, gam_2, gam_4, gam_6, gam_7fx, gam_8 and gam_10
aux_r.sq <- cbind(summary(gam_1)$r.sq, summary(gam_2)$r.sq, summary(gam_4)$r.sq, summary(gam_6)$r.sq,
                  summary(gam_7fx)$r.sq, summary(gam_8)$r.sq, summary(gam_10)$r.sq, summary(gam_11)$r.sq)
colnames(aux_r.sq) <- c("GAM1","GAM2","GAM4","GAM6","GAM7","GAM8","GAM10","GAM11");aux_r.sq # best model -> 7, 11
AIC(gam_1, gam_2, gam_4, gam_6, gam_7fx, gam_8, gam_10, gam_11) # lowest AIC -> 8
aux_REML <- cbind(gam_1$gcv.ubre, gam_2$gcv.ubre, gam_4$gcv.ubre, gam_6$gcv.ubre, gam_7fx$gcv.ubre, gam_8$gcv.ubre,
                    gam_10$gcv.ubre, gam_11$gcv.ubre)
colnames(aux_REML) <- c("GAM1","GAM2","GAM4","GAM6","GAM7","GAM8","GAM10","GAM11");aux_GCV.Cp # best model -> 8

# gam.check to check models residuals
gam.check(gam_7fx) # discard
gam.check(gam_8)
gam.check(gam_10)
gam.check(gam_11)
anova(gam_8, gam_10, gam_11,  test = "LRT") # models are significantly different

# Model 11
gam_11Viz <- getViz(gam_11)
# 1. Residuals vs Fitted values
ggplot(hyper_g, aes(gam_11$fitted.values, gam_11$residuals)) + 
  geom_point(color="blue", alpha = 0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(),
        text = element_text(size = 15, face = "italic")) +
  labs(x="Fitted values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 2. QQ-plot
qq(gam_11Viz, method = "simul1", a.qqpoi = list("shape" = 16, size = 1.5, color = hyper_g$year.f),
   a.ablin = list("linetype" = 2, size = 1)) + 
   theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Deviance residuals")
# 3. Residuals vs Factor Level
ggplot(data = hyper_g, aes(year.f, gam_11$residuals, group = year.f)) + 
  geom_point(aes(color = year.f), alpha = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Factor level", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 4. Residuals vs Time
ggplot(data = hyper_g, aes(Date, gam_11$residuals)) + 
  geom_point(aes(color = year.f), alpha = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Factor level", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
durbinWatsonTest(gam_11) # p-value = 0 (DW Stat = 3.532635) -> errors negatively autocorrelated
Box.test(gam_11$residuals, lag = 1, type = "Ljung-Box") # lag-1 autocorrelation is non-zero
bgtest(gam_11, order = 12) # high order serial correlation

alpha <- 0.95 # build confidence band for ACF plots
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf(gam_11$residuals, plot = FALSE)$n.used)
acf1 <- acf(gam_11$residuals, plot = FALSE)
acfdf1 <- with(acf1, data.frame(lag, acf))
ggplot(data = acfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelations") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)
pacf1 <- pacf(gam_11$residuals, plot = FALSE)
pacfdf1 <- with(pacf1, data.frame(lag, acf))
ggplot(data = pacfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

check(gam_11Viz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

# For each year the spline function follows the patterns observed for the year and with a decrease in 2020
vis.gam(gam_11, n.grid = 50, theta = 35, phi = 32, zlab = "", ticktype = "detailed", color = "topo")
vis.gam(gam_11, plot.type = "contour", color = "terrain", contour.col = "black", lwd = 2)


# FITTING GAMM MODEL (BINOMIAL FAMILY ASSUMING CORRELATED ERRORS) -----------------------------------------------------

ctrl <- lmeControl(msVerbose = FALSE,
                   maxIter = 400,
                   msMaxIter = 400,
                   niterEM = 0,
                   tolerance = 1e-8,
                   msTol = 1e-8,
                   msMaxEval = 400)

# GAMM_AR0 (base model with uncorrelated errors)
gam_11_AR0 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                   data = hyper_g, family = binomial, method = "REML", control = ctrl)
summary(gam_11_AR0$gam) # R-sq.(adj) =  0.985 
acf(resid(gam_11_AR0$lme, type = "normalized"))
pacf(resid(gam_11_AR0$lme, type = "normalized"))

# GAMM_AR1
gam_11_AR1 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                   data = hyper_g, family = binomial, method = "REML", control = ctrl,
                   correlation = corAR1(form = ~ 1|year.f))
summary(gam_11_AR1$gam) # R-sq.(adj) =  0.985

# GAMM_AR2
gam_11_AR2 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                   data = hyper_g, family = binomial, method = "REML", control = ctrl,
                   correlation = corARMA(form = ~ 1|year.f, p = 2))
summary(gam_11_AR2$gam) # R-sq.(adj) =  0.985

# GAMM_AR3
gam_11_AR3 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                   data = hyper_g, family = binomial, method = "REML", control = ctrl,
                   correlation = corARMA(form = ~ 1|year.f, p = 3))
summary(gam_11_AR3$gam) # R-sq.(adj) =  0.985

# GAMM_ARMA1
gam_11_ARMA1 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                   data = hyper_g, family = binomial, method = "REML", control = ctrl,
                   correlation = corARMA(form = ~ 1|year.f, p = 1, q = 1))
summary(gam_11_ARMA1$gam) # R-sq.(adj) =  0.985

# GAMM_ARMA2
gam_11_ARMA2 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                     data = hyper_g, family = binomial, method = "REML", control = ctrl,
                     correlation = corARMA(form = ~ 1|year.f, p = 0, q = 1))
summary(gam_11_ARMA2$gam) # R-sq.(adj) =  0.985

# GAMM_ARMA3
gam_11_ARMA3 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                     data = hyper_g, family = binomial, method = "REML", control = ctrl,
                     correlation = corARMA(form = ~ 1|year.f, p = 0, q = 2))
summary(gam_11_ARMA3$gam) # R-sq.(adj) =  0.985

# GAMM_ARMA4
gam_11_ARMA4 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                     data = hyper_g, family = binomial, method = "REML", control = ctrl,
                     correlation = corARMA(form = ~ 1|year.f, p = 1, q = 2))
summary(gam_11_ARMA4$gam) # R-sq.(adj) =  0.985

# GAMM_ARMA5
gam_11_ARMA5 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                     data = hyper_g, family = binomial, method = "REML", control = ctrl,
                     correlation = corARMA(form = ~ 1|year.f, p = 2, q = 2))
summary(gam_11_ARMA5$gam) # R-sq.(adj) =  0.985

# GAMM_CAR1
gam_11_CAR1 <- gamm(meanPercent ~ year.f + s(nMonth, bs = "tp", k = 12), weights = floor(meanDenom),
                     data = hyper_g, family = binomial, method = "REML", control = ctrl,
                     correlation = corCAR1(form = ~ 1|year.f))
summary(gam_11_CAR1$gam) # R-sq.(adj) =  0.985

# MODEL ANALYSIS -----------------------------------------------------------------------------------------------------
AIC(gam_11_AR0$lme, gam_11_AR1$lme, gam_11_AR2$lme, gam_11_AR3$lme, gam_11_ARMA1$lme, gam_11_ARMA2$lme, gam_11_ARMA3$lme,
    gam_11_ARMA4$lme, gam_11_ARMA5$lme, gam_11_CAR1$lme) # -> lowest AIC: AR3 or ARMA3 or ARMA4 or ARMA5
BIC(gam_11_AR0$lme, gam_11_AR1$lme, gam_11_AR2$lme, gam_11_AR3$lme, gam_11_ARMA1$lme, gam_11_ARMA2$lme, gam_11_ARMA3$lme,
    gam_11_ARMA4$lme, gam_11_ARMA5$lme, gam_11_CAR1$lme) # -> lowest BIC: AR3 or ARMA3 or ARMA4 or ARMA5

# GAMM_AR3
gam.check(gam_11_AR3$gam) # correct degrees of freedom
gam_11_AR3Viz <- getViz(gam_11_AR3$gam)
check(gam_11_AR3Viz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

#This code is modified for D.L. Miller's dsm package for distance sampling, from
#the rqgam.check function. The code is designed to extract randomized quantile 
#residuals from GAMs, using the family definitions in mgcv. Note statmod only
#supports RQ residuals for the following families: Tweedie, Poisson, Gaussian,  Any errors are due to Eric Pedersen
library(statmod) #This has functions for randomized quantile residuals
# https://github.com/eric-pedersen/mgcv-esa-workshop/blob/master/code_snippets/quantile_resid.R
rqresiduals = function (gam.obj) {
  if(!"gam" %in% attr(gam.obj,"class")){
    stop('"gam.obj has to be of class "gam"')
  }
  if (!grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
             gam.obj$family$family)){
    stop(paste("family " , gam.obj$family$family, 
               " is not currently supported by the statmod library, 
                 and any randomized quantile residuals would be inaccurate.",
               sep=""))
  }
  if (grepl("^Tweedie", gam.obj$family$family)) {
    if (is.null(environment(gam.obj$family$variance)$p)) {
      p.val <- gam.obj$family$getTheta(TRUE)
      environment(gam.obj$family$variance)$p <- p.val
    }
    qres <- qres.tweedie(gam.obj)
  }
  else if (grepl("^Negative Binomial", gam.obj$family$family)) {
    if ("extended.family" %in% class(gam.obj$family)) {
      gam.obj$theta <- gam.obj$family$getTheta(TRUE)
    }
    else {
      gam.obj$theta <- gam.obj$family$getTheta()
    }
    qres <- qres.nbinom(gam.obj)
  }
  else {
    qres <- qresid(gam.obj)
  }
  return(qres)
}

AR3_resid = residuals(gam_11_AR3$gam, type="deviance")
AR3_rqresid = rqresiduals(gam_11_AR3$gam)
layout(matrix(1:2, nrow=1))
plot(gam_11_AR3$gam$linear.predictors,AR3_resid)
plot(gam_11_AR3$gam$linear.predictors,AR3_rqresid)
plot(gam_11_AR3$gam, scale = 0)
plot(gam_11_AR3$gam, residuals = TRUE, pch = 19, cex = 0.75)
concurvity(gam_11_AR3$gam, full = TRUE) # model is ok in terms of co-linearity

# 1. Normalized Residuals vs Fitted values
ggplot(,aes(fitted(gam_11_AR3$lme), resid(gam_11_AR3$lme, type = "normalized"))) + 
  geom_point(color="blue", alpha = 0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(),
        text = element_text(size = 15, face = "italic")) +
  labs(x="Fitted values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 2. QQ-plot
qq(gam_11_AR3Viz, method = "simul1", a.qqpoi = list("shape" = 16, size = 1.5, color = hyper_g$year.f),
   a.ablin = list("linetype" = 2, size = 1)) + 
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Deviance residuals")

## Model Checking function (https://raw.githubusercontent.com/gavinsimpson/random_code/master/tsDiagGamm.R)
tsDiagGamm <- function(x, timevar, observed, f = 0.3, type = "normalized") {
  resi <- resid(x$lme, type = type)
  fits <- fitted(x$lme)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  viz <- getViz(x$gam)
  check(viz, a.qq = list(method = "tnorm", a.cipoly = list(fill = "light blue")))[[1]]
  #qqnorm(resi)
  #qqline(resi)
  acf(resi, main = "ACF of Residuals")
}
tsDiagGam <- function(x, timevar, observed, f = 0.3) {
  resi <- resid(x$gam)
  fits <- fitted(x$gam)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  viz <- getViz(x$gam)
  check(viz, a.qq = list(method = "tnorm", a.cipoly = list(fill = "light blue")))[[1]]
  #qqnorm(resi)
  #qqline(resi)
  acf(resi, main = "ACF of Residuals")
}
with(hyper_g, tsDiagGamm(gam_11_AR3, timevar = Date, observed = meanPercent))
with(hyper_g, tsDiagGam(gam_11_AR3, timevar = Date, observed = meanPercent))

# Correlogram of model AR(3)
res <- resid(gam_11_AR3$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(3) errors")
pacf(res, lag.max = 36, main = "pACF- AR(3) errors")

# GAMM_ARMA5
gam.check(gam_11_ARMA5$gam) # correct degrees of freedom
gam_11_ARMA5Viz <- getViz(gam_11_ARMA5$gam)
check(gam_11_ARMA5Viz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
with(hyper_g, tsDiagGamm(gam_11_ARMA5, timevar = Date, observed = meanPercent))
with(hyper_g, tsDiagGam(gam_11_ARMA5, timevar = Date, observed = meanPercent))

ARMA5_resid = residuals(gam_11_ARMA5$gam, type="deviance")
ARMA5_rqresid = rqresiduals(gam_11_ARMA5$gam)
layout(matrix(1:2, nrow=1))
plot(gam_11_ARMA5$gam$linear.predictors,ARMA5_resid)
plot(gam_11_ARMA5$gam$linear.predictors,ARMA5_rqresid)

plot(gam_11_ARMA5$gam, scale = 0)
plot(gam_11_ARMA5$gam, residuals = TRUE, pch = 19, cex = 0.75)
acf(resid(gam_11_AR3$lme, type = "normalized"), lag.max = 36, main = "ACF - ARMA(5) errors")
pacf(resid(gam_11_AR3$lme, type = "normalized"), lag.max = 36, main = "ACF - ARMA(5) errors")
concurvity(gam_11_ARMA5$gam, full = TRUE) # model is ok in terms of colinearity

##################################################################################################################

# MODEL WITH INTERACTION TERM (UNCORRELATED ERRORS) ----------------------------------------------------------------

ctrl <- lmeControl(msVerbose = FALSE,
                   maxIter = 400,
                   msMaxIter = 400,
                   niterEM = 0,
                   tolerance = 1e-8,
                   msTol = 1e-8,
                   msMaxEval = 400)

gam_int <- gam(meanPercent ~ s(nMonth,bs="tp",k=12) + year.f + ti(nMonth, Year, k=c(12,7)),
             weights = floor(meanDenom), family = binomial, data = hyper_g, method = "REML")
summary(gam_int) # R-sq.(adj) =  0.999   Deviance explained =  100%
AIC(gam_int) # AIC = 976.711
gam_int$gcv.ubre # REML = 599.0944 
plot(gam_int, residuals = TRUE, pch = 1 , cex = 1)
concurvity(gam_int, full = TRUE) # no co-linearity problems
anova(gam_10, gam_int, test="LRT") # interaction term is significant
require(data.table)
datas <- rbindlist(list(hyper_g[,c("meanPercent","Date")], data.table(meanPercent = gam_int$fitted.values,
                                                                      Date = hyper_g[,"Date"])))
datas[, Type := c(rep("Real", nrow(hyper_g)), rep("Fitted", nrow(hyper_g)))]

ggplot(data = datas, aes(Date, meanPercent, group = Type, colour = Type)) +
  geom_line(size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0), legend.text = element_text(size = 14),
        legend.title = element_text(size = 17)) +
  labs(x = "Date", y = "Mean percentage of patients with condition")

# Model Assumptions
gam.check(gam_int) # degrees of freedom are ok
gam_intViz <- getViz(gam_int)
check(gam_intViz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

# 1. Residuals vs Fitted values
ggplot(hyper_g, aes(gam_int$fitted.values, gam_int$residuals)) + 
  geom_point(color="blue", alpha = 0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(),
        text = element_text(size = 15, face = "italic")) +
  labs(x="Fitted values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 2. QQ-plot
qq(gam_intViz, method = "simul1", a.qqpoi = list("shape" = 16, size = 1.5, color = hyper_g$year.f),
   a.ablin = list("linetype" = 2, size = 1)) + 
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  ylab("Deviance residuals")
# 3. Residuals vs Factor Level
ggplot(data = hyper_g, aes(year.f, gam_int$residuals, group = year.f)) + 
  geom_point(aes(color = year.f), alpha = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Factor level", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 4. Residuals vs Time
ggplot(data = hyper_g, aes(Date, gam_int$residuals)) + 
  geom_point(aes(color = year.f), alpha = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank()) +
  labs(x="Factor level", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
durbinWatsonTest(gam_int) # p-value = 0 (DW Stat = 3.222049) -> errors negatively autocorrelated
Box.test(gam_int$residuals, lag = 1, type = "Ljung-Box") # lag-1 autocorrelation is non-zero
bgtest(gam_int, order = 12) # high order serial correlation

alpha <- 0.95 # build confidence band for ACF plots
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf(gam_11$residuals, plot = FALSE)$n.used)
acf1 <- acf(gam_int$residuals, plot = FALSE)
acfdf1 <- with(acf1, data.frame(lag, acf))
ggplot(data = acfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelations") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)
pacf1 <- pacf(gam_int$residuals, plot = FALSE)
pacfdf1 <- with(pacf1, data.frame(lag, acf))
ggplot(data = pacfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,0.08,0.01)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

###################################################################################################################

# MODEL WITH INTERACTION TERM AND CORRELATED ERROS (could not fit AR and ARMA models) ------------------------------
# MODEL PRESENTED IN REPORT

gam_int_CAR1 <- gamm(meanPercent ~ 0 + s(nMonth,bs="tp",k=12) + year.f + ti(nMonth,Year,k=c(12,7)),
                     weights = floor(meanDenom), family = binomial, data = hyper_g, method = "REML", control = ctrl,
                     correlation = corCAR1(form = ~ 1|year.f))
summary(gam_int_CAR1$gam) # R-sq.(adj) =  0.999
summary(gam_int_CAR1$lme)
AIC(gam_int_CAR1$lme) # AIC = -180.4951
BIC(gam_int_CAR1$lme) # BIC = -148.8944

gam.check(gam_int_CAR1$gam) # degrees of freedom ok
with(hyper_g, tsDiagGamm(gam_int_CAR1, timevar = Date, observed = meanPercent))
with(hyper_g, tsDiagGam(gam_int_CAR1, timevar = Date, observed = meanPercent))
gam_int_CAR1Viz <- getViz(gam_int_CAR1$gam)
check(gam_int_CAR1Viz,
      a.qq = list(method = "tnorm",
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
resid <- residuals(gam_int_CAR1$gam, type = "deviance")
hist(resid, xlab = "Residuals", main = "Histogram of residuals", bins = 1)
CAR1_resid <- residuals(gam_int_CAR1$gam, type="deviance")
CAR1_rqresid <- rqresiduals(gam_int_CAR1$gam)
layout(matrix(1:2, nrow=1))
plot(gam_int_CAR1$gam$linear.predictors,CAR1_resid)
plot(gam_int_CAR1$gam$linear.predictors,CAR1_rqresid)
plot(gam_int_CAR1$gam, scale = 0)
plot(gam_int_CAR1$gam, residuals = TRUE, pch = 19, cex = 0.75)
concurvity(gam_int_CAR1$gam, full = TRUE) # model is ok in terms of co-linearity

# 1. Normalized Residuals vs Fitted values
ggplot(,aes(napredict(gam_int_CAR1$gam$na.action, gam_int_CAR1$gam$linear.predictors),
            resid(gam_int_CAR1$gam, type = "deviance"))) + 
  geom_point(color="dodgerblue", alpha = 1) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x="Linear predictor", y = "Residuals") +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8)
# 2. QQ-plot
qq(gam_int_CAR1Viz, method = "simul1", a.qqpoi = list("shape" = 16, size = 1.5),
   a.ablin = list("linetype" = 2, size = 1)) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17), title = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  ylab("Deviance residuals")
# Histogram of residuals
ggplot(,aes(x = resid(gam_int_CAR1$gam, type = "deviance"))) + 
  geom_histogram(color="black", fill="dodgerblue", alpha = 0.6, binwidth = 0.3) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x="Residuals", y = "Count") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22))
# Observed vs fitted values
ggplot(,aes(fitted(gam_int_CAR1$gam),
            napredict(gam_int_CAR1$gam$na.action, gam_int_CAR1$gam$y))) + 
  geom_point(color="dodgerblue", alpha = 1) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x="Fitted values", y = "Observed values")
# 4. Residuals vs Time
colnames(hyper_g)["year.f"] <- "Year"
ggplot(data = hyper_g, aes(Date, resid(gam_int_CAR1$gam, type = "deviance"))) + 
  geom_point(color = "dodgerblue", alpha = 1) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 17),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x="Date (observation order)", y = "Residuals") +
  stat_smooth(method="loess", color="#b22222", fill = "#2d74b3", alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", alpha=0.8) +
  scale_x_date(limits = as.Date(c("2014-01-01","2021-01-01")), date_breaks = "1 year", date_labels =  "%Y", 
               expand = c(0.04,0))


acf(resid(gam_int_CAR1$lme, type = "normalized"), lag.max = 36, main = "ACF - CAR1 errors")
pacf(resid(gam_int_CAR1$lme, type = "normalized"), lag.max = 36, main = "PACF - CAR1 errors")
alpha <- 0.95 # build confidence band for ACF plots
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf(gam_int_CAR1$gam$residuals, plot = FALSE)$n.used)
acf1 <- acf(resid(gam_int_CAR1$gam, type = "deviance"), plot = FALSE)
acfdf1 <- with(acf1, data.frame(lag, acf))
ggplot(data = acfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Autocorrelation") +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_y_continuous(breaks = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)
pacf1 <- pacf(resid(gam_int_CAR1$gam, type = "deviance"), plot = FALSE)
pacfdf1 <- with(pacf1, data.frame(lag, acf))
ggplot(data = pacfdf1[-1,], mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank())+
  xlab("Lag") + 
  ylab("Partial Autocorrelation") +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue', size = 0.7)

####################################################################################################################

# TEST MODELS WITH 2021 OBSERVATIONS -------------------------------------------------------------------------------

hyper <- read.table("hipertensao.csv", sep = ";", header = TRUE)
colnames(hyper)
colnames(hyper)[1] <- "Period"
colnames(hyper)[2] <- "Region"
colnames(hyper)[3] <- "Entity"
colnames(hyper)[4] <- "GPS"
colnames(hyper)[5] <- "BP"
colnames(hyper)[6] <- "Prop"
hyper$Period <- as.Date(paste0(hyper$Period,"-01"), format="%Y-%m-%d", tz="GMT")
hyper$Region <- as.character(hyper$Region)
hyper$BP <- as.numeric(hyper$BP)
hyper$Prop <- as.numeric(hyper$Prop)
hyper$Period <- as.Date(hyper$Period, format="%Y-%m-%d", tz="GMT")
hyper$Year <- year(hyper$Period)
hyper$Year <- factor(hyper$Year)
hyper$month <- month(hyper$Period, abbr = FALSE, label = FALSE)
hyper <- transform(hyper, denom = BP/(Prop/100)) # Use formula at https://transparencia.sns.gov.pt/
hyper <- transform(hyper, percent = Prop/100) # Percentage variable from proportion (values between 0 and 1)
Sys.setlocale("LC_TIME", "C")
hyper_test <- 
  hyper %>%
  group_by(month(hyper$Period, label = TRUE, abbr = FALSE), year(hyper$Period)) %>% 
  summarise(meanBP = mean(BP, na.rm = TRUE),
            meanProp = mean(Prop, na.rm = TRUE),
            meanDenom = mean(denom, na.rm = TRUE),
            meanPercent = mean(percent, na.rm = TRUE))
colnames(hyper_test)[1] <- "Month"
colnames(hyper_test)[2] <- "Year"
hyper_test <- hyper_test[order(hyper_test[,2]), ]
hyper_test <- transform(hyper_test, Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))
hyper_test$nMonth <- month(hyper_test$Date, abbr = FALSE, label = FALSE) # number of the month
hyper_test <- transform(hyper_test, Time = as.numeric(Date) / 1000) # time variable (numeric)
hyper_test$nmonth.f <- factor(hyper_test$nMonth)
is.factor(hyper_test$nmonth.f)
hyper_test$year.f <- factor(hyper_test$Year)
is.factor(hyper_test$year.f)

ctrl <- lmeControl(msVerbose = FALSE,
                   maxIter = 400,
                   msMaxIter = 400,
                   niterEM = 0,
                   tolerance = 1e-8,
                   msTol = 1e-8,
                   msMaxEval = 400)

# Remove factor to allow for prediction (adapt models so that they can be run)
gam_11_preduncorr <- gamm(meanPercent ~ s(Year, bs = "ps", k=7) + s(nMonth, bs = "tp", k = 12),
                    weights = floor(meanDenom), data = hyper_g, family = binomial, method = "REML",
                    control = ctrl)
gam_11_pred <- gamm(meanPercent ~ s(Year, bs = "ps", k=7) + s(nMonth, bs = "tp", k = 12),
                         weights = floor(meanDenom), data = hyper_g, family = binomial, method = "REML",
                         control = ctrl, correlation = corCAR1(form = ~ 1|year.f))
gam_int_pred <- gamm(meanPercent ~ s(Year, bs = "ps", k=7) + s(nMonth,bs="tp",k=12) + ti(nMonth, Year, k=c(12,7)),
                     weights = floor(meanDenom), family = binomial, data = hyper_g, method = "REML", control = ctrl,
                     correlation = corCAR1(form = ~ 1|year.f))
p1 <- predict(gam_11_preduncorr$gam, newdata = hyper_test, type = "response")
p2 <- predict(gam_11_pred$gam, newdata = hyper_test, type = "response")
p3 <- predict(gam_int_pred$gam, newdata = hyper_test, type = "response")
plot(meanPercent ~ Date, data = hyper_test, type = "p", ylab = "Percentage of patients with condition", xlab = "Date")
lines(p1 ~ Date, data = hyper_test, col = "red")
lines(p2 ~ Date, data = hyper_test, col = "blue")
lines(p3 ~ Date, data = hyper_test, col = "green")
legend("topright", legend = c("Uncorrelated Errors","No interaction", "Interaction"),
       bty = "n", col = c("red","blue", "green"), lty = 1)
require(data.table)
datas <- rbindlist(list(hyper_test[,c("meanPercent","Date")],
                        data.table(meanPercent = predict(gam_int_pred$gam, newdata = hyper_test, type = "response"),
                                   Date = hyper_test[,"Date"])))
datas[, Values := c(rep("Observed", nrow(hyper_test)),
                  rep("Predicted", nrow(hyper_test)))]

ggplot(data = datas, aes(Date, meanPercent, group = Values, colour = Values)) +
  geom_line(size = 0.8) +
  theme(text = element_text(size = 14, face = "italic"), axis.title = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)), panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent", colour = "transparent"), legend.position="top") +
  labs(x = "Date", y = "Mean percentage of patients with condition") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  scale_y_continuous(breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55))

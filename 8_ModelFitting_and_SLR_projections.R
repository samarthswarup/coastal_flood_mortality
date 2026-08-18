##########################################
# Step 8 - Projection code               #
#                                        #
# 2026-07-20                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# SETUP & PARAMETERS ==============================================================================

# The YEAR_THRESHOLD parameter indicates the first year for which events should be 
# included in the analysis. The primary results use 1990; however, we also considered
# the year 2000. Some portions of the script are hard-coded to produce results for
# both 1990 and 2000.

rm(list=ls())
library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(tidyverse)
library(zoo)
library(glmmTMB)
library(MASS)
library(lme4)
library(nlme)
library(sjPlot)
library(here)
library(ggplot2)
library(DHARMa)
library(splines2)
library(ggstatsplot)
library(performance)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This script has two entry points that each set the working
# directory independently -- the main run below, and the "QUICK LOAD" section
# further down (which starts a fresh session via rm(list=ls()) to skip
# straight to plot generation using already-saved .RDS files). Both must
# point to the same project root.
setwd("~/coastal_flood_mortality")

# Create output directories (safe to run even if they already exist)
dir.create("Output",                                    showWarnings = FALSE, recursive = TRUE)
dir.create("Output/Plots",                             showWarnings = FALSE, recursive = TRUE)
dir.create("Intermediate outputs/Output from Step 8",  showWarnings = FALSE, recursive = TRUE)
dir.create("Intermediate outputs/Output from Step 8/Country tables", showWarnings = FALSE, recursive = TRUE)

step7_date_append_output = "2026-07-05"
step8_date_append_output = "2026-07-20" # to identify relevant input files in 
                                        # Intermediate outputs/Output from Step 8
                                        # if they exist for "quick load" functionality

date_append_output = "2026-07-20"       # for files created here


emdat.data.filtered <- read_excel(paste("Intermediate outputs/Output from Step 7/",
                                        "EMDAT events with exposed pops, HDI GNI and prev events ",
                                        step7_date_append_output,".xlsx",sep=""))

country.data =
  read.csv(paste("Inputs/Identify mismatched and missing countries - step 3/",
                 "SLIIDERS country codes with names and territorial mapping.csv",
                 sep=""))

YEAR_THRESHOLD <- 1990

# The B parameter indicates the number of iterations to utilize when performing 
# bootstrapping. NOTE - Runtime will be quite long (>24 hours) when using values
# above 100. If simply testing out the script, use a small number, like B <- 5.

B <- 100


####Importing data

#create column with disaster number only, removing country code, to get an event specific variable
df <- emdat.data.filtered %>%
  mutate(DisNo.nocoun = gsub("[A-Za-z]", "", DisNo.))

df$no.events.5.years = NA

for(i in 1:nrow(df)){
  
  if(df$Start.Year[i]>=1990&
     df$Start.Year[i]<=2024){
    
    df$no.events.5.years[i] =
      
      sum((df$Start.Year%in%
             (df$Start.Year[i]-5):
             df$Start.Year[i])&
            df$ISO==df$ISO[i])
    
  }
}

trendData <- data.frame(df)

df <- df%>% filter(Start.Year>1989)

# RUN TREND TEST

library(modifiedmk)

trendData <- trendData[trendData$Start.Year >= YEAR_THRESHOLD, ]

trendData <- trendData[trendData$pop.exposed.local.else.adm1.else.national.100yr > 0, ]

trendData$Start.Day[!is.na(trendData$Start.Day) & nchar(trendData$Start.Day) == 1] <- 
  paste("0", trendData$Start.Day[!is.na(trendData$Start.Day) & 
                                   nchar(trendData$Start.Day) == 1], sep = "")

trendData$Start.Month[!is.na(trendData$Start.Month) & nchar(trendData$Start.Month) == 1] <- 
  paste("0", trendData$Start.Month[!is.na(trendData$Start.Month) & 
                                     nchar(trendData$Start.Month) == 1], sep = "")

trendData$Start.Date <- as.Date(
  paste(trendData$Start.Day, "-",
        trendData$Start.Month, "-",
        trendData$Start.Year, sep = ""),
  format = "%d-%m-%Y")

trendData$death.rate <- trendData$Total.Deaths / 
  trendData$pop.exposed.local.else.adm1.else.national.100yr

trendData$death.rate.na.as.0 <- trendData$total.deaths.na.as.0 / 
  trendData$pop.exposed.local.else.adm1.else.national.100yr

for (i in 1:nrow(trendData)) {
  trendData$death.rate <- ifelse(trendData$death.rate > 1, NA, trendData$death.rate)
  trendData$death.rate.na.as.0 <- ifelse(trendData$death.rate.na.as.0 > 1,
                                         NA, trendData$death.rate.na.as.0)
}

# RAW VALUES

plot(trendData$Start.Date, trendData$death.rate)
round(mkttest(trendData$death.rate[!is.na(trendData$death.rate)]), 3)

plot(trendData$Start.Date, trendData$death.rate.na.as.0)
round(mkttest(trendData$death.rate.na.as.0[!is.na(trendData$death.rate.na.as.0)]), 3)

plot(trendData$Start.Date, trendData$pop.exposed.local.else.adm1.else.national.100yr)
round(mkttest(trendData$pop.exposed.local.else.adm1.else.national.100yr), 3)


# hemi <- read.csv("countryList.csv")
# names(hemi)[1] <- "Country"

# trendData <- merge(trendData, hemi, by = "Country", all.x = TRUE)

# Check for possible merge issues (e.g., duplicate country codes in country.data)

duplicate.country.code = 
  unique(country.data$SLIIDERS.country.code[
    which(unlist(lapply(country.data$SLIIDERS.country.code,function(x) 
  length(which(country.data$SLIIDERS.country.code==x))))>1)])

if(length(duplicate.country.code)>0){
  print(paste("Warning! ",
              duplicate.country.code,
              " appear(s) >1 time in country.data. Ensure that merge",
              " operation accounts for this.",sep=""))
}

country.data.to.merge =
  country.data[setdiff(1:nrow(country.data),
                       which(country.data$Name..ISO.3166.if.available.==
                               "Canary Islands")),]

duplicate.country.code = 
  unique(country.data.to.merge$SLIIDERS.country.code[
    which(unlist(lapply(country.data.to.merge$SLIIDERS.country.code,function(x) 
      length(which(country.data.to.merge$SLIIDERS.country.code==x))))>1)])

if(length(duplicate.country.code)>0){
  print(paste("Warning! ",
              duplicate.country.code,
              " appear(s) >1 time in country.data. Ensure that merge",
              " operation accounts for this.",sep=""))
}

trendData <- merge(trendData, country.data.to.merge, by.x = "ISO", 
                   by.y = "SLIIDERS.country.code", all.x = TRUE)

names(trendData)[which(names(trendData)=="Hemisphere")] = "hemi"

which(is.na(trendData$hemi)) # missing hemisphere data
trendData$ISO[which(is.na(trendData$hemi))] # ANT is missing because "Netherlands Antilles"
                                            # is not a country in SLIIDERS. It is 
                                            # decomposed into BES, CUW, ABW, and SXM.
                                            # These are all in the Northern Hemisphere.
                                            # SPI is missing because SLIIDERS considers
                                            # Canary Islands to be part of Spain. Also
                                            # Northern Hemisphere.

trendData$hemi[which(is.na(trendData$hemi)&
                       (trendData$ISO=="ANT"|trendData$ISO=="SPI"))] = "N"


yearlyEvents <- NULL
monthVec <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
prettyMonth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

HOLD <- trendData

trendData <- HOLD
# trendData <- trendData[trendData$hemisphere == "N", ]
# trendData <- trendData[trendData$hemisphere == "S", ]

# NUMBER OF EVENTS, YEAR-BY-YEAR

yearlyEvents <- NULL

for (j in 1:length(unique(trendData$Start.Year))) {
  thisYear <- trendData[trendData$Start.Year == unique(trendData$Start.Year)[j], ]
  yearlyEvents <- rbind(yearlyEvents, data.frame(year = unique(trendData$Start.Year)[j],
                                                 n = nrow(thisYear)))
}

yearlyEvents <- yearlyEvents[order(yearlyEvents$year), ]
round(mkttest(yearlyEvents$n), 3)

# NUMBER OF EVENTS, MONTH-BY-MONTH

yearlyEvents <- NULL

for (i in 1:length(monthVec)) {
  thisMonth <- trendData[!is.na(trendData$Start.Year) & trendData$Start.Month == monthVec[i], ]
  thisMonth <- thisMonth[order(thisMonth$Start.Year), ]
  monthlyEvents <- NULL
  
  if (nrow(thisMonth) > 0) {
    for (j in 1:length(unique(thisMonth$Start.Year))) {
      thisYear <- thisMonth[thisMonth$Start.Year == unique(thisMonth$Start.Year)[j], ]
      monthlyEvents <- rbind(monthlyEvents, data.frame(year = unique(thisMonth$Start.Year)[j],
                                                       n = nrow(thisYear)))
    }
  }
  
  yearlyEvents[[i]] <- monthlyEvents
  names(yearlyEvents)[i] <- prettyMonth[i]
  
}

for (i in 1:length(yearlyEvents)) {
  if (nrow(yearlyEvents[[i]]) >= 3) {
    print(names(yearlyEvents)[i])
    print(round(mkttest(yearlyEvents[[i]]$n), 3))
  }
}

# PLOT YEARLY AVERAGE DEATH RATE FROM RAW DATA (NO MODEL)

par(mfrow = c(1, 2))

deathRates <- NULL
trendData <- trendData[order(trendData$Start.Year), ]

for (i in 1:length(unique(trendData$Start.Year))) {
  current <- trendData[trendData$Start.Year == unique(trendData$Start.Year)[i], ]
  deathRates <- c(deathRates, mean(current$death.rate.na.as.0))
}

# 1990 - 2024

yearSpline <- bSpline(1:length(unique(trendData$Start.Year)), df = 3)
splineMod <- lm(deathRates ~ yearSpline)
splineBeta <- coef(splineMod)

outVals <- NULL

for (i in 1:nrow(yearSpline)) {
  outVals <- c(outVals, 
               splineBeta[1] +
                 splineBeta[2]*yearSpline[i, 1] +
                 splineBeta[3]*yearSpline[i, 2] +
                 splineBeta[4]*yearSpline[i, 3])
}

outVals <- as.numeric(outVals)

# 2000 - 2024

yearSpline2000 <- bSpline(1:length(2000:2024), df = 3)
splineMod2000 <- lm(deathRates[11:length(deathRates)] ~ yearSpline2000)
splineBeta2000 <- coef(splineMod2000)

outVals2000 <- NULL

for (i in 1:nrow(yearSpline2000)) {
  outVals2000 <- c(outVals2000, 
                   splineBeta2000[1] +
                     splineBeta2000[2]*yearSpline2000[i, 1] +
                     splineBeta2000[3]*yearSpline2000[i, 2] +
                     splineBeta2000[4]*yearSpline2000[i, 3])
}

outVals2000 <- as.numeric(outVals2000)

# MAKE PLOT

plot(0:(length(deathRates)-1), outVals, type = 'l',
     ylim = c(0, 0.025),
     xlab = "Year", ylab = "Average Death Rate",
     main = "B-Spline Representation of Average Death Rate vs. Year",
     xaxt = 'n', lwd = 2)

points(0:(length(deathRates)-1), deathRates, col = "grey50")

lines(10:(10+length(outVals2000)-1), outVals2000, type = 'l',
      col = '#1496E1', lwd = 2)

axis(1, at = seq(0, 34, 2),
     labels = seq(1990, 2024, 2))

legend("topright", inset = .02, title="Timeframe",
       c("1990-2024", "2000-2024"), 
       fill=c("black", "#1496E1"), 
       cex=0.8, horiz = TRUE)

# PLOT EVENT DEATH RATES FROM RAW DATA (NO MODEL)

deathRates <- trendData$death.rate.na.as.0
days <- as.numeric(as.Date(trendData$Start.Date) - as.Date("1990-01-01"))

# 1990 - 2024

daySpline <- bSpline(days, df = 3)
splineMod <- lm(deathRates ~ daySpline)
splineBeta <- coef(splineMod)

outVals <- NULL

for (i in 1:nrow(daySpline)) {
  outVals <- c(outVals, 
               splineBeta[1] +
                 splineBeta[2]*daySpline[i, 1] +
                 splineBeta[3]*daySpline[i, 2] +
                 splineBeta[4]*daySpline[i, 3])
}

outVals <- as.numeric(outVals)

dayFrame <- data.frame(days = days, outVals = outVals)
dayFrame <- dayFrame[order(dayFrame$days), ]

# 2000 - 2024

daySpline2000 <- bSpline(days[days > 3750], df = 3)

splineMod2000 <- lm(deathRates[days > 3750] ~ daySpline2000)
splineBeta2000 <- coef(splineMod2000)

outVals2000 <- NULL

for (i in 1:nrow(daySpline2000)) {
  outVals2000 <- c(outVals2000, 
                   splineBeta2000[1] +
                     splineBeta2000[2]*daySpline2000[i, 1] +
                     splineBeta2000[3]*daySpline2000[i, 2] +
                     splineBeta2000[4]*daySpline2000[i, 3])
}

outVals2000 <- as.numeric(outVals2000)

dayFrame2000 <- data.frame(days = days[days > 3750], outVals = outVals2000)
dayFrame2000 <- dayFrame2000[order(dayFrame2000$days), ]

# MAKE PLOT

plot(dayFrame$days, dayFrame$outVals, type = 'l',
     ylim = c(0, 0.025),
     xlab = "Year", ylab = "Death Rate",
     main = "B-Spline Representation of Death Rate vs. Year",
     xaxt = 'n', lwd = 2)

points(days[deathRates <= 0.02], 
       deathRates[deathRates <= 0.02], col = "grey50")

bigDays <- days[deathRates > 0.02]
bigRates <- deathRates[deathRates > 0.02]

for (i in 1:length(bigDays)) {
  points(bigDays[i], 0.02, 
         pch = 21, bg = "steelblue2", col = "black")
}

lines(c(1900, 2300), c(0.05, 0.05), col = "steelblue2", lwd = 2)
lines(c(2300, 2200), c(0.05, 0.0495), col = "steelblue2", lwd = 2)
lines(c(2300, 2200), c(0.05, 0.0505), col = "steelblue2", lwd = 2)

text(4000, 0.0215, "24 Events with Death Rates > 0.02", 
     col = "steelblue2", font = 2, cex = 0.8)

lines(dayFrame2000$days, dayFrame2000$outVals, type = 'l',
      col = '#1496E1', lwd = 2)

axis(1, at = seq(0, 12765, 750),
     labels = seq(1990, 2024, 2))

legend("topright", inset = .02, title="Timeframe",
       c("1990-2024", "2000-2024"), 
       fill=c("black", "#1496E1"), 
       cex=0.8, horiz = TRUE)

par(mfrow = c(1, 1))

# ##create death rate replacing na as 0
df$death.rate<-df$total.deaths.na.as.0/df$pop.exposed.local.else.adm1.else.national.100yr
df$emdeath.rate<-df$total.deaths.na.as.0/df$Total.Affected

##create file with death rate removing nas
filtered.df<-df%>% filter (!is.na(total.deaths.reported))

filtered.df$death.rate.local<-filtered.df$total.deaths.reported/filtered.df$pop.exposed.local.else.adm1.else.national.100yr

# Check for missing values
sum(is.na(df$death.rate))
sum(is.na(filtered.df$death.rate.local))

# # Identify outliers using boxplots
# ggplot(df, aes(y = total.deaths.na.as.0)) + geom_boxplot()
# ggplot(df, aes(y = pop.exposed.local.else.adm1.else.national.100yr)) + geom_boxplot()
# ggplot(filtered.df, aes(y = death.rate.local)) + geom_boxplot()
# 
# ##log of exposed and deaths
# df$Ln.exposed<- log(df$pop.exposed.local.else.adm1.else.national.100yr)
# filtered.df$Ln.exposed<- log(filtered.df$pop.exposed.local.else.adm1.else.national.100yr)
# ggplot(df, aes(y = Ln.exposed)) + geom_boxplot()
# ggplot(filtered.df, aes(y = Ln.exposed)) + geom_boxplot()
# 
# ##comparison of death rates by GNI
df.gni<-df%>% filter (!is.na(gni.group))
df.gni$Year <- df.gni$Start.Year - 1990

df.gnid<-df.gni%>% filter (!is.na(death.rate))
df.gnidr<-df.gnid%>% filter(death.rate<1)
# 
new.df.gnidr <- df.gnidr
new.df.gnidr$gni.group.baseline <- factor(new.df.gnidr$gni.group.baseline,
                                          levels = c("LI", "LMI", "UMI", "HI"))
new.df.gnidr$zeroDeath <- ifelse(new.df.gnidr$death.rate == 0, 1, 0)


new.df.gnidr$logDeathRate <- NA
new.df.gnidr$logDeathRate[new.df.gnidr$zeroDeath == 0] <- 
  log(new.df.gnidr$death.rate[new.df.gnidr$zeroDeath == 0])

new.df.gnidr$death.rate.shift <-
  new.df.gnidr$death.rate +
  min(new.df.gnidr$death.rate[new.df.gnidr$zeroDeath == 0])

new.df.gnidr$logDeathRate0s <- log(new.df.gnidr$death.rate.shift)


# NA as NA

ggbetweenstats(new.df.gnidr[new.df.gnidr$zeroDeath == 0, ], 
               gni.group.baseline, 
               death.rate, 
               outlier.tagging = TRUE)

meanLabel1 <- expression(widehat(mu)[death.rate] == 0.001)
meanLabel2 <- expression(widehat(mu)[death.rate] == 0.003)
meanLabel3 <- expression(widehat(mu)[death.rate] == 0.002)
meanLabel4 <- expression(widehat(mu)[death.rate] == 0.003)


ggbetweenstats(new.df.gnidr[new.df.gnidr$zeroDeath == 0, ], 
               gni.group.baseline, 
               logDeathRate, 
               outlier.tagging = TRUE) +
  annotate("text", x = 1.4, y = -10, parse = TRUE, label = as.character(meanLabel1),
           size = 3, color = 'red') +
  annotate("text", x = 2.4, y = -9, parse = TRUE, label = as.character(meanLabel2),
           size = 3, color = 'red') +
  annotate("text", x = 3.4, y = -7.45, parse = TRUE, label = as.character(meanLabel3),
           size = 3, color = 'red') +
  annotate("text", x = 4.4, y = -9.55, parse = TRUE, label = as.character(meanLabel4),
           size = 3, color = 'red') +
  xlab("GNI Group (Baseline)") + ylab("log(Death Rate)")

# NA as min(nonzero death.rate)

ggbetweenstats(new.df.gnidr, 
               gni.group.baseline, 
               death.rate, 
               outlier.tagging = TRUE)

ggbetweenstats(new.df.gnidr, 
               gni.group.baseline, 
               logDeathRate0s, 
               outlier.tagging = TRUE) +
  annotate("text", x = 1.4, y = -10.85, parse = TRUE, label = as.character(meanLabel1),
           size = 3, color = 'red') +
  annotate("text", x = 2.4, y = -10.5, parse = TRUE, label = as.character(meanLabel2),
           size = 3, color = 'red') +
  annotate("text", x = 3.4, y = -9.9, parse = TRUE, label = as.character(meanLabel3),
           size = 3, color = 'red') +
  annotate("text", x = 4.4, y = -11.35, parse = TRUE, label = as.character(meanLabel4),
           size = 3, color = 'red') +
  xlab("GNI Group (Baseline)") + ylab("log(Death Rate)")

###Prediction ####

baseSet <- df.gni
baseSet$expPop10 <- baseSet$pop.exposed.local.else.adm1.else.national.10yr
baseSet$expPop100 <- baseSet$pop.exposed.local.else.adm1.else.national.100yr
baseSet$expPop1000 <- baseSet$pop.exposed.local.else.adm1.else.national.1000yr
baseSet$expPop10000 <- baseSet$pop.exposed.local.else.adm1.else.national.10000yr
baseSet$expPop100.adm0 <- baseSet$pop.exposed.adm0.100yr
baseSet$expPop100.adm1.only <- baseSet$pop.exposed.adm1.if.available.100yr


baseSet$spline_year <- bSpline(baseSet$Year, df = 3)

baseSet$gnipc.scale <- (baseSet$gnipc - mean(baseSet$gnipc, na.rm = TRUE)) / 
  sd(baseSet$gnipc, na.rm = TRUE)

baseKeepVars <- c("total.deaths.na.as.0",  # (main)
                  "total.deaths.reported", # for Model 4
                  "spline_year", 
                  "hdi",
                  "Declaration",
                  "no.events.5.years",
                  "pop.total.adm0",  
                  "Disaster.Subtype",
                  "DisNo.nocoun",
                  "SLIIDERS.country.code.flat",
                  "Start.Year",
                  
                  "expPop10",            # for Model 5
                  "expPop100",           # (main)
                  "expPop1000",          # for Model 6
                  "expPop10000",         # for Model 7
                  "expPop100.adm0",      # for Model 1
                  "expPop100.adm1.only") # for Model 2
                  
baseKeep <- data.frame(baseSet[, names(baseSet) %in% baseKeepVars])
baseKeep <- subset(baseKeep, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)

names(baseKeep)[which(names(baseKeep)==
                  "SLIIDERS.country.code.flat")] = 
  "Country_Code"

baseKeep$log_expPop100.adm0  <- log(baseKeep$expPop100.adm0)
baseKeep$log_expPop100       <- log(baseKeep$expPop100)
baseKeep$log_expPop100.adm1  <- log(baseKeep$expPop100.adm1.only)
baseKeep$log_expPop10        <- log(baseKeep$expPop10)
baseKeep$log_expPop1000      <- log(baseKeep$expPop1000)
baseKeep$log_expPop10000     <- log(baseKeep$expPop10000)


predMod.3 <- glmmTMB(total.deaths.na.as.0 ~
                     offset(log_expPop100) + log(pop.total.adm0) +
                     + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                     + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.1 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop100.adm0) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.2 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log(expPop100.adm1.only)) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.4 <- glmmTMB(total.deaths.reported ~
                       offset(log_expPop100) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.5 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop10) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.6 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop1000) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

predMod.7 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop10000) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype + 
                       + (1 | Country_Code), data = baseKeep, family = "nbinom2")

baseKeep.since.2000 <-
  baseKeep[baseKeep$Start.Year>=2000,]
baseKeep.since.2000$Year <- baseKeep.since.2000$Start.Year - 2000
baseKeep.since.2000$spline_year <- splines::bs(baseKeep.since.2000$Year, df = 3)


predMod.8 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop100) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype +
                       + (1 | Country_Code),
                     data = baseKeep.since.2000, family = "nbinom2")


# =============================================================================
# MODEL 9: Tropical cyclone-only subset
# (compare against U.S.-focused literature per reviewer suggestion)
# =============================================================================

baseSet.tc <- baseSet[baseSet$Disaster.Subtype == "Tropical cyclone", ]
baseSet.tc$spline_year <- bSpline(baseSet.tc$Year, df = 3)

baseKeep.tc <- data.frame(baseSet.tc[, names(baseSet.tc) %in% baseKeepVars])
baseKeep.tc <- subset(baseKeep.tc, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)
names(baseKeep.tc)[which(names(baseKeep.tc) == "SLIIDERS.country.code.flat")] <- "Country_Code"
baseKeep.tc$log_expPop100 <- log(baseKeep.tc$expPop100)

# NOTE: Disaster.Subtype is dropped from the formula here -- within a
# tropical-cyclone-only subset it is constant (no variance), so it cannot
# be included as a covariate. This is a structural necessity of the
# subsetting, not a simplification choice (matches every other model's
# formula otherwise).
predMod.9 <- glmmTMB(total.deaths.na.as.0 ~
                       offset(log_expPop100) + log(pop.total.adm0) +
                       + spline_year + hdi + Declaration + no.events.5.years +
                       + (1 | Country_Code), data = baseKeep.tc, family = "nbinom2")

# =============================================================================
# MODEL 10: Jonkman et al. (2024) coastal flood definition subset
# (per Reviewer #3)
# =============================================================================

baseSet.jonkman <- baseSet[
  baseSet$Disaster.Group == "Natural" &
    baseSet$Disaster.Subgroup %in% c("Hydrological", "Meteorological") &
    baseSet$Disaster.Subtype %in% c("Coastal flood", "Storm surge"),
]
baseSet.jonkman$spline_year <- bSpline(baseSet.jonkman$Year, df = 3)

baseKeep.jonkman <- data.frame(baseSet.jonkman[, names(baseSet.jonkman) %in% baseKeepVars])
baseKeep.jonkman <- subset(baseKeep.jonkman, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)
names(baseKeep.jonkman)[which(names(baseKeep.jonkman) == "SLIIDERS.country.code.flat")] <- "Country_Code"
baseKeep.jonkman$log_expPop100 <- log(baseKeep.jonkman$expPop100)

# Disaster.Subtype is included as a covariate in this model specification.

predMod.10 <- glmmTMB(total.deaths.na.as.0 ~
                        offset(log_expPop100) + log(pop.total.adm0) +
                        + spline_year + hdi + Declaration + no.events.5.years + Disaster.Subtype +
                        + (1 | Country_Code), data = baseKeep.jonkman, family = "nbinom2")


# GET AVERAGE COUNTRY DEATH RATE
# 
# names(trendData)[names(trendData) == "pop.total.adm0"] <- "pop_total_adm0"
# names(trendData)[names(trendData) == "pop.exposed.local.else.adm1.else.national.100yr"] <- "expPop"
# names(trendData)[names(trendData) == "no.events.5.years"] <- "no_events_5_years"
# names(trendData)[names(trendData) == "total.deaths.na.as.0"] <- "total_deaths_na_as_0"

# trendData$declare <- trendData$Declaration

trendData$spline_year <- bSpline(trendData$Start.Year - 1990, df = 3)
trendData$DisNo_nocoun <- factor(gsub("[A-Za-z]", "", trendData$DisNo.))
trendData$Country_Code <- factor(trendData$SLIIDERS.country.code.flat)
trendData$expPop100 <- trendData$pop.exposed.local.else.adm1.else.national.100yr

trendData$log_expPop100 <- log(trendData$expPop100)

trendData$gnipc.scale <- (trendData$gnipc - mean(trendData$gnipc, na.rm = TRUE)) / 
  sd(trendData$gnipc, na.rm = TRUE)

countryDeaths <- NULL

for (i in 1:length(unique(trendData$Country_Code))) {
  current <- trendData[trendData$Country_Code == unique(trendData$Country_Code)[i], ]
  
  
  newRow <- data.frame(Country_Code = current$Country_Code[1],
                       yearlyDeathRate = NA, yearlyExposed=NA,yearlyEvents=NA)
  
  thisPred <- predict(predMod.3, newdata = current, type = "response", allow.new.levels = TRUE)
  
  thisPred <- thisPred[!is.na(thisPred)]
  
  newRow$yearlyDeathRate <- (mean(thisPred)*nrow(current)) / length(unique(current$Start.Year))
  newRow$yearlyExposed <- mean(current$expPop100)/length(unique(current$Start.Year))
  newRow$yearlyEvents <- nrow(current)/(max(trendData$Start.Year,na.rm=T)-
                                          min(trendData$Start.Year,na.rm=T)+1)
    
  countryDeaths <- rbind(countryDeaths, newRow)
  
}

  countryDeaths <- countryDeaths[order(countryDeaths$yearlyDeathRate), ]

write.csv(countryDeaths, paste("Intermediate outputs/Output from Step 8/Country tables/countryYearlyDeaths_", YEAR_THRESHOLD, 
                               " ", date_append_output,".csv", sep = ""), 
          row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (1990-2024)

countryDeathCIs <- NULL

set.seed(256)

for (i in 1:length(unique(trendData$Country_Code))) {
  
  current <- trendData[trendData$Country_Code == unique(trendData$Country_Code)[i], ]
  
  cat("\014")
  print(paste("ITERATION", i, "OF", length(unique(trendData$Country_Code)),
              "(1990-2024)"))
  
  current <- current[, names(current) %in% c("total.deaths.na.as.0",
                                             "expPop100",
                                             "pop.total.adm0",
                                             "spline_year",
                                             "hdi",
                                             "Declaration",
                                             "no.events.5.years",
                                             "Disaster.Subtype",
                                             "Country_Code",
                                             "Start.Year")]
  
  
  current$log_expPop100 = log(current$expPop100)
  newRow <- data.frame(Country_Code = current$Country_Code[1],
                       nEvents = NA,
                       meanDeaths = NA,
                       meanLogDeaths = NA,
                       meanRate = NA,
                       mdsd = NA,
                       mldsd = NA,
                       mrsd = NA,
                       md_lcl = NA,
                       md_ucl = NA,
                       mld_lcl = NA,
                       mld_ucl = NA,
                       mr_lcl = NA,
                       mr_ucl = NA)
  
  thisPred <- predict(predMod.3, newdata = current, type = "response", allow.new.levels = TRUE)
  
  thisPred <- thisPred[!is.na(thisPred)]

  newRow$meanRate <- mean(thisPred / current$expPop100)
  newRow$meanDeaths <- mean(thisPred)
  newRow$meanLogDeaths <- mean(log(thisPred))
  newRow$nEvents <- length(thisPred)
  
  if (nrow(current) > 1) {
    
    preds <- NULL
    logPreds <- NULL
    rates <- NULL
    
    for (b in 1:B) {
      
      if (b %% 10 == 0) {
        cat("\014")
        print(paste("ITERATION", i, "OF", length(unique(trendData$Country_Code)),
                    "(1990-2024)"))
        print(paste("BOOT", b, "OF", B))
      }
      
      # Block bootstrap: resample YEARS with replacement, not individual
      # events. All events within a drawn year move together as one
      # block, preserving any same-year temporal clustering (e.g. a
      # single storm season generating multiple records) instead of
      # treating every event as exchangeable regardless of when it
      # occurred. This relaxes the assumption that error terms are
      # independent across time.
      unique_years_boot <- unique(current$Start.Year)
      sampled_years_boot <- sample(unique_years_boot, length(unique_years_boot), replace = TRUE)
      bootSamp <- do.call(rbind, lapply(sampled_years_boot, function(yr) current[current$Start.Year == yr, ]))
      bootPred <- predict(predMod.3, newdata = bootSamp, type = "response", allow.new.levels = TRUE)
      preds <- c(preds, mean(bootPred, na.rm = TRUE))
      logPreds <- c(logPreds, mean(log(bootPred), na.rm = TRUE))
      rates <- c(rates, mean(bootPred/bootSamp$expPop100, na.rm = TRUE))
    }
    
    newRow$mdsd <- sd(preds)
    newRow$md_lcl <- newRow$meanDeaths - qnorm(0.975)*newRow$mdsd
    newRow$md_ucl <- newRow$meanDeaths + qnorm(0.975)*newRow$mdsd
    
    newRow$mldsd <- sd(logPreds)
    newRow$mld_lcl <- newRow$meanLogDeaths - qnorm(0.975)*newRow$mldsd
    newRow$mld_ucl <- newRow$meanLogDeaths + qnorm(0.975)*newRow$mldsd
    
    newRow$mrsd <- sd(rates)
    newRow$mr_lcl <- newRow$meanRate - qnorm(0.975)*newRow$mrsd
    newRow$mr_ucl <- newRow$meanRate + qnorm(0.975)*newRow$mrsd
  }
  
  countryDeathCIs <- rbind(countryDeathCIs, newRow)
  
}

write.csv(countryDeathCIs, paste(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ", date_append_output,".csv",sep=""), row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (2050)

trendData2050 <- trendData

predKeepNames <- names(trendData2050)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData2050))]
predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
predKeepNames <- predKeepNames[grepl("2050", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("popscale", predKeepNames)]
# predKeepNames <- predKeepNames[grepl("AR6_med_SSP2.45", predKeepNames)]
predKeepNames <- predKeepNames[grepl("SSP1.26", predKeepNames)|
                   grepl("SSP5.85", predKeepNames)|
                   grepl("SSP1.19", predKeepNames)|
                   grepl("SSP1.26", predKeepNames)|
                   grepl("SSP2.45", predKeepNames)|
                   grepl("SSP3.70", predKeepNames)|
                   grepl("SSP5.85", predKeepNames)]

keepVars <- c("total.deaths.na.as.0",
              "pop.total.adm0",
              "spline_year",
              "hdi",
              "Declaration",
              "no.events.5.years",
              "Disaster.Subtype",
              "Country_Code",
              predKeepNames)

trendData2050 <- trendData2050[, names(trendData2050) %in% keepVars]
# trendData2050 <- trendData2050[, c(1:3, 7:11, 4:6)]
trendData2050 <- trendData2050[, c(1:3, 25:29, 4:24)]

countryDeathCIs50 <- NULL
ci_base <- read.csv(paste(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",step8_date_append_output,".csv",sep=""))

set.seed(256)

for (i in 1:length(unique(trendData2050$Country_Code))) {
  
  # for (j in 9:11) {
  for (j in 9:ncol(trendData2050)) {
    
    cat("\014")
    print(paste("ITERATION", i, "OF", length(unique(trendData2050$Country_Code)),
                "(2050)"))
    
    current <- trendData2050[trendData2050$Country_Code == unique(trendData2050$Country_Code)[i], 
                             c(1:8, j)]
    
    
    names(current)[9] <- "expPop100"
    current$log_expPop100 <- log(current$expPop100)
    
    
    newRow <- data.frame(Country_Code = current$Country_Code[1],
                         nEvents = NA,
                         model = substr(names(trendData2050)[j], 1, 23),
                         meanDeaths = NA,
                         mdsd = NA,
                         lcl = NA,
                         ucl = NA,
                         pctInc = NA,
                         pctsd = NA,
                         pctlcl = NA,
                         pctucl = NA)
    
    thisPred <- predict(predMod.3, newdata = current, type = "response", allow.new.levels = TRUE)
    
    thisPred <- thisPred[!is.na(thisPred)]
    
    curBase <- ci_base[ci_base$Country_Code == unique(current$Country_Code)[1], ]
    
    newRow$meanDeaths <- mean(thisPred)
    newRow$pctInc <- 100*(mean(thisPred) - curBase$meanDeaths) / curBase$meanDeaths
    newRow$nEvents <- length(thisPred)
    
    if (nrow(current) > 1) {
      
      preds <- NULL
      pcts <- NULL
      dataList <- NULL
      
      cat("\014")
      print(paste("ITERATION", i, "OF", length(unique(trendData2050$Country_Code)),
                  "(2050)"))
      print(paste("MODEL", j-8, "OF", length(9:ncol(trendData2050))))
      
      for (b in 1:B) {
        dataList[[b]] <- current[sample(1:nrow(current), replace = TRUE), ]
      }
      
      preds <- lapply(dataList, function(x){mean(predict(predMod.3, 
                                                         newdata = x,
                                                         type = "response",
                                                         allow.new.levels = TRUE))})
      
      pcts <- 100 * (unlist(preds) - curBase$meanDeaths)/curBase$meanDeaths
      
      newRow$mdsd <- sd(unlist(preds))
      newRow$lcl <- newRow$meanDeaths - qnorm(0.975)*newRow$mdsd
      newRow$ucl <- newRow$meanDeaths + qnorm(0.975)*newRow$mdsd
      
      newRow$pctsd <- sd(pcts)
      newRow$pctlcl <- newRow$pctInc - qnorm(0.975)*newRow$pctsd
      newRow$pctucl <- newRow$pctInc + qnorm(0.975)*newRow$pctsd
      
    }
    
    countryDeathCIs50 <- rbind(countryDeathCIs50, newRow)
    
  }
  
  # write.csv(countryDeathCIs50, "CountryDeathCIs50_top20.csv", row.names = FALSE)
  # write.csv(countryDeathCIs50, 
  #           paste("CountryDeathCIs50 ", date_append_output,".csv",sep=""), 
  #           row.names = FALSE)
  
}

write.csv(countryDeathCIs50, 
          paste("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs50 ", date_append_output,".csv",sep=""), 
          row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (2100)

trendData2100 <- trendData

predKeepNames <- names(trendData2100)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData2100))]
predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
predKeepNames <- predKeepNames[grepl("2100", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("popscale", predKeepNames)]
# predKeepNames <- predKeepNames[grepl("AR6_med_SSP2.45", predKeepNames)]
predKeepNames <- predKeepNames[grepl("SSP1.26", predKeepNames)|
                                 grepl("SSP5.85", predKeepNames)|
                                 grepl("SSP1.19", predKeepNames)|
                                 grepl("SSP1.26", predKeepNames)|
                                 grepl("SSP2.45", predKeepNames)|
                                 grepl("SSP3.70", predKeepNames)|
                                 grepl("SSP5.85", predKeepNames)]
keepVars <- c("total.deaths.na.as.0",
              "pop.total.adm0",
              "spline_year",
              "hdi",
              "Declaration",
              "no.events.5.years",
              "Disaster.Subtype",
              "Country_Code",
              predKeepNames)

trendData2100 <- trendData2100[, names(trendData2100) %in% keepVars]
# trendData2100 <- trendData2100[, c(1:3, 7:11, 4:6)]
trendData2100 <- trendData2100[, c(1:3, 25:29, 4:24)]

countryDeathCIs100 <- NULL
ci_base <- 
  read.csv(paste("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",step8_date_append_output,".csv",sep=""))

set.seed(256)

for (i in 1:length(unique(trendData2100$Country_Code))) {
  
  for (j in 9:ncol(trendData2100)) {
    
    cat("\014")
    print(paste("ITERATION", i, "OF", length(unique(trendData2100$Country_Code)),
                "(2100)"))
    
    current <- trendData2100[trendData2100$Country_Code == unique(trendData2100$Country_Code)[i], 
                             c(1:8, j)]
    names(current)[9] <- "expPop100"
    current$log_expPop100 = log(current$expPop100)
    newRow <- data.frame(Country_Code = current$Country_Code[1],
                         nEvents = NA,
                         model = substr(names(trendData2100)[j], 1, 23),
                         meanDeaths = NA,
                         mdsd = NA,
                         lcl = NA,
                         ucl = NA,
                         pctInc = NA,
                         pctsd = NA,
                         pctlcl = NA,
                         pctucl = NA)
    
    thisPred <- predict(predMod.3, newdata = current, type = "response", allow.new.levels = TRUE)
    
    thisPred <- thisPred[!is.na(thisPred)]
    
    curBase <- ci_base[ci_base$Country_Code == unique(current$Country_Code)[1], ]
    
    newRow$meanDeaths <- mean(thisPred)
    newRow$pctInc <- 100*(newRow$meanDeaths - curBase$meanDeaths) / curBase$meanDeaths
    newRow$nEvents <- length(thisPred)
    
    if (nrow(current) > 1) {
      
      preds <- NULL
      pcts <- NULL
      dataList <- NULL
      
      cat("\014")
      print(paste("ITERATION", i, "OF", length(unique(trendData2100$Country_Code)),
                  "(2100)"))
      print(paste("MODEL", j-8, "OF", length(9:ncol(trendData2050))))
      
      for (b in 1:B) {
        dataList[[b]] <- current[sample(1:nrow(current), replace = TRUE), ]
      }
      
      preds <- lapply(dataList, function(x){mean(predict(predMod.3, 
                                                         newdata = x,
                                                         type = "response",
                                                         allow.new.levels = TRUE))})
      
      pcts <- 100 * (curBase$meanDeaths - unlist(preds))/curBase$meanDeaths
      
      newRow$mdsd <- sd(unlist(preds))
      newRow$lcl <- newRow$meanDeaths - qnorm(0.975)*newRow$mdsd
      newRow$ucl <- newRow$meanDeaths + qnorm(0.975)*newRow$mdsd
      
      newRow$pctsd <- sd(pcts)
      newRow$pctlcl <- newRow$pctInc - qnorm(0.975)*newRow$pctsd
      newRow$pctucl <- newRow$pctInc + qnorm(0.975)*newRow$pctsd
      
    }
    
    countryDeathCIs100 <- rbind(countryDeathCIs100, newRow)
    
  }
  
  # write.csv(countryDeathCIs100, "CountryDeathCIs100.csv", row.names = FALSE)
  
}

write.csv(countryDeathCIs100, 
          paste("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs100 ", date_append_output,".csv", sep=""),
                row.names = FALSE)

# BACK TO PREDICTION

predData <- read_excel(
  paste("Intermediate outputs/Output from Step 7/",
  "EMDAT events with exposed pops, HDI GNI and prev events ", 
  step7_date_append_output,
  ".xlsx",sep=""))

predData <- predData[predData$gni.group != "", ]

predData$death.rate <- predData$total.deaths.na.as.0 / 
  predData$pop.exposed.local.else.adm1.else.national.100yr
predData <- predData %>% 
  filter (!is.na(gni.group),
          pop.exposed.local.else.adm1.else.national.100yr>0,
          death.rate<1)

predData$Year <- as.numeric(substr(predData$DisNo., 1, 4)) - 1990
predData$spline_year <- bSpline(predData$Year, df = 3)
predData$GNI <- factor(predData$gni.group, levels = c("LI", "LMI", "UMI", "HI"))
predData$declare <- factor(predData$Declaration)
predData$Country_Code <- factor(predData$SLIIDERS.country.code.flat)
predData$DisNo.nocoun <- factor(gsub("[A-Za-z]", "", predData$DisNo.))
predData$gnipc.scale <- (predData$gnipc - mean(predData$gnipc, na.rm = TRUE)) /
  sd(predData$gnipc, na.rm = TRUE)

predData$no.events.5.years = NA

for(i in 1:nrow(predData)){
  
  if(predData$Start.Year[i]>=1990&
     predData$Start.Year[i]<=2024){
    
    predData$no.events.5.years[i] =
      
      sum((predData$Start.Year%in%
             (predData$Start.Year[i]-5):
             predData$Start.Year[i])&
            predData$ISO==predData$ISO[i])
    
  }
}

predData <- predData[!is.na(predData$hdi), ]

predKeepNames <- names(predData)[grepl("pop.exposed.local.else.adm1.else.national", names(predData))]
predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
predKeepNames <- predKeepNames[grepl(".17", predKeepNames) |
                                 grepl(".50", predKeepNames) |
                                 grepl(".83", predKeepNames)]
## Both the geometry-only ("SLR only") and popscale-adjusted ("SLR +
## popscale") columns are retained here, since both sets of forecasts
## below are produced from predKeep.
predKeepNames <- predKeepNames[substr(predKeepNames, 1, 3) == "AR6"]

keepVars <- c("total.deaths.na.as.0", 
              "spline_year", 
              "hdi",
              "Declaration",
              "no.events.5.years",
              "pop.total.adm0",
              "DisNo.nocoun",
              "Country_Code",
              "Disaster.Subtype",
              predKeepNames)

predKeep <- predData[, names(predData) %in% keepVars]

# =============================================================================
# Dual-treatment forecasts: "SLR only" (geometry-only, static 2019 population)
# vs. "SLR + popscale" (same SLR geometry, with SSP-consistent population
# growth applied) -- for the 7 canonical scenarios plus 2 decoupled bounding
# scenarios (BOUND_worst, BOUND_best).
#
# Column naming (confirmed against real Step 6 output):
#   SLR only:      AR6_{forcing}.{year}.{perc}.pop.exposed.local.else.adm1.else.national.100yr
#   SLR + popscale: <same> + ".popscale." + {popscale_suffix}
#
# BOUND_worst/BOUND_best use med-confidence forcing by default (see chat
# discussion); a low-confidence BOUND_worst variant also exists in the data
# (AR6_low_SSP5.85....popscale.BOUND_worst) if wanted later.
# =============================================================================

model_map <- list(
  list(label = "low_SSP1.26", forcing = "low_SSP1.26",  popscale_suffix = "SSP1"),
  list(label = "low_SSP5.85", forcing = "low_SSP5.85",  popscale_suffix = "SSP5"),
  list(label = "med_SSP1.19", forcing = "med_SSP1.19",  popscale_suffix = "SSP1"),
  list(label = "med_SSP1.26", forcing = "med_SSP1.26",  popscale_suffix = "SSP1"),
  list(label = "med_SSP2.45", forcing = "med_SSP2.45",  popscale_suffix = "SSP2"),
  list(label = "med_SSP3.70", forcing = "med_SSP3.70",  popscale_suffix = "SSP3"),
  list(label = "med_SSP5.85", forcing = "med_SSP5.85",  popscale_suffix = "SSP5"),
  list(label = "BOUND_worst", forcing = "med_SSP5.85",  popscale_suffix = "BOUND_worst"),
  list(label = "BOUND_worst_low", forcing = "low_SSP5.85",  popscale_suffix = "BOUND_worst"),
  list(label = "BOUND_best",  forcing = "med_SSP1.19",  popscale_suffix = "BOUND_best")
)

years       <- c(2050, 2100)
percentiles <- c("17", "50", "83")

final_dual <- data.frame()

for (m in model_map) {
  for (yr in years) {
    for (p in percentiles) {

      geom_col <- paste0("AR6_", m$forcing, ".", yr, ".", p,
                         ".pop.exposed.local.else.adm1.else.national.100yr")
      pop_col  <- paste0(geom_col, ".popscale.", m$popscale_suffix)

      ## Hard checks: fail loudly, not silently, if a column is missing.
      if (!(geom_col %in% names(predKeep))) {
        stop("SLR-only column not found for model '", m$label, "', year ", yr,
             ", percentile ", p, ": ", geom_col)
      }
      if (!(pop_col %in% names(predKeep))) {
        stop("SLR+popscale column not found for model '", m$label, "', year ", yr,
             ", percentile ", p, ": ", pop_col)
      }

      for (treatment in c("SLR only", "SLR + popscale")) {

        col_use <- if (treatment == "SLR only") geom_col else pop_col

        feedIn <- predKeep
        feedIn$log_expPop100 <- log(feedIn[[col_use]])

        thisPred <- predict(predMod.3, newdata = feedIn, type = "response",
                            allow.new.levels = TRUE)

        final_dual <- rbind(final_dual, data.frame(
          model     = m$label,
          treatment = treatment,
          year      = yr,
          perc      = p,
          est       = mean(thisPred, na.rm = TRUE)
        ))
      }
    }
  }
  message("Completed model: ", m$label)
}

write.csv(final_dual,
          paste0("Intermediate outputs/Output from Step 8/Country tables/Dual_treatment_forecasts_",
                 date_append_output, ".csv"),
          row.names = FALSE)

message("Done. ", nrow(final_dual), " rows written (expected: ",
        length(model_map) * length(years) * length(percentiles) * 2, ").")

settings <- c("low_SSP1.26", # These are for SLR-only calcs
              "low_SSP5.85",
              "med_SSP1.19",
              "med_SSP1.26",
              "med_SSP2.45",
              "med_SSP3.70",
              "med_SSP5.85")

final <- NULL
ROW <- 1

for (i in 1:length(settings)) {
  
  ## Explicitly exclude popscale columns here: predKeep now contains both
  ## geometry-only and popscale-adjusted columns (widened to support the
  ## new dual-treatment forecast code above), but this loop's substring
  ## matching was built around there being exactly one match per
  ## setting/year/percentile. Restoring the original static-population-only
  ## behavior here.
  thisModelVars <- names(predKeep)[grepl(settings[i], names(predKeep)) &
                                    !grepl("popscale", names(predKeep))]
  
  loopKeepVars <- c("total.deaths.na.as.0", 
                    "spline_year", 
                    "hdi",
                    "Declaration",
                    "no.events.5.years",
                    "pop.total.adm0",
                    "DisNo.nocoun",
                    "Country_Code",
                    "Disaster.Subtype",
                    thisModelVars)
  
  current <- predKeep[, names(predKeep) %in% loopKeepVars]
  
  for (j in c("2050", "2100")) {
    
    ## NOTE: the old "i != 8" special case (pre-dating the current
    ## 7-scenario AR6-only design) has been removed as dead/unreachable
    ## code. All settings now use the standard year/percentile-narrowing
    ## path below.
    thisYearVars <- names(current)[grepl(j, names(current))]
    innerLoopVars <- c("total.deaths.na.as.0", 
                       "spline_year", 
                       "hdi",
                       "declare",
                       "no.events.5.years",
                       "pop.total.adm0",
                       "DisNo.nocoun",
                       "Country_Code",
                       "Disaster.Subtype",
                       thisYearVars)
    
    thisYear <- current[, names(current) %in% innerLoopVars]
    
    for (k in c(".17.pop", ".50.pop", ".83.pop")) {
      
      thisPercVars <- names(thisYear)[grepl(k, names(thisYear))]
      centerLoopVars <- c("total.deaths.na.as.0", 
                          "spline_year", 
                          "hdi",
                          "declare",
                          "no.events.5.years",
                          "pop.total.adm0",
                          "DisNo.nocoun",
                          "Country_Code",
                          "Disaster.Subtype",
                          thisPercVars)
      
      thisPerc <- thisYear[, names(thisYear) %in% centerLoopVars]
      expVar <- which(grepl("pop.exposed", names(thisPerc)))

      ## Safety check: if narrowing by setting/year/percentile matches more
      ## than one exposure column, stop loudly rather than silently picking
      ## the wrong one (guards against future column-naming collisions).
      if (length(expVar) > 1) {
        stop("Ambiguous match for setting '", settings[i], "', year ", j,
             ", percentile '", k, "': ", length(expVar),
             " exposure columns matched (expected exactly 1): ",
             paste(names(thisPerc)[expVar], collapse = ", "))
      }
      if (length(expVar) == 0) {
        stop("No exposure column matched for setting '", settings[i],
             "', year ", j, ", percentile '", k, "'.")
      }

      names(thisPerc)[expVar] <- "expPop"
      
      feedIn <- baseKeep
      
      # feedIn$expPop <- thisPerc$expPop
      feedIn$log_expPop100 <- log(thisPerc$expPop)
      
      
      thisPred <- predict(predMod.3, newdata = feedIn, type = "response", allow.new.levels = TRUE)
      
      addRow <- data.frame(model = settings[i],
                           year = j,
                           perc = substr(k, 2, 3),
                           est = mean(thisPred))
      
      final <- rbind(addRow, final)
      
    }
  }
}

# NOW GET PREDICTIONS PER-COUNTRY

countrySet <- predData

keepVars <- c("total.deaths.na.as.0", 
              "spline_year", 
              "hdi",
              "Declaration",
              "no.events.5.years",
              "pop.total.adm0",
              "DisNo.nocoun",
              "Country_Code",
              "Disaster.Subtype",
              "AR6_med_SSP5.85.2100.17.pop.exposed.local.else.adm1.else.national.100yr",
              "AR6_med_SSP5.85.2100.50.pop.exposed.local.else.adm1.else.national.100yr",
              "AR6_med_SSP5.85.2100.83.pop.exposed.local.else.adm1.else.national.100yr")

countryKeep <- countrySet[, names(countrySet) %in% keepVars]

countryRes17 <- NULL
countryRes50 <- NULL
countryRes83 <- NULL

for (i in 1:length(unique(countryKeep$Country_Code))) {
  
  if (i %% 10 == 0) {
    cat("\014")
    print(paste("ITERATION", i, "OF", length(unique(countryKeep$Country_Code))))
  }
  
  current <- countryKeep[countryKeep$Country_Code == unique(countryKeep$Country_Code)[i], ]
  
  cur17 <- current[, c(-4, -5)]
  cur50 <- current[, c(-3, -5)]
  cur83 <- current[, c(-3, -4)]
  
  names(cur17) <- c("Disaster.Subtype", "Declaration", "total.deaths.na.as.0", "expPop100", "hdi",
                    "pop.total.adm0", "spline_year", "Country_Code",
                    "DisNo_nocoun", "no.events.5.years")
  names(cur50) <- names(cur17) 
  names(cur83) <- names(cur17) 
  cur17$log_expPop100 = log(cur17$expPop100)
  cur50$log_expPop100 = log(cur50$expPop100)
  cur83$log_expPop100 = log(cur83$expPop100)
  
  # 17% Prediction
  pred17 <- predict(predMod.3, newdata = cur17, type = "response", allow.new.levels = TRUE)
  
  # 50% Prediction
  pred50 <- predict(predMod.3, newdata = cur50, type = "response", allow.new.levels = TRUE)
  
  # 83% Prediction
  pred83 <- predict(predMod.3, newdata = cur83, type = "response", allow.new.levels = TRUE)
  
  countryRes17 <- rbind(countryRes17,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred17/cur17$expPop100)))
  countryRes50 <- rbind(countryRes50,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred50/cur50$expPop100)))
  countryRes83 <- rbind(countryRes83,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred83/cur83$expPop100)))
  
}


write.csv(countryRes17, paste("Intermediate outputs/Output from Step 8/Country tables/meanDeathRate17 ",
                              date_append_output,".csv",sep=""), row.names = FALSE)

write.csv(countryRes50, paste("Intermediate outputs/Output from Step 8/Country tables/meanDeathRate50 ",
                              date_append_output,".csv",sep=""), row.names = FALSE)

write.csv(countryRes83, paste("Intermediate outputs/Output from Step 8/Country tables/meanDeathRate83 ",
                              date_append_output,".csv",sep=""), row.names = FALSE)


# NOW GET 1990-2024 (MEAN)

obsFinal <- NULL
BOOT <- B

set.seed(256)

for (i in 1:length(unique(df.gni$Year))) {
  
  cat("\014")
  print(paste("ITERATION", i, "OF", length(unique(df.gni$Year))))
  
  current <- predData[predData$Year == unique(predData$Year)[i], ]
  current <- current[!is.na(current$gnipc.scale), ]
  
  current$expPop100 <- current$pop.exposed.local.else.adm1.else.national.100yr
  # names(current)[names(current) == "no.events.5.years"] <- "no_events_5_years"
  # names(current)[names(current) == "pop.total.adm0"] <- "pop_total_adm0"
  current$log_expPop100 <- log(current$expPop100)
  
  meanPred <- mean(predict(predMod.3, newdata = current, type = "response", allow.new.levels = TRUE)/current$expPop100)
  
  bootRes <- NULL
  
  for (j in 1:BOOT) {
    
    if (j %% 10 == 0) {
      cat("\014")
      print(paste("ITERATION", i, "OF", length(unique(df.gni$Year))))
      print(paste("BOOTSTRAP", j, "OF", BOOT))
    }
    
    thisSamp <- current[sample(1:nrow(current), replace = TRUE), ]
    bootRes[j] <- mean(predict(predMod.3, newdata = thisSamp, type = "response", allow.new.levels = TRUE)/current$expPop100)
    
  }
  
  obsFinal <- rbind(obsFinal, 
                    data.frame(model = "observed",
                               year = current$Year[i],
                               perc = 17,
                               est = meanPred- qnorm(0.83)*sd(bootRes)),
                    data.frame(model = "observed",
                               year = current$Year[i],
                               perc = "PE",
                               est = meanPred),
                    data.frame(model = "observed",
                               year = current$Year[i],
                               perc = 83,
                               est = meanPred + qnorm(0.83)*sd(bootRes)))
  
}

final$year <- as.numeric(final$year) - 1990

plotData <- rbind(obsFinal, final)

# SAVE RESULTS (for quick load after)


for (i in 1:10) {
  eval(parse(text = paste0(
    "saveRDS(predMod.", i,
    ", file = 'Intermediate outputs/Output from Step 8/predMod.", i, " ",
    date_append_output, ".RDS')"
  )))
}


saveRDS(predData, paste("Intermediate outputs/Output from Step 8/predData ",
                        date_append_output,".RDS",sep=""))

saveRDS(final, paste("Intermediate outputs/Output from Step 8/final_HDI ",
                     date_append_output,".RDS",sep=""))

saveRDS(obsFinal, paste("Intermediate outputs/Output from Step 8/obsFinal_HDI ",
                        date_append_output,".RDS",sep=""))

saveRDS(plotData, paste("Intermediate outputs/Output from Step 8/plotData_HDI ",
                        date_append_output,".RDS",sep=""))

saveRDS(countryRes50, paste("Intermediate outputs/Output from Step 8/meanDeathRate_HDI ",
                            date_append_output,".RDS",sep=""))


saveRDS(trendData, paste("Intermediate outputs/Output from Step 8/trendData ",
                            date_append_output,".RDS",sep=""))

# QUICK LOAD ======================================================================================

# NOTE - If script has already been run once and the above .RDS files have been
# created, one can start from here to proceed with plot generation.

library(dplyr)
rm(list=ls())
# EDIT THIS to the same project root as above.
setwd("~/coastal_flood_mortality")

#
step7_date_append_output = "2026-07-05"
step8_date_append_output = "2026-07-20" # to identify relevant input files in
                                        # Intermediate outputs/Output from Step 8
                                        # if they exist for "quick load" functionality

date_append_output = "2026-07-20"       # for files created here
# 
# 
emdat.data.filtered <- readxl::read_excel(
  paste0("Intermediate outputs/Output from Step 7/EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
)

# 2) Rebuild the pieces used to construct baseSet / baseKeep
df <- emdat.data.filtered %>%
  dplyr::mutate(DisNo.nocoun = gsub("[A-Za-z]", "", DisNo.))

df$Year <- df$Start.Year - 1990
df$no.events.5.years <- NA_integer_
for(i in seq_len(nrow(df))){
  if(df$Start.Year[i] >= 1990 && df$Start.Year[i] <= 2024){
    df$no.events.5.years[i] <- sum(df$Start.Year %in% (df$Start.Year[i]-5):df$Start.Year[i] &
                                     df$ISO == df$ISO[i])
  }
}

df.gni <- df %>% dplyr::filter(!is.na(gni.group))

baseSet <- df.gni
baseSet$expPop10             <- baseSet$pop.exposed.local.else.adm1.else.national.10yr
baseSet$expPop100            <- baseSet$pop.exposed.local.else.adm1.else.national.100yr
baseSet$expPop1000           <- baseSet$pop.exposed.local.else.adm1.else.national.1000yr
baseSet$expPop10000          <- baseSet$pop.exposed.local.else.adm1.else.national.10000yr
baseSet$expPop100.adm0       <- baseSet$pop.exposed.adm0.100yr
baseSet$expPop100.adm1.only  <- baseSet$pop.exposed.adm1.if.available.100yr
baseSet$spline_year          <- splines2::bSpline(baseSet$Year, df = 3)

baseKeepVars <- c("total.deaths.na.as.0", "total.deaths.reported",
                  "spline_year", "hdi", "Declaration", "no.events.5.years",
                  "pop.total.adm0", "Disaster.Subtype", "DisNo.nocoun",
                  "SLIIDERS.country.code.flat",
                  "expPop10","expPop100","expPop1000","expPop10000",
                  "expPop100.adm0","expPop100.adm1.only","Start.Year")

baseKeep <- data.frame(baseSet[, names(baseSet) %in% baseKeepVars])
names(baseKeep)[names(baseKeep) == "SLIIDERS.country.code.flat"] <- "Country_Code"

# Add the log exposure columns EXPECTED by the 8 models
baseKeep$log_expPop100.adm0 <- log(baseKeep$expPop100.adm0)
baseKeep$log_expPop100      <- log(baseKeep$expPop100)
baseKeep$log_expPop100.adm1 <- log(baseKeep$expPop100.adm1.only)
baseKeep$log_expPop10       <- log(baseKeep$expPop10)
baseKeep$log_expPop1000     <- log(baseKeep$expPop1000)
baseKeep$log_expPop10000    <- log(baseKeep$expPop10000)

# Optional hygiene: restrict to rows used in fitting
baseKeep <- subset(baseKeep, !is.na(hdi) & is.finite(pop.total.adm0))

baseKeep.since.2000 <- baseKeep[baseKeep$Start.Year>=2000,]

for (i in 1:10) {
  eval(parse(text = paste0(
    "predMod.",i," = readRDS('Intermediate outputs/Output from Step 8/predMod.", i, " ",
    step8_date_append_output, ".RDS')"
  )))
}


final <- readRDS(paste("Intermediate outputs/Output from Step 8/final_HDI ",
                       step8_date_append_output,".RDS",sep=""))

obsFinal <- readRDS(paste("Intermediate outputs/Output from Step 8/obsFinal_HDI ",
                          step8_date_append_output,".RDS",sep=""))

plotData <- readRDS(paste("Intermediate outputs/Output from Step 8/plotData_HDI ",
                          step8_date_append_output,".RDS",sep=""))

predData <- readRDS(paste("Intermediate outputs/Output from Step 8/predData ",
                          step8_date_append_output,".RDS",sep=""))

meanDeathRate <- readRDS(paste("Intermediate outputs/Output from Step 8/meanDeathRate_HDI ",
                               step8_date_append_output,".RDS",sep=""))

trendData <- readRDS(paste("Intermediate outputs/Output from Step 8/trendData ",
                               step8_date_append_output,".RDS",sep=""))



# SETUP & PARAMETERS ==============================================================================

# !!! THE CURRENT WORKING DIRECTORY SHOULD BE THE STEP10 FOLDER !!!

# The YEAR_THRESHOLD parameter indicates the first year for which events should be 
# included in the analysis. The primary results use 1990; however, we also considered
# the year 2000. Some portions of the script are hard-coded to produce results for
# both 1990 and 2000.

YEAR_THRESHOLD <- 1990

# The B parameter indicates the number of iterations to utilize when performing 
# bootstrapping. NOTE - Runtime will be quite long (>24 hours) when using values
# above 100. If simply testing out the script, use a small number, like B <- 5.

B <- 1000

# BEGIN SCRIPT ====================================================================================

setwd("../Step07")

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
library(rWCVPdata)

####Importing data
df <- read_excel("7b output 7.xlsx")

#create column with disaster number only, removing country code, to get an event specific variable
df <- df %>%
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

setwd("../Step10")

hemi <- read.csv("countryList.csv")
names(hemi)[1] <- "Country"

trendData <- merge(trendData, hemi, by = "Country", all.x = TRUE)

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

##create death rate replacing na as 0
df$death.rate<-df$total.deaths.na.as.0/df$pop.exposed.local.else.adm1.else.national.100yr
df$emdeath.rate<-df$total.deaths.na.as.0/df$Total.Affected

##create file with death rate removing nas
filtered.df<-df%>% filter (!is.na(total.deaths.reported))

filtered.df$death.rate.local<-filtered.df$total.deaths.reported/filtered.df$pop.exposed.local.else.adm1.else.national.100yr

# Check for missing values
sum(is.na(df$death.rate))
sum(is.na(filtered.df$death.rate.local))

# Identify outliers using boxplots
ggplot(df, aes(y = total.deaths.na.as.0)) + geom_boxplot()
ggplot(df, aes(y = pop.exposed.local.else.adm1.else.national.100yr)) + geom_boxplot()
ggplot(filtered.df, aes(y = death.rate.local)) + geom_boxplot()

##log of exposed and deaths
df$Ln.exposed<- log(df$pop.exposed.local.else.adm1.else.national.100yr)
filtered.df$Ln.exposed<- log(filtered.df$pop.exposed.local.else.adm1.else.national.100yr)
ggplot(df, aes(y = Ln.exposed)) + geom_boxplot()
ggplot(filtered.df, aes(y = Ln.exposed)) + geom_boxplot()

##comparison of death rates by GNI
df.gni<-df%>% filter (!is.na(gni.group))
df.gni$Year <- df.gni$Start.Year - 1990

df.gnid<-df.gni%>% filter (!is.na(death.rate))
df.gnidr<-df.gnid%>% filter(death.rate<1)

new.df.gnidr <- df.gnidr
new.df.gnidr$gni.group.baseline <- factor(new.df.gnidr$gni.group.baseline,
                                          levels = c("LI", "LMI", "UMI", "HI"))
new.df.gnidr$zeroDeath <- ifelse(new.df.gnidr$death.rate == 0, 1, 0)
zeroTable <- table(new.df.gnidr$zeroDeath, new.df.gnidr$gni.group.baseline)
zeroTable <- zeroTable[2, ] / (zeroTable[1, ] + zeroTable[2, ])
barplot(zeroTable, col = c("darkseagreen2", "darkorange3", "mediumpurple1", "maroon1"),
        ylim = c(0, 0.35), xlab = "Income Category",
        ylab = "Proportion of Events with No Deaths")
lines(c(0.1, 4.9), c(0.05, 0.05), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.10, 0.10), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.15, 0.15), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.20, 0.20), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.25, 0.25), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.30, 0.30), lty = 2, col = "black")

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
baseSet$expPop <- baseSet$pop.exposed.local.else.adm1.else.national.100yr

baseSet$spline_year <- bSpline(baseSet$Year, df = 3)

baseSet$gnipc.scale <- (baseSet$gnipc - mean(baseSet$gnipc, na.rm = TRUE)) / 
  sd(baseSet$gnipc, na.rm = TRUE)

baseKeepVars <- c("total.deaths.na.as.0", 
                  "spline_year", 
                  "hdi",
                  "Declaration",
                  "no.events.5.years",
                  "pop.total.adm0",
                  "Disaster.Subtype",
                  "DisNo.nocoun",
                  "SLIIDERS.country.code.flat",
                  "expPop")

baseKeep <- data.frame(baseSet[, names(baseSet) %in% baseKeepVars])
baseKeep <- baseKeep[, c(3, 8, 6, 10, 5, 2, 1, 4, 7, 9)]
baseKeep <- baseKeep[!is.na(baseKeep$hdi), ]

names(baseKeep) <- c("total_deaths_na_as_0",
                     "no_events_5_years",
                     "pop_total_adm0",
                     "spline_year", "hdi", "declare", "Disaster.Subtype",
                     "Country_Code", "DisNo_nocoun", "expPop")

predMod <- glmmTMB(total_deaths_na_as_0 ~
                     offset(log(expPop)) + log(pop_total_adm0) +
                     + spline_year + hdi + declare + no_events_5_years + Disaster.Subtype + 
                     + (1 | Country_Code), data = baseKeep, family = "nbinom2")

# GET AVERAGE COUNTRY DEATH RATE

names(trendData)[names(trendData) == "pop.total.adm0"] <- "pop_total_adm0"
names(trendData)[names(trendData) == "pop.exposed.local.else.adm1.else.national.100yr"] <- "expPop"
names(trendData)[names(trendData) == "no.events.5.years"] <- "no_events_5_years"
names(trendData)[names(trendData) == "total.deaths.na.as.0"] <- "total_deaths_na_as_0"

trendData$declare <- trendData$Declaration

trendData$spline_year <- bSpline(trendData$Start.Year - 1990, df = 3)
trendData$DisNo_nocoun <- factor(gsub("[A-Za-z]", "", trendData$DisNo.))
trendData$Country_Code <- factor(trendData$SLIIDERS.country.code.flat)

trendData <- trendData[trendData$Country_Code %in% c("CHN", "PHL", "USA", "VNM", "IND",
                                                     "MDG", "TWN", "JPN", "BGD", "MEX"), ]

trendData$gnipc.scale <- (trendData$gnipc - mean(trendData$gnipc, na.rm = TRUE)) / 
  sd(trendData$gnipc, na.rm = TRUE)

countryDeaths <- NULL

for (i in 1:length(unique(trendData$Country_Code))) {
  current <- trendData[trendData$Country_Code == unique(trendData$Country_Code)[i], ]
  
  newRow <- data.frame(Country_Code = current$Country_Code[1],
                       yearlyDeathRate = NA)
  
  thisPred <- predict(predMod, newdata = current, type = "response")
  thisPred <- thisPred / current$expPop
  
  thisPred <- thisPred[!is.na(thisPred)]
  
  newRow$yearlyDeathRate <- (mean(thisPred)*nrow(current)) / length(unique(current$Start.Year))
  countryDeaths <- rbind(countryDeaths, newRow)
  
}

countryDeaths <- countryDeaths[order(countryDeaths$yearlyDeathRate), ]

setwd("./Output")

write.csv(countryDeaths, paste("countryYearlyDeaths_", YEAR_THRESHOLD, ".csv", sep = ""), 
          row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (1990-2024)

countryDeathCIs <- NULL

set.seed(256)

for (i in 1:length(unique(trendData$Country_Code))) {
  
  current <- trendData[trendData$Country_Code == unique(trendData$Country_Code)[i], ]
  
  cat("\014")
  print(paste("ITERATION", i, "OF", length(unique(trendData$Country_Code)),
              "(1990-2024)"))
  
  current <- current[, names(current) %in% c("total_deaths_na_as_0",
                                             "expPop",
                                             "pop_total_adm0",
                                             "spline_year",
                                             "hdi",
                                             "declare",
                                             "no_events_5_years",
                                             "Disaster.Subtype",
                                             "Country_Code")]
  
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
  
  thisPred <- predict(predMod, newdata = current, type = "response")
  
  thisPred <- thisPred[!is.na(thisPred)]
  
  newRow$meanRate <- mean(thisPred / current$expPop)
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
      
      if (nrow(current) >= 10) {
        thisSamp <- sample(1:(nrow(current)-4), 
                           size = floor(nrow(current)/5),
                           replace = TRUE)
        thisSamp <- c(thisSamp, thisSamp+1, thisSamp+2, thisSamp+3, thisSamp+4)
        bootSamp <- current[thisSamp, ]
      } else {
        bootSamp <- current[sample(1:nrow(current), replace = TRUE), ]
      }
      
      bootPred <- predict(predMod, newdata = bootSamp, type = "response")
      preds <- c(preds, mean(bootPred, na.rm = TRUE))
      logPreds <- c(logPreds, mean(log(bootPred), na.rm = TRUE))
      rates <- c(rates, mean(bootPred/bootSamp$expPop, na.rm = TRUE))
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

write.csv(countryDeathCIs, "CountryDeathCIs.csv", row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (2050)

trendData2050 <- trendData

predKeepNames <- names(trendData2050)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData2050))]
predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
predKeepNames <- predKeepNames[grepl("2050", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("popscale", predKeepNames)]
predKeepNames <- predKeepNames[grepl("AR6_med_SSP2.45", predKeepNames)]

keepVars <- c("total_deaths_na_as_0",
              "pop_total_adm0",
              "spline_year",
              "hdi",
              "declare",
              "no_events_5_years",
              "Disaster.Subtype",
              "Country_Code",
              predKeepNames)

trendData2050 <- trendData2050[, names(trendData2050) %in% keepVars]
trendData2050 <- trendData2050[, c(1:2, 6:11, 3:5)]

countryDeathCIs50 <- NULL
ci_base <- read.csv("CountryDeathCIs.csv")

set.seed(256)

for (i in 1:length(unique(trendData2050$Country_Code))) {
  
  for (j in 9:11) {
    
    cat("\014")
    print(paste("ITERATION", i, "OF", length(unique(trendData2050$Country_Code)),
                "(2050)"))
    
    current <- trendData2050[trendData2050$Country_Code == unique(trendData2050$Country_Code)[i], 
                             c(1:8, j)]
    names(current)[9] <- "expPop"
    
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
    
    thisPred <- predict(predMod, newdata = current, type = "response")
    
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
      print(paste("MODEL", j-8, "OF", length(9:11)))
      
      for (b in 1:B) {
        if (nrow(current) >= 10) {
          thisSamp <- sample(1:(nrow(current)-4), 
                             size = floor(nrow(current)/5),
                             replace = TRUE)
          thisSamp <- c(thisSamp, thisSamp+1, thisSamp+2, thisSamp+3, thisSamp+4)
          dataList[[b]] <- current[thisSamp, ]
        } else {
          dataList[[b]] <- current[sample(1:nrow(current), replace = TRUE), ]
        }
      }
      
      preds <- lapply(dataList, function(x){mean(predict(predMod, 
                                                         newdata = x,
                                                         type = "response"))})
      
      pcts <- 100 * (curBase$meanDeaths - unlist(preds))/curBase$meanDeaths
      
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
  write.csv(countryDeathCIs50, "CountryDeathCIs50.csv", row.names = FALSE)
  
}

write.csv(countryDeathCIs50, "CountryDeathCIs50.csv", row.names = FALSE)

# GET CIs FOR COUNTRY-LEVEL DEATHS (2100)

trendData2100 <- trendData

predKeepNames <- names(trendData2100)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData2100))]
predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
predKeepNames <- predKeepNames[grepl("2100", predKeepNames)]
predKeepNames <- predKeepNames[!grepl("popscale", predKeepNames)]
predKeepNames <- predKeepNames[grepl("AR6_med_SSP2.45", predKeepNames)]

keepVars <- c("total_deaths_na_as_0",
              "pop_total_adm0",
              "spline_year",
              "hdi",
              "declare",
              "no_events_5_years",
              "Disaster.Subtype",
              "Country_Code",
              predKeepNames)

trendData2100 <- trendData2100[, names(trendData2100) %in% keepVars]
trendData2100 <- trendData2100[, c(1:2, 6:11, 3:5)]

countryDeathCIs100 <- NULL
ci_base <- read.csv("CountryDeathCIs.csv")

set.seed(256)

for (i in 1:length(unique(trendData2100$Country_Code))) {
  
  for (j in 9:11) {
    
    cat("\014")
    print(paste("ITERATION", i, "OF", length(unique(trendData2100$Country_Code)),
                "(2100)"))
    
    current <- trendData2100[trendData2100$Country_Code == unique(trendData2100$Country_Code)[i], 
                             c(1:8, j)]
    names(current)[9] <- "expPop"
    
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
    
    thisPred <- predict(predMod, newdata = current, type = "response")
    
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
      print(paste("MODEL", j-8, "OF", length(9:11)))
      
      for (b in 1:B) {
        if (nrow(current) >= 10) {
          thisSamp <- sample(1:(nrow(current)-4), 
                             size = floor(nrow(current)/5),
                             replace = TRUE)
          thisSamp <- c(thisSamp, thisSamp+1, thisSamp+2, thisSamp+3, thisSamp+4)
          dataList[[b]] <- current[thisSamp, ]
        } else {
          dataList[[b]] <- current[sample(1:nrow(current), replace = TRUE), ]
        }
      }
      
      preds <- lapply(dataList, function(x){mean(predict(predMod, 
                                                         newdata = x,
                                                         type = "response"))})
      
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
  
  write.csv(countryDeathCIs100, "CountryDeathCIs100.csv", row.names = FALSE)
  
}

write.csv(countryDeathCIs100, "CountryDeathCIs100.csv", row.names = FALSE)

# BACK TO PREDICTION

setwd("../../Step07")
predData <- read_excel("7b output 7.xlsx")
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

# predData <- predData[predData$Country_Code %in% c("CHN", "PHL", "USA", "VNM", "IND",
#                                                   "MDG", "TWN", "JPN", "BGD", "MEX"), ]

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
predKeepNames <- predKeepNames[!grepl("popscale", predKeepNames)]
predKeepNames <- predKeepNames[substr(predKeepNames, 1, 3) == "AR6"]

keepVars <- c("total.deaths.na.as.0", 
              "spline_year", 
              "hdi",
              "declare",
              "no.events.5.years",
              "pop.total.adm0",
              "DisNo.nocoun",
              "Country_Code",
              "Disaster.Subtype",
              predKeepNames)

predKeep <- predData[, names(predData) %in% keepVars]

settings <- c("low_SSP1.26",
              "low_SSP5.85",
              "med_SSP1.19",
              "med_SSP1.26",
              "med_SSP2.45",
              "med_SSP3.70",
              "med_SSP5.85")

final <- NULL
ROW <- 1

for (i in 1:length(settings)) {
  
  thisModelVars <- names(predKeep)[grepl(settings[i], names(predKeep))]
  
  loopKeepVars <- c("total.deaths.na.as.0", 
                    "spline_year", 
                    "hdi",
                    "declare",
                    "no.events.5.years",
                    "pop.total.adm0",
                    "DisNo.nocoun",
                    "Country_Code",
                    "Disaster.Subtype",
                    thisModelVars)
  
  current <- predKeep[, names(predKeep) %in% loopKeepVars]
  
  for (j in c("2050", "2100")) {
    
    if (i != 8) {
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
    } else {
      innerLoopVars <- loopKeepVars
    }
    
    thisYear <- current[, names(current) %in% innerLoopVars]
    
    for (k in c(".17.pop", ".50.pop", ".83.pop")) {
      
      if (i != 8) {
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
      } else {
        centerLoopVars <- innerLoopVars
      }
      
      thisPerc <- thisYear[, names(thisYear) %in% centerLoopVars]
      expVar <- which(grepl("pop.exposed", names(thisPerc)))
      names(thisPerc)[expVar] <- "expPop"
      
      feedIn <- baseKeep
      
      feedIn$expPop <- thisPerc$expPop
      
      thisPred <- predict(predMod, newdata = feedIn, type = "response")
      
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
              "declare",
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
  
  names(cur17) <- c("Disaster.Subtype", "total_deaths_na_as_0", "expPop", "hdi",
                    "pop_total_adm0", "spline_year", "declare", "Country_Code",
                    "DisNo_nocoun", "no_events_5_years")
  names(cur50) <- c("Disaster.Subtype", "total_deaths_na_as_0", "expPop", "hdi",
                    "pop_total_adm0", "spline_year", "declare", "Country_Code",
                    "DisNo_nocoun", "no_events_5_years")
  names(cur83) <- c("Disaster.Subtype", "total_deaths_na_as_0", "expPop", "hdi",
                    "pop_total_adm0", "spline_year", "declare", "Country_Code",
                    "DisNo_nocoun", "no_events_5_years")
  
  # 17% Prediction
  pred17 <- predict(predMod, newdata = cur17, type = "response")
  
  # 50% Prediction
  pred50 <- predict(predMod, newdata = cur50, type = "response")
  
  # 83% Prediction
  pred83 <- predict(predMod, newdata = cur83, type = "response")
  
  countryRes17 <- rbind(countryRes17,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred17/cur17$expPop)))
  countryRes50 <- rbind(countryRes50,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred50/cur50$expPop)))
  countryRes83 <- rbind(countryRes83,
                        data.frame(Country_Code = current$Country_Code[1],
                                   death_rate = mean(pred83/cur83$expPop)))
  
}

setwd("../Step10/Output")
write.csv(countryRes17, "meanDeathRate17.csv", row.names = FALSE)
write.csv(countryRes50, "meanDeathRate50.csv", row.names = FALSE)
write.csv(countryRes83, "meanDeathRate83.csv", row.names = FALSE)

# NOW GET 1990-2024 (MEAN)

obsFinal <- NULL
BOOT <- B

set.seed(256)

for (i in 1:length(unique(df.gni$Year))) {
  
  cat("\014")
  print(paste("ITERATION", i, "OF", length(unique(df.gni$Year))))
  
  current <- predData[predData$Year == unique(predData$Year)[i], ]
  current <- current[!is.na(current$gnipc.scale), ]
  
  current$expPop <- current$pop.exposed.local.else.adm1.else.national.100yr
  names(current)[names(current) == "no.events.5.years"] <- "no_events_5_years"
  names(current)[names(current) == "pop.total.adm0"] <- "pop_total_adm0"
  
  meanPred <- mean(predict(predMod, newdata = current, type = "response")/current$expPop)
  
  bootRes <- NULL
  
  for (j in 1:BOOT) {
    
    if (j %% 10 == 0) {
      cat("\014")
      print(paste("ITERATION", i, "OF", length(unique(df.gni$Year))))
      print(paste("BOOTSTRAP", j, "OF", BOOT))
    }
    
    thisSamp <- current[sample(1:nrow(current), replace = TRUE), ]
    bootRes[j] <- mean(predict(predMod, newdata = thisSamp, type = "response")/current$expPop)
    
  }
  
  obsFinal <- rbind(obsFinal, 
                    data.frame(model = "observed",
                               year = current$Year[i],
                               perc = 17,
                               est = meanPred - qnorm(0.83)*sd(bootRes)),
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

# SAVE RESULTS

saveRDS(predMod, "predMod_HDI.RDS")
saveRDS(predData, "predData.RDS")

saveRDS(final, "final_HDI.RDS")
saveRDS(obsFinal, "obsFinal_HDI.RDS")
saveRDS(plotData, "plotData_HDI.RDS")

saveRDS(countryRes50, "meanDeathRate_HDI.RDS")

setwd("..")

# QUICK LOAD ======================================================================================

# NOTE - If script has already been run once and the above .RDS files have been
# created, one can start from here to proceed with plot generation.

# If starting from here, the current working directory will still need to be the
# Step10 folder.

setwd("./Output")

predMod <- readRDS("predMod_HDI.RDS")
predData <- readRDS("predData.RDS")

final <- readRDS("final_HDI.RDS")
obsFinal <- readRDS("obsFinal_HDI.RDS")
plotData <- readRDS("plotData_HDI.RDS")

meanDeathRate <- readRDS("meanDeathRate_HDI.RDS")

settings <- c("low_SSP1.26",
              "low_SSP5.85",
              "med_SSP1.19",
              "med_SSP1.26",
              "med_SSP2.45",
              "med_SSP3.70",
              "med_SSP5.85")

line17 <- obsFinal[obsFinal$model == "observed" & obsFinal$perc == 17, ]
linePE <- obsFinal[obsFinal$model == "observed" & obsFinal$perc == "PE", ]
line83 <- obsFinal[obsFinal$model == "observed" & obsFinal$perc == 83, ]

SPAN83 <- 0.25
SPAN <- 0.20

loess17 <- loess(line17$est ~ line17$year, span = SPAN)
smoothed17 <- predict(loess17) 

loessPE <- loess(linePE$est ~ linePE$year, span = SPAN)
smoothedPE <- predict(loessPE)

loess83 <- loess(line83$est ~ line83$year, span = SPAN83)
smoothed83 <- predict(loess83)

# par(mfrow = c(1, 2))

# plot(linePE$year, linePE$est, 
#      ylim = c(0, 0.05), xlim = c(0, 32), type = 'l',
#      xlab = "Year", ylab = "Average Death Rate", xaxt = 'n')
# 
# axis(1, at = seq(0, 32, 5),
#      labels = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))

model60 <- plotData[plotData$model != "observed" & plotData$year == 60, ]
model110 <- plotData[plotData$model != "observed" & plotData$year == 110, ]

colorList <- c("gray70", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")

par(mar = c(4.1, 4.1, 2.1, 2.1))

plot(-5, -5, xlim = c(35, 130), ylim = c(0, 300),
     xlab = "", xaxt = 'n', ylab = "Percentage Increase in Deaths per Event")

par(mar = c(5.1, 4.1, 4.1, 2.1))

text(60, 15, "Projections for 2050", font = 2)
text(110, 15, "Projections for 2100", font = 2)

predData <- predData[!is.na(predData$gnipc.scale), ]

predData$expPop <- predData$pop.exposed.local.else.adm1.else.national.100yr
names(predData)[names(predData) == "no.events.5.years"] <- "no_events_5_years"
names(predData)[names(predData) == "pop.total.adm0"] <- "pop_total_adm0"

aggDeath <- mean(predict(predMod, predData, type = "response"))

model60$pctChg <- 100*(model60$est - aggDeath) / aggDeath
model110$pctChg <- 100*(model110$est - aggDeath) / aggDeath

w <- 3
shift <- w*seq(-3, 3, 1)

for (i in 1:length(settings)) {
  
  current60 <- model60[model60$model == settings[i], ]
  current110 <- model110[model110$model == settings[i], ]
  
  points(60+shift[i], current60$pctChg[2], pch = 21, bg = colorList[i], col = "#ffffcc")
  lines(c(60+shift[i], 60+shift[i]), c(current60$pctChg[1], current60$pctChg[3]), col = colorList[i], lwd = 2)
  
  points(110+shift[i], current110$pctChg[2], pch = 21, bg = colorList[i], col = "#ffffcc")
  lines(c(110+shift[i], 110+shift[i]), c(current110$pctChg[1], current110$pctChg[3]), col = colorList[i], lwd = 2)
  
}

lines(c(37.5, 127.5), c(50, 50), lty = 3)
lines(c(37.5, 127.5), c(100, 100), lty = 3)
lines(c(37.5, 127.5), c(150, 150), lty = 3)
lines(c(37.5, 127.5), c(200, 200), lty = 3)
lines(c(37.5, 127.5), c(250, 250), lty = 3)

# par(mfrow = c(1, 1))

# MAKE FIGURE

ci_base <- read.csv("CountryDeathCIs.csv")
ci_50 <- read.csv("CountryDeathCIs50.csv")
ci_100 <- read.csv("CountryDeathCIs100.csv")

EVENT_THRESHOLD <- 50

keep_base <- ci_base[ci_base$nEvents >= EVENT_THRESHOLD, ]
keep_50 <- ci_50[ci_50$nEvents >= EVENT_THRESHOLD, ]
keep_100 <- ci_100[ci_100$nEvents >= EVENT_THRESHOLD, ]

par(mfrow = c(2, 2))

# TOP-LEFT

keep_base <- keep_base[order(keep_base$meanRate), ]

plot(-5, -5,
     xlim = c(1, nrow(keep_base)), xlab = "Country", xaxt = "n",
     ylim = c(0, 0.004), ylab = "Average per-Event Death Rate")

axis(1, at = 1:nrow(keep_base), labels = keep_base$Country_Code)

for (i in 1:nrow(keep_base)) {
  lines(c(i, i), c(max(0, keep_base$mr_lcl[i]), 
                   keep_base$mr_ucl[i]))
  lines(c(i-0.1, i+0.1), c(max(0, keep_base$mr_lcl[i]), max(0, keep_base$mr_lcl[i])))
  lines(c(i-0.1, i+0.1), c(keep_base$mr_ucl[i], keep_base$mr_ucl[i]))
  points(i, keep_base$meanRate[i], pch = 21, col = "black", bg = "#1496E1")
}

keep_base <- keep_base[order(keep_base$meanDeaths), ]

# TOP-RIGHT

plot(-5, -5,
     xlim = c(1, nrow(keep_base)), xlab = "Country", xaxt = "n",
     ylim = c(0, 12000), ylab = "Average Deaths per Event")

axis(1, at = 1:nrow(keep_base), labels = keep_base$Country_Code)

for (i in 1:nrow(keep_base)) {
  
  if (keep_base$Country_Code[i] == "BGD") {
    lines(c(i, i), c(max(0, keep_base$md_lcl[i]), 
                     keep_base$meanDeaths[i]))
    lines(c(i, i), c(max(0, keep_base$md_ucl[i]), 
                     keep_base$meanDeaths[i]), lty = 2)
    lines(c(i-0.1, i+0.1), c(max(0, keep_base$md_lcl[i]), max(0, keep_base$md_lcl[i])))
    lines(c(i-0.1, i+0.1), c(keep_base$md_ucl[i], keep_base$md_ucl[i]))
  } else {
    lines(c(i, i), c(max(0, keep_base$md_lcl[i]), 
                     keep_base$md_ucl[i]))
    lines(c(i-0.1, i+0.1), c(max(0, keep_base$md_lcl[i]), max(0, keep_base$md_lcl[i])))
    lines(c(i-0.1, i+0.1), c(keep_base$md_ucl[i], keep_base$md_ucl[i]))
  }
  
  points(i, keep_base$meanDeaths[i], pch = 21, col = "black", bg = "#1496E1")
}

# BOTTOM-LEFT

keep_50 <- keep_50[order(keep_50$pctInc), ]

plot(-5, -5,
     xlim = c(1, nrow(keep_base)), xlab = "Country", xaxt = "n",
     ylim = c(0, 300), ylab = "Percent Change in Average Deaths (2050)")

axis(1, at = 1:nrow(keep_base), labels = unique(keep_50$Country_Code))

for (i in 1:length(unique(keep_50$Country_Code))) {
  current <- keep_50[keep_50$Country_Code == unique(keep_50$Country_Code)[i], ]
  
  if (min(current$pctlcl) > 0 & max(current$pctucl) < 300) {
    lines(c(i, i), c(min(current$pctlcl), max(current$pctucl)))
    lines(c(i-0.1, i+0.1), c(min(current$pctlcl), min(current$pctlcl)))
    lines(c(i-0.1, i+0.1), c(max(current$pctucl), max(current$pctucl)))
  } else if (min(current$pctlcl) < 0 & max(current$pctucl) < 300) {
    lines(c(i, i), c(current$pctInc[1], max(current$pctucl)))
    lines(c(i, i), c(-50, current$pctInc[1]), lty = 2)
    lines(c(i-0.1, i+0.1), c(max(current$pctucl), max(current$pctucl)))
  } else if (min(current$pctlcl) < 0 & max(current$pctucl) > 300) {
    lines(c(i, i), c(current$pctInc[3], 350), lty = 2)
    lines(c(i, i), c(current$pctInc[1], current$pctInc[3]))
    lines(c(i, i), c(-50, current$pctInc[1]), lty = 2)
  }
  
  points(i, current$pctInc[1], pch = 21, col = "black", bg = "#A6D9F7")
  points(i, current$pctInc[2], pch = 21, col = "black", bg = "#1496E1")
  points(i, current$pctInc[3], pch = 21, col = "black", bg = "#052538")
}

# BOTTOM-RIGHT

keep_100 <- keep_100[order(keep_100$pctInc), ]

plot(-5, -5,
     xlim = c(1, nrow(keep_base)), xlab = "Country", xaxt = "n",
     ylim = c(0, 300), ylab = "Percent Change in Average Deaths (2100)")

axis(1, at = 1:nrow(keep_base), labels = unique(keep_100$Country_Code))

for (i in 1:length(unique(keep_100$Country_Code))) {
  current <- keep_100[keep_100$Country_Code == unique(keep_100$Country_Code)[i], ]
  
  if (current$Country_Code[1] %in% c("MEX", "JPN")) {
    lines(c(i, i), c(current$pctInc[1], current$pctInc[2]))
    lines(c(i, i), c(-50, current$pctInc[1]), lty = 2)
    lines(c(i, i), c(current$pctInc[2], 350), lty = 2)
  } else {
    if (min(current$pctlcl) > 0 & max(current$pctucl) < 300) {
      lines(c(i, i), c(min(current$pctlcl), max(current$pctucl)))
      lines(c(i-0.1, i+0.1), c(min(current$pctlcl), min(current$pctlcl)))
      lines(c(i-0.1, i+0.1), c(max(current$pctucl), max(current$pctucl)))
    } else if (min(current$pctlcl) < 0 & max(current$pctucl) < 300) {
      lines(c(i, i), c(current$pctInc[1], max(current$pctucl)))
      lines(c(i, i), c(-50, current$pctInc[1]), lty = 2)
      lines(c(i-0.1, i+0.1), c(max(current$pctucl), max(current$pctucl)))
    } else if (min(current$pctlcl) < 0 & max(current$pctucl) > 300) {
      lines(c(i, i), c(current$pctInc[3], 350), lty = 2)
      lines(c(i, i), c(current$pctInc[1], current$pctInc[3]))
      lines(c(i, i), c(-50, current$pctInc[1]), lty = 2)
    }
  }
  
  points(i, current$pctInc[1], pch = 21, col = "black", bg = "#A6D9F7")
  points(i, current$pctInc[2], pch = 21, col = "black", bg = "#1496E1")
  points(i, current$pctInc[3], pch = 21, col = "black", bg = "#052538")
}

par(mfrow = c(1, 1))

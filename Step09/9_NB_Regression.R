rm(list=ls()) 

library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(glmmTMB)
#library(MASS)
#library(lme4)
#library(nlme)
library(sjPlot)
library(ggplot2)
library(DHARMa)
library(splines)
library(ggstatsplot)

# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

####Importing data
df <-read_excel("../Step08/8 output - EMDAT events with exposed pops, HDI GNI and prev events.xlsx")

##Add number of events over previous five years--note this was copied from Step 7b, as it was 
#commented out in that step

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

##change to 1989 for main analysis and 1999 for alternative analysis based on data accuracy by EM-DAT authors 
df <- df%>% filter(Start.Year>1989)

##only PRK (North Korea) has missing gni.group (12 events), manually replace as LI

sum(is.na(df$gni.group)) #12, #9 for 2000 forward

#df$gni.group.baseline[df$ISO=="PRK"] = "LI"

#df$gni.group[df$ISO=="PRK"] = "LI"



##create per event death rate using local only and larger areas for events with less granular location data
df$death.rate<-df$total.deaths.na.as.0/df$pop.exposed.local.else.adm1.else.national.100yr
df$death.rate.local<-df$total.deaths.na.as.0/df$pop.exposed.local.if.available.100yr

##death rate removing nas in EM-DAT deaths column
filtered.df<-df%>% filter (!is.na(total.deaths.reported))

#sum # of deaths globally and in U.S.
sum(filtered.df$Total.Deaths)
range(filtered.df$Total.Deaths)
result_row <- filtered.df %>% filter(Total.Deaths == 165708)

##local only exposed pops by removing nas in local deaths column
filtered.df.local<-df%>% filter (!is.na(pop.exposed.local.if.available.100yr))

##adm1 only exposed pops by removing nas in adm1 deaths column
filtered.df.adm1<-df%>% filter (!is.na(pop.exposed.adm1.if.available.100yr))

##location data available
filtered.df.location<-df%>% filter (!is.na(Location))

# Find rows where column 'death.rate.local has 'Inf' or is above 1

filtered.df.local %>%
  filter(death.rate.local > 1 | death.rate.local==Inf) %>%
  dplyr::select(DisNo., pop.exposed.local.if.available.100yr, 
                 pop.exposed.local.else.adm1.else.national.100yr, 
                 total.deaths.na.as.0)


# Fit a basic GLMM (assuming random intercept for country), try poisson, 
#check for overdispersion, complete for total deaths removing Nas, 
#and sensitivity analysis replacing NAs with zeros, since only 
#1 event in dataset with deaths as zero

filtered.df$Country_Code<- as.factor(filtered.df$SLIIDERS.country.code.flat)
filtered.df$Year<- filtered.df$Start.Year - 1990
filtered.df$GNI<-as.factor(filtered.df$gni.group)
filtered.df$type <-as.factor(filtered.df$Disaster.Subtype)
filtered.df$declare <-as.factor(filtered.df$Declaration)


# Create spline term for time 
plot(filtered.df$Start.Year,log(filtered.df$death.rate))

filtered.df$spline_year <- bs(filtered.df$Year, df = 3) 


#Check distribution of death rate (either using else exposed pop or using local only) and compare across income groups
gghistostats(filtered.df,death.rate)
ggbetweenstats(filtered.df, gni.group, death.rate, outlier.tagging = TRUE)

gghistostats(filtered.df,death.rate.local)
ggbetweenstats(filtered.df.local, gni.group, death.rate.local, outlier.tagging = TRUE)

#Check distribution of exposed population and compare across income groups
gghistostats(filtered.df,pop.exposed.local.else.adm1.else.national.100yr)
ggbetweenstats(filtered.df, gni.group, pop.exposed.local.else.adm1.else.national.100yr, outlier.tagging = TRUE)

gghistostats(filtered.df,pop.exposed.local.if.available.100yr)
ggbetweenstats(filtered.df, gni.group, pop.exposed.local.if.available.100yr, outlier.tagging = TRUE)

##Check distribution of per capita GNI
##creates issues in running models, so using categorical income group for now
##tried scaling, runs but need to discuss if better to keep categorical 

gghistostats(filtered.df,gnipc)

filtered.df$gnipc.scale <- scale(filtered.df$gnipc)

##if use gnipc, need to remove PRK since don't have value

filtered.df <- filtered.df %>% 
  filter(!is.na(as.vector(gnipc.scale)))


##use if want to limit to events in countries with at least X number of events
##should be done prior to spline scaling
filtered.df.country <- filtered.df %>%
  group_by(Country_Code) %>%
  filter(n() >= 3) %>%
  ungroup()

#fit poisson

fit.poisson <- glmmTMB(total.deaths.reported ~  
                         offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
                       + spline_year + gni.group + no.events.5.years + declare + log(pop.total.adm0)
                       + (1|Country_Code), data = filtered.df, family = "poisson")
summary(fit.poisson)

# Residual diagnostics--testing for overdispersion, outliers, etc.  

sim_res.p <- simulateResiduals(fit.poisson)
plot(sim_res.p)

#fit negative binomial
##previously ran with DisNo.nocoun as random effects to account for event specific variance 
#for those events that hit several countries, but so few and couldn't get predictions estimates 
#by country to work with that term included, best diagnostic plots looks to be gnipc.scale 
#alone (tried hdi and gni group) and limiting to countries with at least 3 events improves diagnostics
#adding type improves fit as measured by AIC/BIC, but not diagnostics and increases variance in 
#country-specific estimates, likely because of tsunamis

fit <- glmmTMB(total.deaths.reported ~  offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
               + declare + hdi + type + spline_year + no.events.5.years + log(pop.total.adm0)
               + (1|Country_Code), data = filtered.df, family = "nbinom2")
summary(fit)

#fit negative binomial including hdi instead of gnipc
fit.hdi <- glmmTMB(total.deaths.reported ~  offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
               + spline_year + hdi + type + no.events.5.years + declare + log(pop.total.adm0)
               + (1|Country_Code), data = filtered.df, family = "nbinom2")
summary(fit.hdi)
# Residual diagnostics--testing for overdispersion, outliers, etc.  

sim_res.nb <- simulateResiduals(fit, re.form = NULL)
plot(sim_res.nb)

#####################################################################
##fitting same models using dataset in which NAs are replaced with zeros

df$Country_Code<- as.factor(df$SLIIDERS.country.code.flat)
df$Year <- df$Start.Year - 1990
df$GNI<-as.factor(df$gni.group)
df$type <-as.factor(df$Disaster.Subtype)
df$declare <-as.factor(df$Declaration)

#log death rate
df$zeroDeath <- ifelse(df$death.rate == 0, 1, 0)
df$logDeathRate <- NA
df$logDeathRate[df$zeroDeath == 0] <- 
  log(df$death.rate[df$zeroDeath == 0])

df$death.rate.shift <-
  df$death.rate +
  min(df$death.rate[df$zeroDeath == 0])

df$logDeathRate0s <- log(df$death.rate.shift)

#log deaths (only non-zero) and exposed populations
df$l.Total.Deaths<-log(df$Total.Deaths)
df$l.pop.exposed.local.else.adm1.else.national.100yr<-log(df$pop.exposed.local.else.adm1.else.national.100yr)

#log total deaths nas as zero
df$zeroDeaths <- ifelse(df$total.deaths.na.as.0 == 0, 1, 0)
df$l.Total.Deaths.0 <- NA
df$l.Total.Deaths.0[df$zeroDeaths == 0] <- 
  log(df$total.deaths.na.as.0[df$zeroDeaths == 0])
df$death.shift <-
  df$total.deaths.na.as.0 +
  min(df$total.deaths.na.as.0[df$zeroDeaths == 0])

df$logDeath0s <- log(df$death.shift)

df$gni.group <- factor(df$gni.group,
                levels = c("LI", "LMI", "UMI", "HI"))

ggbetweenstats(df, gni.group, death.rate, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, logDeathRate, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, logDeathRate0s, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, total.deaths.na.as.0, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, l.Total.Deaths.0, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, logDeath0s, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, pop.exposed.local.else.adm1.else.national.100yr, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, pop.exposed.local.if.available.100yr, outlier.tagging = TRUE)
ggbetweenstats(df, gni.group, l.pop.exposed.local.else.adm1.else.national.100yr, outlier.tagging = TRUE)

ggbetweenstats(filtered.df.local, gni.group, death.rate.local, outlier.tagging = TRUE)
gghistostats(df,pop.exposed.local.if.available.100yr)
ggbetweenstats(df, gni.group, pop.exposed.local.if.available.100yr, outlier.tagging = TRUE)

# Create spline term for time

plot(df$Year,log(df$death.rate))

df$spline_year <- bs(df$Year, df = 3) 

#scale gnipc

gghistostats(df,gnipc)

df$gnipc.scale <- scale(df$gnipc)

##if want to limit to countries with greater than 3 events
df.country <- df %>%
  group_by(Country_Code) %>%
  filter(n() >= 3) %>%
  ungroup()

##if use gnipc, need to remove PRK since don't have value

df<-df%>% 
  filter (!is.na(hdi))

#model with only data from 2000 forward
df.2000 <- df%>% filter(Start.Year>1999)
df.2000$Year <- df.2000$Start.Year - 2000

plot(df.2000$Year,log(df.2000$death.rate))

df.2000$spline_year <- bs(df.2000$Year, df = 3) 

#model with only tropical cyclone data
df.tc <- df%>% filter(Disaster.Subtype=="Tropical cyclone")

plot(df.tc$Year,log(df.tc$death.rate))

df.tc$spline_year <- bs(df.tc$Year, df = 3) 

#fit poisson, zero inflated

fit.poisson.na <- glmmTMB(total.deaths.na.as.0 ~  
                            offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
                       + spline_year + gnipc.scale + no.events.5.years + log(pop.total.adm0)
                       + (1|Country_Code), data = df, ziformula=~1, family = "poisson")
summary(fit.poisson.na)

# Residual diagnostics--testing for overdispersion, outliers, etc.  

sim_res.p <- simulateResiduals(fit.poisson.na)
plot(sim_res.p)



#fit negative binomial, run with df.gni (all countries with an event) 
#and df.gni.country (countries with at least 3 events)

fit.NAzero <- glmmTMB(total.deaths.na.as.0 ~  
                        offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
                      + spline_year + type + hdi + no.events.5.years + declare + log(pop.total.adm0) 
                       + (1 | Country_Code), data = df, family = "nbinom2")
summary(fit.NAzero)

# Residual diagnostics--testing for overdispersion, outliers, etc.  

sim_res.nb.NAzero <- simulateResiduals(fit.NAzero)
plot(sim_res.nb.NAzero)

#fit model only using events in 2000-2024
fit.NAzero.2000 <- glmmTMB(total.deaths.na.as.0 ~  
                        offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
                      + spline_year + type + hdi + no.events.5.years + declare + log(pop.total.adm0) 
                      + (1 | Country_Code), data = df.2000, family = "nbinom2")
summary(fit.NAzero.2000)

# Residual diagnostics  

sim_res.nb.NAzero.2000 <- simulateResiduals(fit.NAzero.2000)
plot(sim_res.nb.NAzero.2000)

#fit model only using events in 2000-2024
fit.NAzero.tc <- glmmTMB(total.deaths.na.as.0 ~  
                             offset(log(pop.exposed.local.else.adm1.else.national.100yr)) 
                           + spline_year + hdi + no.events.5.years + declare + log(pop.total.adm0) 
                           + (1 | Country_Code), data = df.tc, family = "nbinom2")
summary(fit.NAzero.tc)

# Residual diagnostics  

sim_res.nb.NAzero.tc <- simulateResiduals(fit.NAzero.tc)
plot(sim_res.nb.NAzero.tc)
#Make a table comparing parameter values for NAs removed and NAs as zero models

tab_model(fit,fit.NAzero,
          show.intercept = F,
          show.r2 = F,
          show.ci = 0.95,
          show.aic = T,
          show.est = T,
          show.p = T,
          show.loglik = T,
          show.dev = T,
          collapse.ci = T,
          digits.rsq = 3,
          show.re.var = T,
          emph.p = T,
          string.est = "Risk ratios",
          string.pred = "Covariate variables",
          title = "Regression model results ",
          file="regression.nbinomGNINAszerosNAremoved 2000 2_28.doc")

#Make a table comparing parameter values for 1990 vs 2000 start date models

tab_model(fit.NAzero.2000,fit.NAzero,
          show.intercept = F,
          show.r2 = F,
          show.ci = 0.95,
          show.aic = T,
          show.est = T,
          show.p = T,
          show.loglik = T,
          show.dev = T,
          collapse.ci = T,
          digits.rsq = 3,
          show.re.var = T,
          emph.p = T,
          string.est = "Risk ratios",
          string.pred = "Covariate variables",
          title = "Regression model results ",
          file="regression.nbinomGNINAszerosNAremoved 2000 3_1.doc")

#Make a table comparing parameter values for tc only versus all event model

tab_model(fit.NAzero.tc,fit.NAzero,
          show.intercept = F,
          show.r2 = F,
          show.ci = 0.95,
          show.aic = T,
          show.est = T,
          show.p = T,
          show.loglik = T,
          show.dev = T,
          collapse.ci = T,
          digits.rsq = 3,
          show.re.var = T,
          emph.p = T,
          string.est = "Risk ratios",
          string.pred = "Covariate variables",
          title = "Regression model results ",
          file="regression.nbinomGNINAszeros tc 3_1.doc")


#national population model
fit.nat <- glmmTMB(total.deaths.na.as.0 ~  
                        offset(log(pop.exposed.adm0.100yr)) 
                      + spline_year + type + hdi + no.events.5.years + declare + log(pop.total.adm0) 
                      + (1 | Country_Code), data = df, family = "nbinom2")
summary(fit.nat)


#local population model
fit.local <- glmmTMB(total.deaths.na.as.0 ~  
                     offset(log(pop.exposed.local.if.available.100yr)) 
                   + spline_year + type + hdi + no.events.5.years + declare + log(pop.total.adm0) 
                   + (1 | Country_Code), data = df, family = "nbinom2")
summary(fit.local)
#Make a table comparing parameter values for NAs removed and NAs as zero models
tab_model(fit.nat, fit.local,fit.NAzero,
          show.intercept = F,
          show.r2 = F,
          show.ci = 0.95,
          show.aic = T,
          show.est = T,
          show.p = T,
          show.loglik = T,
          show.dev = T,
          collapse.ci = T,
          digits.rsq = 3,
          emph.p = T,
          string.est = "Risk ratios",
          string.pred = "Covariate variables",
          title = "Regression model results ",
          file="regression.nbinomGNINAszerosnatlocal 2_19.doc")

###Lloyd et al. model comparison

###model #1 GLM
lloyd = glm(log((death.rate + 1)*100000) ~  log(pop.total.adm0) + no.events.5.years + hdi + hdi^2, 
                 family = gaussian, data = df)
summary(lloyd)

sim_res.lloyd <- simulateResiduals(lloyd)
plot(sim_res.lloyd)

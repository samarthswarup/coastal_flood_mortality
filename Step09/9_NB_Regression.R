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
  View()

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
df.gni.country <- df.gni %>%
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


###Calculate country-level estimates and CIs from model 

# Calculate observed proportions of categorical variables within each country
z_proportions <- df %>%
  group_by(Country_Code, declare, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Country_Code) %>%
  mutate(prop = count / sum(count)) %>%
  dplyr::select(Country_Code, declare, type, prop)


# Create allowed combinations based on data ranges for each country
allowed_combinations <- df %>%
  group_by(Country_Code) %>%
  summarise(
    hdi_mean = mean(hdi, na.rm = TRUE), hdi_sd = sd(hdi),               
    pop.total.adm0_mean = mean(pop.total.adm0, na.rm = TRUE), pop.total.adm0_sd =sd(pop.total.adm0),
    no.events.5.years_mean = mean(no.events.5.years, na.rm = TRUE), no.events.5.years_sd = sd(no.events.5.years),
    .groups = "drop"
  )

# Function to compute predictions for each country
predict_function <- function(fit, z_proportions, allowed_combinations, df, ...) {
  total <-length(unique(z_proportions$Country_Code))
  counter <- 0
  
  results <- lapply(unique(z_proportions$Country_Code), function(level) {
    counter <<-counter +1
    message("Processing Country_Code: ", level, " (", counter, " of ", total, ")")
    
    # Get observed proportions of `declare`, 'type' for the current country
    level_proportions <- z_proportions %>% filter(Country_Code == level)
    
    # Get allowed combinations for continuous predictors for the current `country
    level_combinations <- allowed_combinations %>% filter(Country_Code == level)
    
    # Create a dataset for predictions 
    
    spline_year_values <- seq(min(df$spline_year), max(df$spline_year), length.out = 100) 
    newdata <- expand.grid(
      spline_year = spline_year_values,
      Country_Code = level,
      hdi = level_combinations$hdi_mean,                
      pop.total.adm0 = level_combinations$pop.total.adm0_mean,
      no.events.5.years =level_combinations$no.events.5.years_mean,
      declare = level_proportions$declare,
      type = level_proportions$type,
      pop.exposed.local.else.adm1.else.national.100yr = mean(df$pop.exposed.local.else.adm1.else.national.100yr)
                  
    )
    
    # Apply log transformation to country population
    newdata <- newdata %>%
      mutate(log_pop.total.adm0 = log(pop.total.adm0))
    
    # Apply spline transformation to time
    spline_terms <- ns(newdata$spline_year, df = 3)
    spline_colnames <- paste0("ns(spline_year, df = 3)", seq_len(ncol(spline_terms)))
    colnames(spline_terms) <- spline_colnames
    newdata <- cbind(newdata, spline_terms)
    
    # Predict for all combinations of events and average over events
    newdata$linear_pred <- predict(
      fit, 
      newdata = newdata, 
      re.form = NULL, # Include variability in events
      type = "link"
    )
    
    # Calculate standard errors for confidence intervals
    se <- predict(
      fit, 
      newdata = newdata, 
      re.form = NULL, 
      se.fit = TRUE, ...
    )$se.fit
    
 
  # Back-transform predictions to the response scale
  newdata <- newdata %>%
    mutate(
      rate = exp(linear_pred) / pop.exposed.local.else.adm1.else.national.100yr,  # Calculate rate
      lower = exp(linear_pred - 1.96 * se) / pop.exposed.local.else.adm1.else.national.100yr,  # Lower CI
      upper = exp(linear_pred + 1.96 * se) / pop.exposed.local.else.adm1.else.national.100yr  # Upper CI
    ) 
  # Merge with proportions and calculate weighted rates
  newdata <- newdata %>%
    left_join(level_proportions, by = c("Country_Code", "declare")) %>%
    group_by(declare) %>%
    summarise(
      weighted_rate = mean(rate) * first(prop),
      weighted_lower = mean(lower) * first(prop),
      weighted_upper = mean(upper) * first(prop),
      .groups = "drop"
    )
  
  # Aggregate rates weighted by the proportions of declare
  data.frame(
    Country_Code = level,
    mean_rate = sum(newdata$weighted_rate),
    lower_rate = sum(newdata$weighted_lower),
    upper_rate = sum(newdata$weighted_upper)
  )
})

  # Combine results into a single data frame
  do.call(rbind, results)
}

# Compute predictions
results.NAzero <- predict_function(fit.NAzero, z_proportions, allowed_combinations, df, allow.new.levels=TRUE)

# Print results
write.csv(results.NAzero, "country_predicted_event_mortality_rate_NAzero.3.2.csv") 

mean(results.NAzero$mean_rate)
median (results.NAzero$mean_rate)

###Additional supplemental analyses#####


###initial exploration of data distribution####

# Check for missing values in both versions of death rate
sum(is.na(df$death.rate))
sum(is.na(filtered.df$death.rate.local))

# Identify outliers using boxplots
ggplot(df, aes(y = total.deaths.na.as.0)) + geom_boxplot()
ggplot(df, aes(y = pop.exposed.local.else.national.100yr)) + geom_boxplot()
ggplot(filtered.df, aes(y = death.rate.local)) + geom_boxplot()
ggplot(filtered.df, aes(y = gnipc)) + geom_boxplot()

##log of exposed and deaths
df$Ln.exposed<- log(df$pop.exposed.local.else.national.100yr)
filtered.df$Ln.exposed<- log(filtered.df$pop.exposed.local.else.national.100yr)
ggplot(df, aes(y = Ln.exposed)) + geom_boxplot()
ggplot(filtered.df, aes(y = Ln.exposed)) + geom_boxplot()

##Various ways to calculate per year and per event death rates### 

##create per year death rate using only years with an event
df$Year.factor<-as.factor(df$Start.Year)
df <- df %>%
  group_by(SLIIDERS.country.code.flat, Year.factor) %>%
  mutate(Events.count.per.year = sum(n()))%>%
  mutate(peryear.death.rate = death.rate*Events.count.per.year)%>%
  ungroup()

##create per year death rate per record using all years of dataset (currently 1990-2022)

df <- df %>%
  group_by(SLIIDERS.country.code.flat) %>%
  mutate(Events.all.count = sum(n()))%>%
  mutate(peryearallyears.death.rate = death.rate*Events.all.count/34)%>%
  ungroup()

##Create per country death rate using all years of dataset (currently 1990-2024)

df <- df %>%
  group_by(SLIIDERS.country.code.flat) %>%
  mutate(allyears.death.rate = (sum(total.deaths.na.as.0)/sum(pop.exposed.local.else.national.100yr)/34))%>%
  ungroup()


##create per year death rate using only years with an event for NAs removed death rates
filtered.df$Year.factor<-as.factor(filtered.df$Start.Year)
filtered.df <- filtered.df %>%
  group_by(SLIIDERS.country.code.flat, Year.factor) %>%
  mutate(Events.count.per.year = sum(n()))%>%
  mutate(peryear.death.rate = death.rate.local*Events.count.per.year)%>%
  ungroup()

##create per year death rate using all years of dataset (currently 1990-2024)
##for NAs removed death rates

filtered.df <- filtered.df %>%
  group_by(SLIIDERS.country.code.flat) %>%
  mutate(Events.all.count = sum(n()))%>%
  mutate(peryearallyears.death.rate = death.rate.local*Events.all.count/34)%>%
  ungroup()


##comparison of event death rates by GNI, NAs zeros
df.gni<-df%>% filter (!is.na(gni.group))
df.gnid<-df.gni%>% filter (!is.na(death.rate))
df.gnidr<-df.gnid%>% filter(death.rate<1)
ggbetweenstats(df.gnidr, gni.group, death.rate, outlier.tagging = TRUE)

##comparison of yearly death rates by year GNI, NAs zeros
df.gnidy<-df.gni%>% filter (!is.na(peryear.death.rate))
df.gnidry<-df.gnidy%>% filter(peryear.death.rate<1)
ggbetweenstats(df.gnidry, gni.group, peryear.death.rate, outlier.tagging = TRUE)

##comparison of yearly death rates all years GNI, NAs zeros
df.gnidya<-df.gni%>% filter (!is.na(peryearallyears.death.rate))
df.gnidrya<-df.gnidya%>% filter(peryearallyears.death.rate<1)
ggbetweenstats(df.gnidrya, gni.group, peryearallyears.death.rate, outlier.tagging = TRUE)

##comparison of country yearly death rate all years GNI, NAs zeros
##--need to subset first so only 1 value per country
df.gnidyac<-df.gni%>% filter (!is.na(allyears.death.rate))
df.gnidryac<-df.gnidyac%>% filter(allyears.death.rate<1)
ggbetweenstats(df.gnidryac, gni.group, allyears.death.rate, outlier.tagging = TRUE)

##comparison of event death rates by GNI, NAs removed
filtered.df.gni<-filtered.df%>% filter (!is.na(gni.group))
filtered.df.gnid<-filtered.df.gni%>% filter (!is.na(death.rate.local))
filtered.df.gnidr<-filtered.df.gnid%>% filter(death.rate.local<1)
ggbetweenstats(filtered.df.gnidr, gni.group, death.rate.local, outlier.tagging = TRUE)

##comparison of yearly death rates by year by GNI, NAs removed
filtered.df.gni<-filtered.df%>% filter (!is.na(gni.group))
filtered.df.gnidy<-filtered.df.gni%>% filter (!is.na(peryear.death.rate))
filtered.df.gnidry<-filtered.df.gnidy%>% filter(peryear.death.rate<1)
ggbetweenstats(filtered.df.gnidry, gni.group, peryear.death.rate, outlier.tagging = TRUE)

##comparison of yearly death rates by year by GNI, NAs removed
filtered.df.gni<-filtered.df%>% filter (!is.na(gni.group))
filtered.df.gnidya<-filtered.df.gni%>% filter (!is.na(peryearallyears.death.rate))
filtered.df.gnidrya<-filtered.df.gnidy%>% filter(peryearallyears.death.rate<1)
ggbetweenstats(filtered.df.gnidrya, gni.group, peryearallyears.death.rate, outlier.tagging = TRUE)

# Histograms 
gghistostats(df.gnidr,death.rate)
gghistostats(df.gnidr,pop.exposed.local.else.national.100yr)

gghistostats(df.gnidry,peryear.death.rate)
gghistostats(df.gnidry,pop.exposed.local.else.national.100yr)

#create column with disaster number only, removing country code, to get an event specific variable
##was used in some models as random effect term
df <- df %>%
  mutate(DisNo.nocoun = gsub("[A-Za-z]", "", DisNo.))

#try glme.nb just to see if different from using glmmTMB function--was very similar
fit.glmer <- glmer.nb(total.deaths.reported ~  offset(log(pop.exposed.local.else.national.100yr))
                      + spline_year + GNI + declare + no.events.5.years + log(pop.total.adm0) 
                      + (1 | Country_Code), data = filtered.df.gni.country)
summary(fit.glmer)

#report fit stats
library(pscl)
pR2(fit)['McFadden']
aic_value <- AIC(fit)
aic_value
bic_value<- BIC(fit)
bic_value


###########################################################
###Predictions appending to datasets ####
df$predictions <- predict(fit.NAzero, newdata = df, type = "response")
df$Predicted_MortalityRisk <- (df$predictions/df$pop.exposed.local.else.adm1.else.national.100yr)

mean(df$predictions)
mean(df$Predicted_MortalityRisk)

median(df$predictions)
median(df$Predicted_MortalityRisk)

##per event predictions by country
Countryaverage <- df %>% 
  group_by(Country_Code) %>% 
  summarise(Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Deaths= sum(total.deaths.na.as.0),
            Aff_pop=sum(pop.exposed.local.else.adm1.else.national.100yr),
            observed.death.rate=(Deaths/Aff_pop),
            Predicted_Deaths= mean(predictions))
mean(Countryaverage$Predicted_Mortality_rate)
write.csv(Countryaverage, "country_predicted_mortality_rate_NAszeros1990_3_2.csv") 

##per year predictions by country
Countryyearlyaverage <- df.gni %>% 
  group_by(Country_Code,Year) %>% 
  summarise(Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Predicted_yearly_mortality_rate = Predicted_Mortality_rate*Events_Count,
            Deaths= mean(total.deaths.na.as.0),
            Aff_pop=mean(pop.exposed.local.else.national.100yr),
            observed.mean.yearly.death.rate=(Deaths/Aff_pop)*Events_Count,
            Predicted_Deaths_peryear= mean(predictions)*Events_Count)

mean(Countryyearlyaverage$Predicted_yearly_Mortality_rate)

write.csv(Countryyearlyaverage, "country_predicted_yearly_mortality_rate_naszeros.csv") 

##per year predictions by country all years
Countryyearlyaverage32 <- df.gni %>% 
  group_by(Country_Code) %>% 
  summarise(Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Events_per_year = Events_Count/32,
            Predicted_yearly_mortality_rate = Predicted_Mortality_rate*Events_per_year,
            Deaths= mean(total.deaths.na.as.0),
            Aff_pop=mean(pop.exposed.local.else.national.100yr),
            observed.mean.event.death.rate=(Deaths/Aff_pop),
            observed.mean.yearly.death.rate=observed.mean.event.death.rate*Events_per_year,
            Predicted_Deaths= mean(predictions))

mean(Countryyearlyaverage$Predicted_yearly_mortality_rate)

write.csv(Countryyearlyaverage32, "country_predicted_yearly_mortality_rate_naszeros32.csv") 

##predictions by year
Yearaverage <- df.gni %>% 
  group_by(Year) %>% 
  summarise(Year= median(Year),
            Mean_Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Mean_Deaths= mean(total.deaths.na.as.0),
            Mean_Aff_pop=mean(pop.exposed.local.else.national.100yr),
            observed.mean.death.rate=(Mean_Deaths/Mean_Aff_pop),
            Predicted_Deaths= mean(predictions))

plot(Yearaverage$Year, Yearaverage$ Mean_Predicted_Mortality_rate)
mean(Yearaverage$ Mean_Predicted_Mortality_rate)

write.csv(Yearaverage, "Year_predicted_mortality_rate_naszeros.csv") 

#############
#####predictions without NAs

filtered.df$predictions <- predict(fit, newdata = filtered.df, type = "response")
filtered.df$Predicted_MortalityRisk <- (filtered.df$predictions/filtered.df$pop.exposed.local.else.adm1.else.national.100yr)

mean(filtered.df$predictions)
mean(filtered.df$Predicted_MortalityRisk)

median(filtered.df$predictions)
median(filtered.df$Predicted_MortalityRisk)
sd(filtered.df$predictions)


##predictions by country
average <- filtered.df %>% 
  group_by(Country_Code) %>% 
  summarise(Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Deaths= sum(total.deaths.reported),
            Aff_pop=sum(pop.exposed.local.else.national.100yr),
            observed.mean.death.rate=(Deaths/Aff_pop),
            Predicted_Deaths= mean(predictions))

mean(average$Predicted_Mortality_rate)

write.csv(average, "Country_predicted_mortality_rate_nas removed2_6.csv") 

##predictions by year
average <- filtered.df.gni %>% 
  group_by(Year) %>% 
  summarise(Year=median(Year),
            Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Deaths= mean(total.deaths.reported),
            Aff_pop=mean(pop.exposed.local.else.national.100yr),
            observed.mean.death.rate=(Deaths/Aff_pop),
            Predicted_Deaths= mean(predictions))

plot(average$Year, average$Predicted_Mortality_rate)
mean(average$Predicted_Mortality_rate)

write.csv(average, "Year_predicted_mortality_rate_nas removed.csv") 

##per year predictions by country
Countryyearlyaverage <- filtered.df.gni %>% 
  group_by(Country_Code,Year) %>% 
  summarise(Predicted_Mortality_rate = mean(Predicted_MortalityRisk),
            Events_Count = sum(n()),
            Deaths= mean(total.deaths.reported),
            Aff_pop=mean(pop.exposed.local.else.national.100yr),
            observed.mean.death.rate=(Deaths/Aff_pop),
            Predicted_Deaths= mean(predictions))

plot(Countryyearlyaverage$Year, Countryyearlyaverage$Predicted_Mortality_rate)
mean(Countryyearlyaverage$Predicted_Mortality_rate)

write.csv(Countryyearlyaverage, "country_predicted_yearly_mortality_rate_nas removed.csv") 

#Creating death rate based on EMDAT only data

df$emdeath.rate<-df$total.deaths.na.as.0/df$Total.Affected

#Events with no exposed population or death rate greater than 1 to examine further 

sum(filtered.df.local$pop.exposed.local.if.available.100yr==0) 

sum(is.na(df$pop.exposed.local.if.available.100yr))

sum(is.na(filtered.df.local$pop.exposed.local.if.available.100yr))

filtered_rows <- df[df$death.rate.local < 1 | df$pop.exposed.local.if.available.100yr == 0, c("DisNo.", 
                                                                                         "SLIIDERS.country.code.flat","gni.group", "pop.exposed.local.if.available.100yr", 
                                                                                         "total.deaths.na.as.0","death.rate", "death.rate.local","pop.exposed.local.else.adm1.else.national.100yr"), ]

filtered_rows <- filtered_rows[!is.na(filtered_rows$DisNo.), ]

write.csv(filtered_rows, "events w weird exposed pops 2 13.csv") 

##remove problematic rows for now--check filtered_rows object

filtered.df.local<-filtered.df.local%>% 
  filter (pop.exposed.local.if.available.100yr > 0,
          death.rate.local<1)

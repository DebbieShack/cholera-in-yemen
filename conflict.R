# library(MMWRweek)
library(lubridate) #month()
library(dplyr)
library(rgdal) #readOGR
# library(sp)
# library(RColorBrewer)
library(tmap)
library(magrittr)
library(tidyverse)
# library(gridExtra)
library(ggplot2)
library(reshape2)
library(pscl)
conflict_raw <- read.csv("C:/Users/dms228/OneDrive - University of Exeter/PhD/data/Yemen/conflict_data_yem.csv")
shape_file_govs <- "C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm1_govyem_cso_20191002.shp"
shape_govs <- readOGR(shape_file_govs)
tCase_pDist <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/tCase_pDist.RDS")

#Only consider events of type either "Shelling/artillery/millile attack" or "Air/drone strike" from 2017 onwards
d1 <- as.POSIXct("2017-01-01 GMT")
explosions <- conflict_raw %>% 
  filter(event_type == "Explosions/Remote violence") %>%
  subset(select = c(event_date, year, sub_event_type, admin1, admin2)) %>%
  mutate(event_date = as.POSIXct(event_date)) %>%
  filter(event_date >= d1) %>%
  mutate(month = month(event_date)) %>%
  mutate(count = 1) %>%
  mutate(year = as.numeric(year))



#######Change spellings to be inline with .shp file #####
explosions$admin1[explosions$admin1 == "Al Hudaydah"] <- "Al Hodeidah"
explosions$admin1[explosions$admin1 == "Sadah"] <- "Sa'dah"
explosions$admin1[explosions$admin1 == "Taizz"] <- "Ta'iz"
explosions$admin1[explosions$admin1 == "Sanaa"] <- "Sana'a"
explosions$admin1[explosions$admin1 == "Amanat al Asimah"] <- "Sana'a City"
explosions$admin1[explosions$admin1 == "Suqutra"] <- "Socotra"
explosions$admin1[explosions$admin1 == "Lahij"] <- "Lahj"
explosions$admin1[explosions$admin1 == "Al Mahrah"] <- "Al Maharah"
explosions$admin1[explosions$admin1 == "Marib"] <- "Ma'rib"
explosions$admin1[explosions$admin1 == "Ad Dali"] <- "Ad Dali'"
###############

#######Change spellings to be inline with WHO weekly cholera data ##################
explosions[4:5] <- gsub("[^[:alnum:]]", "", as.matrix(explosions[,4:5])) 
explosions$admin1[explosions$admin1 == "AdDali"] <- "AlDhalee"
explosions$admin1[explosions$admin1 == "AlMahrah"] <- "AlMaharah"
explosions$admin1[explosions$admin1 == "AlMahrah"] <- "AlMaharah"
explosions$admin1[explosions$admin1 == "AmanatalAsimah"] <- "AmanatAlAsimah"
explosions$admin1[explosions$admin1 == "Lahij"] <- "Lahj"
explosions$admin1[explosions$admin1 == "Sadah"] <- "Saada"

explosions$admin2[explosions$admin1 == "Sadah"] <- "Saada"

########################

##Join total cases and explosions at district level
tExp_pDist <- aggregate(explosions$count, by = list(explosions$admin1, explosions$admin2), FUN = sum)
colnames(tExp_pDist) <- c("Governorate", "District", "Total_explosions")
tExpCaseDf <- inner_join(tExp_pDist, tCase_pDist, by = c("Governorate", "District"))

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/WHO_weekly_data/cases_strikes_basic.png", width = 600, height = 400)
ggplot(data = tExpCaseDf, aes(x = Total_explosions, y = Total_Cases)) +
  labs(title = "Total Cases vs. Total Strikes Regression Plot", x = "Strikes", y = "Suspected Cases") +
  geom_point() +
  theme_bw()
dev.off()

##Join total cases per 100,000 inhabitants and explosions 
WHO_weekly_per <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_per.RDS")
explosions <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/explosions.RDS")

##Wave 1 (up to May 2019)
w1_end <- as.POSIXct("2018-05-01")
WHO_weekly_per_w1 <- WHO_weekly_per %>% filter(Date < w1_end) %>% group_by(Governorate, District) %>% 
  summarize(across(c(S.Cases, Deaths),sum,na.rm=TRUE)) 
explosions_w1 <- explosions %>% filter(event_date < w1_end) %>% group_by(admin1, admin2) %>% 
  summarize(sum(count, na.rm = TRUE)) %>% set_colnames(c("Governorate", "District", "Count"))

#Now join them
tExpCasePerDf_w1 <- inner_join(WHO_weekly_per_w1, explosions_w1, by = c("Governorate", "District"))
tExpCaseP

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/WHO_weekly_data/StrikesVsDeaths_w1.png", width = 600, height = 400)
ggplot(data = tExpCasePerDf_w1, aes(x = Count)) +
  geom_point(aes(y = Deaths)) +
  labs(x = "Total strikes", y = "Cholera deaths per 100,000 inhabitants", title = "First Wave") +
  theme_bw()
dev.off()


##Wave 2 (August 2018 and onwards)
w2_start <- as.POSIXct("2018-08-01")
WHO_weekly_per_w2 <- WHO_weekly_per %>% filter(Date < w2_start) %>% group_by(Governorate, District) %>% 
  summarize(across(c(S.Cases, Deaths),sum,na.rm=TRUE)) 
explosions_w2 <- explosions %>% filter(event_date < w2_start) %>% group_by(admin1, admin2) %>% 
  summarize(sum(count, na.rm = TRUE)) %>% set_colnames(c("Governorate", "District", "Count"))

#Convert deaths per 100,000 into integers
tExpCasePerDf_w2$Deaths <- tExpCasePerDf_w2$Deaths %>% as.integer()
tExpCasePerDf_w1$Deaths <- tExpCasePerDf_w1$Deaths %>% as.integer()

tExpCasePerDf_w2 <- inner_join(WHO_weekly_per_w2, explosions_w2, by = c("Governorate", "District"))

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/WHO_weekly_data/StrikesVsDeaths_w3.png", width = 600, height = 400)
ggplot(data = tExpCasePerDf_w2, aes(x = Count)) +
  geom_point(aes(y = Deaths)) +
  labs(x = "Total strikes", y = "Cholera deaths per 100,000 inhabitants", 
       title = "Second Wave") +
  theme_bw()
dev.off()

model_w2 <- zeroinfl(Deaths ~ Count, data = tExpCasePerDf_w2, dist = "negbin")
summary(model_w2)

model_w1 <- zeroinfl(Deaths ~ Count, data = tExpCasePerDf_w1, dist = "negbin")
summary(model_w1)


#####Aggregate Monthly by Governorate###############

explosions %<>% mutate(months_since = month + 12*(year - 2017))
explosions_monthly <- aggregate(explosions$count, by = list(explosions$months_since, explosions$admin1), FUN = sum)
explosions_monthly_yemen <- aggregate(explosions$count, by = list(explosions$months_since), FUN = sum)
colnames(explosions_monthly_yemen) <- c("months_since", "Total strikes")
colnames(explosions_monthly) <- c("months_since", "gov", "strikes")
monthly_sum_expl <- dcast(explosions_monthly, months_since ~ gov) #Data frame of weekly 'strikes' per governate
monthly_sum_expl$Yemen <- explosions_monthly_yemen$`Total strikes`
monthly_sum_expl$months_lag1 <- monthly_sum_expl$months_since - 1
saveRDS(monthly_sum_expl,"C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/monthly_sum_expl.RDS")
#sum_expl <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_expl.RDS")

#####Aggregate Monthly by District###############

explosions %<>% mutate(months_since = month + 12*(year - 2017))
explosions_monthly <- explosions %>% group_by(admin1, admin2, months_since) %>% summarise(Count = sum(count))
#explosions_monthly_yemen <- aggregate(explosions$count, by = list(explosions$months_since), FUN = sum)
colnames(explosions_monthly_yemen) <- c("months_since", "Total strikes")
colnames(explosions_monthly) <- c("Governorate","District","months_since","Strikes")
MonthlyExpCases <- merge(WHO_monthly_per,explosions_monthly, by.x = c("Governorate", "District", "months_since_lag1"), by.y = c("Governorate", "District", "months_since"), all.x = TRUE)
MonthlyExpCases$Strikes[is.na(MonthlyExpCases$Strikes)] = 0
MonthlyExpCases$S.Cases <- MonthlyExpCases$S.Cases %>% as.integer

ggplot(data = MonthlyExpCases, aes(x = Strikes, y = S.Cases)) +
  geom_point() +
  labs(x = "Total Monthly Strikes", "Total Cholera Cases in Following Month")

monthly_sum_expl <- dcast(explosions_monthly, months_since ~ gov) #Data frame of weekly 'strikes' per governate
monthly_sum_expl$Yemen <- explosions_monthly_yemen$`Total strikes`
monthly_sum_expl$months_lag1 <- monthly_sum_expl$months_since - 1
saveRDS(monthly_sum_expl,"C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/monthly_sum_expl.RDS")
#sum_expl <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_expl.RDS")

#####Aggregate Weekly##############
explosions$weeks_since <- explosions$event_date %>%   #Week number starting 2017-01-01
  difftime(d1, units = "weeks") %>%
  floor() %>%
  as.numeric() %>%
  add(1)

explosions_weekly <- aggregate(explosions$count, by = list(explosions$weeks_since, explosions$admin1, explosions$admin2), FUN = sum)
colnames(explosions_weekly) <- c("weeks_since", "gov", "strikes")
sum_expl <- dcast(explosions_weekly, weeks_since ~ gov) #Data frame of weekly 'strikes' per governate
sum_expl$wks_lag2 <- sum_expl$weeks_since - 2
#saveRDS(sum_expl,"C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_expl.RDS")
#sum_expl <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_expl.RDS")


##############################################################################



#Aggregate Annually
eDEWS_annual <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/eDEWS_annual.RDS") #weekly average
WHO_2019 <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_annual.RDS")[[2]] #Annual 2019 from WHO
WHO_2018 <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_annual.RDS")[[1]] #Annual 2018 from WHO

cases_2017 <- eDEWS_annual %>% subset(year == 2017) %>% subset(select = -c(year)) %>% 
  merge(pop_2017, by.x = "gov", by.y = "Gov") %>% mutate(cases_per = (100000*cases)/Population)

cases_2018 <- eDEWS_annual %>% subset(year == 2018) %>% subset(select = -c(year)) %>%
  merge(pop_2017, by.x = "gov", by.y = "Gov") %>% mutate(cases_per = (100000*cases)/Population)

cases_2019 <- eDEWS_annual %>% subset(year == 2019) %>% subset(select = -c(year)) %>%
  merge(pop_2017, by.x = "gov", by.y = "Gov") %>% mutate(cases_per = (100000*cases)/Population)

cases_2020 <- eDEWS_annual %>% subset(year == 2020) %>% subset(select = -c(year)) %>%
  merge(pop_2017, by.x = "gov", by.y = "Gov") %>% mutate(cases_per = (100000*cases)/Population)

saveRDS(list(cases_2017, cases_2018, cases_2019, cases_2020),
        "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/eDEWS_byYear.RDS")

explosions_annual <- aggregate(explosions$count, by = list(explosions$year, explosions$admin1), FUN = sum)
colnames(explosions_annual) <- c("year", "gov", "strikes")
stkr_2017 <- explosions_annual %>% subset(year == 2017) %>% subset(select = -year) %>% 
  merge(cases_2017, by.x = "gov", by.y = "gov")

stkr_2018 <- explosions_annual %>% subset(year == 2018) %>% subset(select = -year) %>% 
  merge(cases_2018, by.x = "gov", by.y = "gov") %>% merge(WHO_2018, by.x = "gov", by.y = "gov") %>%
  rename(cases_WHO = cases.y,deaths_WHO = deaths , cases_eDEWS = cases.x)


stkr_2019 <- explosions_annual %>% subset(year == 2019) %>% subset(select = -year) %>% 
  merge(cases_2019, by.x = "gov", by.y = "gov") %>% merge(WHO_2019, by.x = "gov", by.y = "gov") %>%
  rename(cases_WHO = cases.y,deaths_WHO = deaths , cases_eDEWS = cases.x)

stkr_2020 <- explosions_annual %>% subset(year == 2020) %>% subset(select = -year) %>% 
  merge(cases_2020, by.x = "gov", by.y = "gov")



#Merge in WHO annual cases data for 2018 and 2019
coeffs <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/reg_coeffs.RDS")
coeffs_shp <- merge(shape_govs, coeffs, by.x = "ADM1_EN", by.y = "gov")


tm_shape(coeffs_shp) +
  tm_polygons("temp", title = "Temperature Vs. Cases Regression Coefficient")


tmap_mode("view")

tmap_last()
spplot(exp_shp, "strikes")

### Cases Vs. Strikes Absoloute (eDEWS Data)
plt_2017 <- ggplot(data = stkr_2017, aes(x = cases, y = strikes)) +
  ggtitle("2017") + 
  geom_point()
plt_2018 <- ggplot(data = stkr_2018, aes(x = cases, y = strikes)) +
  ggtitle("2018") + 
  geom_point()
plt_2019 <- ggplot(data = stkr_2019, aes(x = cases, y = strikes)) +
  ggtitle("2019") + 
  geom_point()
plt_2020 <- ggplot(data = stkr_2020, aes(x = cases, y = strikes)) +
  ggtitle("2020") + 
  geom_point()

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/conflict_vs_cases.png", width = 900, height = 900)
ggarrange(plt_2017, plt_2018, plt_2019, plt_2020, nrow = 2, ncol = 2) %>% 
  annotate_figure(top = "Cholera Cases Against No. Strikes per Governate")
dev.off()

#(WHO Data)
plt_2018_WHO <- ggplot(data = stkr_2018, aes(x = cases_WHO, y = strikes)) +
  ggtitle("2018 (WHO Data)") + 
  geom_point()
plot(plt_2018_WHO)

plt_2019_WHO <- ggplot(data = stkr_2019, aes(x = cases_WHO, y = strikes)) +
  ggtitle("2019 (WHO Data)") + 
  geom_point()
plot(plt_2019_WHO)


### Cases per capita Vs. Strikes
plt_2017_cpt <- ggplot(data = stkr_2017, aes(x = cases_per, y = strikes)) +
  ggtitle("2017") + 
  xlab("Cases per 100,000 population") +
  geom_point()
plt_2018_cpt <- ggplot(data = stkr_2018, aes(x = cases_per, y = strikes)) +
  ggtitle("2018") +
  xlab("Cases per 100,000 population") +
  geom_point()
plt_2019_cpt <- ggplot(data = stkr_2019, aes(x = cases_per, y = strikes)) +
  ggtitle("2019") + 
  xlab("Cases per 100,000 population") +
  geom_point()
plt_2020_cpt <- ggplot(data = stkr_2020, aes(x = cases_per, y = strikes)) +
  ggtitle("2020") + 
  xlab("Cases per 100,000 population") +
  geom_point()

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/conflict_vs_casescapita.png", width = 900, height = 900)
ggarrange(plt_2017_cpt, plt_2018_cpt, plt_2019_cpt, plt_2020_cpt, nrow = 2, ncol = 2) %>% 
  annotate_figure(top = text_grob("Cholera Cases Against No. Strikes per Governate", size = 18, face = "bold"))
dev.off()

, col.regions = brewer.pal(n = 16, name = "Reds")

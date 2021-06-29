#IDP
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(anchors) #replace.value()
library(cowplot)
library(stats)
library(MASS)
# shelter_type <- read_excel("C:/Users/dms228/OneDrive - University of Exeter/PhD/data/Yemen/dtm_displacement_2019.xlsx", 
#                            sheet = "Shelter Type")
yemen_popDens <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/yemen_popDensData.RDS")
WHO_weekly <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_dist.RDS")
popDens_byGov <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/yemen_popDens_govAgg.RDS")
WHO_weekly_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_per.RDS")
WHO_monthly_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthly_per.RDS")
#WHO_monthly <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_monthly.RDS")
#WHO_monthly_gov <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_monthly_gov.RDS")
WHO_weeklyGov_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weeklyGov_per.RDS")
WHO_weeklyGov <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_gov.RDS")
WHO19_monthly_gov <- WHO_monthly %>% filter(Year == 2019)
WHO19_monthly_distPer <- WHO_monthly_per %>% filter(year == 2019)
WHO19_monthlyGov_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthlyGov_per.RDS") %>% filter(year == 2019)

#popDens_byGov <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popDens_gov.RDS")
IDPs <- read_excel("C:/Users/dms228/OneDrive - University of Exeter/PhD/data/Yemen/dtm_displacement_2019.xlsx", 
                   sheet = "RDT-IDPs", skip = 1)[-1,] %>% dplyr::select(-c(2,3,5,6,7,8,11,13,16,33)) 

IDPs <- IDPs[IDPs$`Total # of HH` > 0,]

#C:/Users/dms228/github/cholera-in-yemen/saved_data/

WHO_weekly$Date <- WHO_weekly$Date %>% as.numeric() %>% as.POSIXct.Date()
WHO_weekly$Year <- WHO_weekly$Date %>% year()





##WHO total cases in 2019
WHO19_distTot <- WHO_weekly %>% filter(year(Date) == 2019) %>% group_by(Governorate, District) %>% dplyr::summarise(across(S.Cases:Under5, sum))
WHO19_govTot <- WHO_weeklyGov %>% filter(year(Date) == 2019) %>% group_by(Governorate) %>% dplyr::summarise(across(S.Cases:Under5, sum, na.rm = TRUE))


#####IDP Cleaning, cleaning and adding some helpful dates
#Change spellings to fit new data
IDPs[c(2,4,5)] <- gsub("[^[:alnum:]]", "", as.matrix(IDPs[c(2,4,5)])) 
IDPs <- replace.value(IDPs, names = c("Governorate", "Governorate of Origin"), from = "Hadramaut", to = "Hadramawt")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "AlMawasit", to = "AlMawasid")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "MerkhahAsSufla", to = "MarkhahAssufla")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "AlMakhad", to = "AlMakhadir")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "AsSaddahgf", to = "AsSaddah")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "Bidbadah", to = "BidBidah")
IDPs <- replace.value(IDPs, names = c("District of Origin"), from = "Medghal", to = "Madkhal")

IDPs$`Displacement Date` <- IDPs$`Displacement Date` %>% as.numeric() %>% as.Date(origin = '1900-01-01') %>% as.POSIXct()
IDPs$`Total # of HH` <- IDPs$`Total # of HH` %>% as.numeric()
IDPs$month <- month(IDPs$`Displacement Date`)

HH_leaving_dist <- aggregate(IDPs$`Total # of HH`, by = list(Gov = IDPs$`Governorate of Origin`, Dis = IDPs$`District of Origin`), FUN = sum) 
HH_leaving_gov <- aggregate(IDPs$`Total # of HH`, by = list(Gov = IDPs$`Governorate of Origin`), FUN = sum) 
HH_arriv <- aggregate(IDPs$`Total # of HH`, by = list(Gov = IDPs$`Governorate`), FUN = sum)

#Total number of households leaving each district in 2019
tot_idp_dist <- merge(HH_leaving_dist, WHO19_distTot, by.x = c("Gov","Dis"), by.y = c("Governorate","District"),
                      all.y = TRUE) %>% dplyr::rename(HH_leaving = x)
tot_idp_dist$HH_leaving[is.na(tot_idp_dist$HH_leaving)] <- 0 #Missing HH_leaving data is assumed to be zero
tot_idp_dist <- na.omit(tot_idp_dist) #District (mostly 'blank') with no cholera case data are removed. 
#Total number of households leaving or arriving at each Governorate in 2019
tot_idp_gov <- merge(HH_leaving_gov, HH_arriv, by.x = c("Gov"), by.y = "Gov") %>% merge(WHO19_govTot, by.x = "Gov", by.y = "Governorate") %>%
  dplyr::rename(HH_leaving = x.x, HH_arrive = x.y) %>% merge(popDens_byGov, by.x = "Gov", by.y = "Governorate") %>%
  dplyr::select(-c(`2017`,`2018`,`2020`)) %>% dplyr::rename(PopDensity = `2019`)

#saveRDS(tot_idp_gov,"C:/Users/dms228/github/cholera-in-yemen/saved_data/tot_idp_gov.RDS")
#saveRDS(tot_idp_dist, "C:/Users/dms228/github/cholera-in-yemen/saved_data/tot_idp_dist.RDS")


#Linear regression (log) plot of comparing the No.Households fleeing a district with cholera suspected cases in the same district
png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/lgHH_fleeingVsCases_dist.png", width = 600, height = 400)                   
ggplot(data = tot_idp_dist, aes(x = log(HH_leaving), y = S.Cases)) +
  geom_point() +
  labs(x = "Number of Households Fleeing District (log)", y = "Number of Suspected Cases") +
  theme_bw()
dev.off()

png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/HH_fleeingVsCases_dist.png", width = 600, height = 400)                   
ggplot(data = tot_idp_dist, aes(x = HH_leaving, y = S.Cases)) +
  geom_point() +
  labs(x = "Number of Households Fleeing District", y = "Number of Suspected Cases") +
  theme_bw()
dev.off()

png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/HH_fleeingVsDeaths_gov.png", width = 600, height = 400)                   
ggplot(data = tot_idp_gov, aes(x = log(HH_leaving), y = log(Deaths))) +
  geom_point() +
  labs(x = "Number of Households Fleeing Governorate (log)", y = "Number of Deaths in Governorate (log)") +
  theme_bw()
dev.off()

png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/HH_fleeingVsCases_gov.png", width = 600, height = 400)                   
ggplot(data = tot_idp_gov, aes(x = log(HH_leaving), y = log(S.Cases))) +
  geom_point() +
  labs(x = "Number of Households Fleeing Governorate (log)", y = "Number of Suspected Cases in Governorate (log)") +
  theme_bw()
dev.off()

#Just a side not
ggplot(data = tot_idp_gov, aes(x = PopDensity, y = S.Cases)) +
  geom_point() +
  labs(x = "Population Density", y = "Total Number of Suspected Cases in 2019") +
  theme_bw()

##Negative Binomial regression
leaving_deaths_nb <- glm.nb(Deaths ~ HH_leaving, data = tot_idp_gov)
summary(leaving_deaths_nb)

leaving_deathsPopDens_nb <- glm.nb(Deaths ~ HH_leaving + PopDensity, data = tot_idp_gov)
summary(leaving_deathsPopDens_nb)

leaving_cases_nb <- glm.nb(S.Cases ~ HH_leaving, data = tot_idp_gov)
summary(leaving_cases_nb)

leaving_casesPopDens_nb <- glm.nb(S.Cases ~ HH_leaving + PopDensity, data = tot_idp_gov)
summary(leaving_casesPopDens_nb)

m2 <- glm.nb(Deaths ~ HH_leaving + PopDensity, data = tot_idp_gov)
summary(m2)

colnames(tot_idp) <- c("gov", "HH_leaving", "HH_arriving", "totCases_19")
tot_idp$totCases_19 <- as.numeric(tot_idp$totCases_19)

#Plot of people fleeing a Governorate vs. Total 2019 cases (WHO)
ggplot(data = tot_idp_gov, aes(x = log(HH_leaving), y = S.Cases)) +
  geom_point()
ggplot(data = tot_idp_gov, aes(x = log(HH_leaving), y = log(Deaths))) +
  geom_point()


ggplot(data = tot_idp, aes(x = HH_arriving, y = totCases_19)) +
  geom_point()


#Estimated Imported Infections for each governorate
tot_idp_gov <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/tot_idp_gov.RDS")


#Initialise df
EstImpInf_byGov <- data.frame()
for(i in 1:21){
  gov <- tot_idp_gov$Gov[i]
  gov_idp <- IDPs %>% filter(Governorate == gov) #Filter people going to `gov`
  gov_idp <- gov_idp[which(gov_idp$`Total # of HH` > 0),] #Remove negative HH numbers (assume data error)
  idp_agg1 <- aggregate(gov_idp$`Total # of HH`, by = list(gov_idp$`Governorate of Origin`, gov_idp$month), 
                        FUN = sum) #Ignore districts. Aggregate (sum) items with matching origin Governorate and month
  colnames(idp_agg1) <- c("gov_orig", "month", "HH")
  idp_agg1$origCholProb <- rep(0, nrow(idp_agg1))
  for(j in 1:nrow(idp_agg1)){ #The probability that any given person coming from the district is likely to have cholera
    gov_month <- WHO19_monthlyGov_per %>%
      filter(month == idp_agg1$month[j]) %>%
      filter(Governorate == idp_agg1$gov_orig[j])
    tryCatch(idp_agg1$origCholProb[j] <- (gov_month[4] %>% pull()) / 100000,
             error = function(e) {print(paste("Error! Gov_ent:", gov,
                                              "Month:",idp_agg1$month[j],
                                              "Gov_orig:",idp_agg1$gov_orig[j],
                                              "ij = ",i,j))})
  }
  idp_agg1$estInf <- idp_agg1$HH * idp_agg1$origCholProb * 6.7 #Average persons per HH = 6.7
  idp_agg2 <- aggregate(idp_agg1$estInf, by = list(idp_agg1$month), FUN = sum)
  colnames(idp_agg2) <- c("month", "estInf")
  idp_agg2$gov <- rep(gov, nrow(idp_agg2))
  EstImpInf_byGov <- rbind(EstImpInf_byGov, idp_agg2)
}



#Estimated Imported Infections for each District
#Initialise df
EstImpInf_byDist <- data.frame()
i = 4
j = 1
for(i in 1:20){
  gov <- tot_idp_gov$Gov[i] # The receiving governorate
  gov_idp <- IDPs %>% filter(Governorate == gov)
  gov_idp <- gov_idp[gov_idp$`Total # of HH`>0,] #Remove all negative numbers. Assume error in dataset. 
  idp_distMonthAgg <- aggregate(gov_idp$`Total # of HH`, by = list(gov_idp$`Governorate of Origin`, 
                                                                   gov_idp$`District of Origin`, gov_idp$month), 
                                FUN = sum) # Aggregate (sum) items by origin Governorate, origin district and month for HH entering Governorate [i]
  colnames(idp_distMonthAgg) <- c("gov_orig","dist_orig", "month", "HH")
  idp_distMonthAgg <- cbind(gov_rec = gov, idp_distMonthAgg)
  idp_distMonthAgg$origCholProb <- rep(0, nrow(idp_distMonthAgg))
  
  for(j in 1:nrow(idp_distMonthAgg)){ #The probability that any given person coming from the country is likely to have cholera
    orig_dist_month <- WHO19_monthly_distPer %>%
      filter(month == idp_distMonthAgg$month[j]) %>%
      filter(Governorate == idp_distMonthAgg$gov_orig[j]) %>%
      filter(District == idp_distMonthAgg$dist_orig[j])
    tryCatch(idp_distMonthAgg$origCholProb[j] <- (orig_dist_month[5] %>% pull()) / 100000,
             error = function(e) {print(paste("Error! Gov_ent:", gov,
                                              "Month:",idp_distMonthAgg$month[j],
                                              "  Gov_orig:",idp_distMonthAgg$gov_orig[j],
                                              "  Dist_orig:",idp_distMonthAgg$dist_orig[j],
                                              "ij = ",i,j))})
  }
  idp_distMonthAgg$estInf <- idp_distMonthAgg$HH * idp_distMonthAgg$origCholProb * 6.7 #Average persons per HH = 6.7
  idp_distMonthAgg <- idp_distMonthAgg[!is.na(idp_distMonthAgg$origCholProb),] #Remove NaNs. Not quite sure why they're there. I think difference in names
  idp_MonthAgg <- aggregate(idp_distMonthAgg$estInf, by = list(idp_distMonthAgg$month), FUN = sum)
  colnames(idp_MonthAgg) <- c("month", "estInf")
  idp_MonthAgg$gov <- rep(gov, nrow(idp_MonthAgg))
  EstImpInf_byDist <- rbind(EstImpInf_byDist, idp_MonthAgg)
}

#Combine EstImpInf_byGov and EstImpInf_byDist to compare
EstImpInf_both <- merge(EstImpInf_byGov, EstImpInf_byDist, by.x = c("month","gov"), by.y = c("month","gov"))
colnames(EstImpInf_both) <- c("Month","Governorate","byGov","byDist")

ggplot(data = EstImpInf_both) +
  geom_line(aes(x = Month, y = byGov, col = Governorate))
ggplot(data = EstImpInf_both) +
  geom_line(aes(x = Month, y = byDist, col = Governorate))

#example AlHudaydah
AlHud_estImp <- EstImpInf_both %>% filter(Governorate == "AlHudaydah")
ggplot(data = AlHud_estImp, aes(x = Month)) +
  geom_line(aes(y = byGov, col = "byGov")) +
  geom_line(aes(y = byDist, col = "byDist"))

#example Ibb
Ibb_estImp <- EstImpInf_both %>% filter(Governorate == "Ibb")
ggplot(data = Ibb_estImp, aes(x = Month)) +
  geom_line(aes(y = byGov, col = "byGov")) +
  geom_line(aes(y = byDist, col = "byDist"))

#example Ibb
Ibb_estImp <- EstImpInf_both %>% filter(Governorate == "Ibb")
ggplot(data = Ibb_estImp, aes(x = Month)) +
  geom_line(aes(y = byGov, col = "byGov")) +
  geom_line(aes(y = byDist, col = "byDist"))

#example Hajjah
Hajjah_estImp <- EstImpInf_both %>% filter(Governorate == "Hajjah")
ggplot(data = Hajjah_estImp, aes(x = Month)) +
  geom_line(aes(y = byGov, col = "byGov")) +
  geom_line(aes(y = byDist, col = "byDist"))



##Merge cholera data and EstImpInf_byGov
EstImpInf_annual <- EstImpInf_byGov %>% group_by(gov) %>% summarise(ImpCases = sum(estInf))
EstImpInfxCases <- merge(WHO19_govTot, EstImpInf_annual, by.x = "Governorate", by.y = "gov")
EstImpInfxCases$S.Cases <- EstImpInfxCases$S.Cases %>% as.integer
EstImpInfxCases$ImpCases <- EstImpInfxCases$ImpCases %>% as.integer

png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/EstImpVsCases_gov.png", width = 600, height = 400)
ggplot(data = EstImpInfxCases, aes(x = ImpCases, y = S.Cases)) +
  geom_point() +
  labs(x = "Estimated Imported Cases", y = "Suspected Cases") +
  theme_bw()
dev.off()

negbin <- glm.nb(S.Cases ~ ImpCases, data = EstImpInfxCases)
lm <- lm(S.Cases ~ ImpCases, data = EstImpInfxCases)
summary(negbin)
summary(lm)

##Merge cholera data and EstImpInf_byDist
EstImpInf_annual.dist <- EstImpInf_byDist %>% group_by(gov) %>% summarise(ImpCases = sum(estInf))
EstImpInfxCases.dist <- merge(WHO19_govTot, EstImpInf_annual.dist, by.x = "Governorate", by.y = "gov")
EstImpInfxCases.dist$S.Cases <- EstImpInfxCases.dist$S.Cases %>% as.integer
EstImpInfxCases.dist$ImpCases <- EstImpInfxCases.dist$ImpCases %>% as.integer


ggplot(data = EstImpInfxCases.dist, aes(x = ImpCases, y = S.Cases)) +
  geom_point() +
  labs(x = "Estimated Imported Cases", y = "Suspected Cases") +
  theme_bw()
dev.off()

negbin.dist <- glm.nb(S.Cases ~ ImpCases, data = EstImpInfxCases.dist)
lm.dist <- lm(S.Cases ~ ImpCases, data = EstImpInfxCases)
summary(negbin.dist)
summary(lm.dist)
###To conclude, aggregating by district is less helpful than aggregating just by Gov. So we'll now ignore aggregating by district

##Just to check it was useful, we'll do suspected cases vs. incoming idps.
png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/HHAriveVsCases_gov.png", width = 600, height = 400)
ggplot(data = tot_idp_gov, aes(x = HH_arrive, y = S.Cases)) +
  geom_point() +
  labs(x = "Households arriving in Governorate", y = "Suspected Cases") +
  theme_bw()
dev.off()

png(file = "C:/Users/dms228/github/cholera-in-yemen/plots/idp/HHAriveVsDeaths_gov.png", width = 600, height = 400)
ggplot(data = tot_idp_gov, aes(x = HH_arrive, y = Deaths)) +
  geom_point() +
  labs(x = "Households arriving in Governorate", y = "Cholera deaths") +
  theme_bw()
dev.off()

lm.HH <- lm(S.Cases ~ HH_arrive, data = tot_idp_gov)
lm.HH.deaths <- lm(Deaths ~ HH_arrive, data = tot_idp_gov)
summary(lm.HH)
summary(lm.HH.deaths)
#I'm not actually adding any new code here. Just a comment to test GitHub

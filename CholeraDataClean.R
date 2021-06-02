#WHO_weekly
library(lubridate)
library(dplyr)
library(plyr)
library(stats)
library(rlist) #list.cbind
library(ggplot2)
library(magrittr)
#The data comes from here: https://app.powerbi.com/view?r=eyJrIjoiNTY3YmU0NTItMmFjYy00OTUxLWI2NzEtOTU5N2Q0MDBjMjE5IiwidCI6ImI3ZTNlYmJjLTE2ZTctNGVmMi05NmE5LTVkODc4ZDg3MDM5ZCIsImMiOjl9
#NOTE: they stopped collecting data for some time in 2017 at the end of the first wave.
#NOTE2: For some period in 2018, they changed the definition of 'cholera case' from suspected case to confirmed case
#The data is cumulative starting from 27/04/2017

#This loop takes each raw file, removes the apostrophes, then saves it as a new .txt file in the folder NoApo. It then
#reads in the new .txt file, converts each into a dataframe, removes the Arabic. Each week is a dataframe item
#within a list. 
data_list <- vector(mode = "list", length = 193)
date_list <- vector(length =193)
date <- as.Date("2017/04/30")
for(i in 1:193){
  filename_date <- paste(year(date) - 2000, month(date), day(date), sep = "_")
  filename_raw <- paste("C:/Users/dms228/Downloads/Yemen Data/WHO_weekly/",filename_date, sep = "")
  text_raw <- readLines(paste("C:/Users/dms228/Downloads/Yemen Data/WHO_weekly/",filename_date, sep = ""))
  txt_cln <- gsub("'","",text_raw) #Remove all apostophes from the text
  cln_filename <- paste("C:/Users/dms228/Downloads/Yemen Data/WHO_weekly/NoApo/",filename_date,".txt", sep = "")
  fileConn <- file(cln_filename)
  writeLines(txt_cln, fileConn)
  close(fileConn)
  raw_data <- read.table(cln_filename, fill = TRUE, header = TRUE, sep = "\t", encoding = 'UTF-8')
  raw_data[,1:2] <- as.data.frame(apply(raw_data[,1:2],2, function(x) {gsub("[^[:alnum:]]", "", x)})) #Removes Arabic, and spaces
  raw_data[,4:5] <- as.data.frame(apply(raw_data[,4:5],2, function(x) {gsub("[(Blank)]", 0, x)})) #Convert (Blank) to 0
  raw_data[,4:5] <- as.data.frame(apply(raw_data[,4:5],2, function(x) {as.numeric(x)}))
  raw_data$Date <- date
  data_list[[i]] <- raw_data
  date_list[i] <- date
  date <- date + 7
}
data <- rbind.fill(data_list)

#Convert cumulative data into weekly data
all_locations <- data %>% filter(Date == date_list[193]) %>% select(c(1,2))
WHO_weekly <- data.frame(matrix(nrow = (nrow(all_locations) - 1) * 193 , ncol = 8))
i = 1
j = 192
for(i in 1:nrow(all_locations)){
  for(j in 1:(length(date_list) - 1)){
    date_current <- date_list[j+1] #week i
    date_last <- date_list[j] #previous week
    gov <- all_locations$Governorate[i]
    dist <- all_locations$District[i]
    CFR <- data %>% filter(Date == date_current & Governorate == gov & District == dist) %>% select(CFR) %>% pull() #Case Fatality Ratio in current week
    if (length(CFR) == 0){
      CFR <- NA #If there were no entries for that combination of date and location, return NA
    }
    current_week <- data %>% filter(Date == date_current & Governorate == gov & District == dist) %>% select(c(3,4,6,7)) %>% as.numeric()
    last_week <- data %>% filter(Date == date_last & Governorate == gov & District == dist) %>% select(c(3,4,6,7)) %>% as.numeric()
    current_week[is.na(current_week)] <- 0
    last_week[is.na(last_week)] <- 0
    net <- current_week - last_week
    row_num <- (i-1)*192 + j
    WHO_weekly[row_num,] <- c(gov, dist, date_current, net, CFR)
  }
}

colnames(WHO_weekly) <- c("Governorate", "District", "Date", "S.Cases", "Deaths","RDT","Under5","CFR")
WHO_weekly[,4:8] <- sapply(WHO_weekly[,4:8], as.numeric) %>% data.frame()
WHO_weekly$Governorate[WHO_weekly$Governorate == "Moklla"] <- "Hadramawt"
WHO_weekly$Governorate[WHO_weekly$Governorate == "Sayon"] <- "Hadramawt"
WHO_weekly <- WHO_weekly[1:65280,]
##WHO_weekly_govs

gov_agg <- function(x) {aggregate(x, by = list(WHO_weekly$Governorate, WHO_weekly$Date), FUN = sum)}
yem_agg <- function(x) {aggregate(x, by = list(WHO_weekly$Date), FUN = sum)}

WHO_weekly_gov_raw <- apply(WHO_weekly[,4:7],2, gov_agg)
WHO_weekly_gov <- list.cbind(WHO_weekly_gov_raw) %>% select(c(1,2,3,6,9,12))
colnames(WHO_weekly_gov) <- c("Governorate", "Date", "S.Cases", "Deaths", "RDT", "Under5") #weekly numbers aggregated to Governorate level
WHO_weekly_yem_raw <- apply(WHO_weekly[,4:7],2, yem_agg)
WHO_weekly_yem <- list.cbind(WHO_weekly_yem_raw) %>% select(c(1,2,4,6,8))
colnames(WHO_weekly_yem) <- c("Date", "S.Cases", "Deaths", "RDT", "Under5") #weekly numbers aggregated at Yemen level

###weekly numbers aggregated to District level
WHO_weekly$Date <- WHO_weekly$Date %>% as.numeric() %>% as.POSIXct.Date() %>% as.Date()
saveRDS(WHO_weekly, "C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_dist.RDS")
#WHO_weekly <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_dist.RDS")

###weekly numbers aggregated to Governorate level
WHO_weekly_gov$Date <- WHO_weekly_gov$Date %>% as.numeric() %>% as.POSIXct.Date() %>% as.Date()
saveRDS(WHO_weekly_gov, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_gov.RDS")
#WHO_weekly_gov <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_gov.RDS")

###weekly numbers aggregated to National level#
WHO_weekly_yem$Date <- WHO_weekly_yem$Date %>% as.numeric() %>% as.POSIXct.Date() %>% as.Date()
saveRDS(WHO_weekly_yem, "C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_yem.RDS")
#WHO_weekly_yem <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_yem.RDS")


#At District level
WHO_weekly$Month <- month(WHO_weekly$Date)
WHO_monthly <- WHO_weekly %>% group_by(Month,Year,Governorate,District) %>% 
  summarise(across(c(S.Cases,Deaths), mean, na.rm = TRUE))
WHO_monthly[,5:6] <- WHO_monthly[,5:6] * (30/7)  
WHO_monthly %<>% mutate(Months_since = Month + 12*(Year - 2017))
WHO_monthly %<>% mutate(Months_since_lag1 = Months_since - 1)
#saveRDS(WHO_monthly,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthly.RDS")

#At Governorate level
WHO_weekly_gov$Month <- month(WHO_weekly_gov$Date)
WHO_weekly_gov$Year <- year(WHO_weekly_gov$Date)
WHO_monthly_gov <- WHO_weekly_gov %>% group_by(Month,Year,Governorate) %>% 
  summarise(across(c(S.Cases,Deaths), mean, na.rm = TRUE))
WHO_monthly_gov[,4:5] <- WHO_monthly_gov[,4:5] * (30/7)  
WHO_monthly_gov %<>% mutate(Months_since = Month + 12*(Year - 2017))
WHO_monthly_gov %<>% mutate(Months_since_lag1 = Months_since - 1)
saveRDS(WHO_monthly_gov,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthly_gov.RDS")


#################Cholera data per 100,000 inhabitants
#At district level
yemen_pop <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/yemen_popData.RDS")
pop_cases_joined <- left_join(WHO_weekly, yemen_pop, by = c("Governorate", "District"))

i = 2017
WHO_weekly_per <- data.frame(col_name=character(0),unique_cnt=integer(0))
for(i in 2017:2020){
  year <- pop_cases_joined %>% filter(Year == i) 
  year_ch <- as.character(i)
  cases_per <- (year[,4:7] / year[,year_ch])*100000 
  cases_per_df <- cbind(year[,1:3],cases_per)
  WHO_weekly_per <- rbind(WHO_weekly_per, cases_per_df)
}

#At Governorate Level
yemen_pop_govAgg <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/yemen_pop_govAgg.RDS")
WHO_weekly_gov$Year <- year(WHO_weekly_gov$Date)
pop_cases_gov_joined <- dplyr::left_join(WHO_weekly_gov, yemen_pop_govAgg, by = "Governorate")

WHO_weeklyGov_per <- data.frame(col_name=character(0),unique_cnt=integer(0))
for(i in 2017:2020){
  year <- pop_cases_gov_joined %>% filter(Year == i) 
  year_ch <- as.character(i)
  cases_per <- (year[,3:6] / year[,year_ch])*100000 
  cases_per_df <- cbind(year[,1:2],cases_per)
  WHO_weeklyGov_per <- rbind(WHO_weeklyGov_per, cases_per_df)
}

##At National Level
yemen_pop_single <- yemen_pop_govAgg %>% summarise(across(`2017`:`2020`,sum))

WHO_weeklyYem_per <- data.frame(col_name=character(0),unique_cnt=integer(0))
for(i in 2017:2020){
  year <- WHO_weekly_yem %>% filter(Year == i)
  pop <- yemen_pop_single[1,4] %>% pull()
  cases_per <- (year[,2:5] / pop) * 100000
  cases_per_df <- cbind(Date = year[,1],cases_per)
  WHO_weeklyYem_per <- rbind(WHO_weeklyYem_per, cases_per_df)
}


#######SaveRDS##############
saveRDS(WHO_weekly_per,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_per.RDS")
WHO_weekly_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_per.RDS")
saveRDS(WHO_weeklyGov_per,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weeklyGov_per.RDS")
WHO_weeklyGov_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weeklyGov_per.RDS")
saveRDS(WHO_weeklyYem_per,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weeklyYem_per.RDS")
WHO_weeklyYem_per <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weeklyYem_per.RDS")

#############################
##Monthly Cases and Deaths per 100,000 population normalised to a 30 day month - At district level
WHO_weekly_per$month <- month(WHO_weekly_per$Date)
WHO_weekly_per$year <- year(WHO_weekly_per$Date)
WHO_monthly_per <- WHO_weekly_per %>% group_by(month,year,Governorate,District) %>% dplyr::summarise(across(c(S.Cases,Deaths), mean, na.rm = TRUE))
WHO_monthly_per[,5:6] <- WHO_monthly_per[,5:6] * (30/7)  
WHO_monthly_per %<>% mutate(months_since = month + 12*(year - 2017))
WHO_monthly_per %<>% mutate(months_since_lag1 = months_since - 1)
saveRDS(WHO_monthly_per,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthly_per.RDS")

##Monthly Cases and Deaths per 100,000 population normalised to a 30 day month - Aggregated at Governorate level
WHO_weeklyGov_per$month <- month(WHO_weeklyGov_per$Date)
WHO_weeklyGov_per$year <- year(WHO_weeklyGov_per$Date)
WHO_monthlyGov_per <- WHO_weeklyGov_per %>% group_by(month,year,Governorate) %>% dplyr::summarise(across(c(S.Cases,Deaths), mean, na.rm = TRUE))
WHO_monthlyGov_per[,4:5] <- WHO_monthlyGov_per[,4:5] * (30/7)  
WHO_monthlyGov_per %<>% mutate(months_since = month + 12*(year - 2017))
WHO_monthlyGov_per %<>% mutate(months_since_lag1 = months_since - 1)
saveRDS(WHO_monthlyGov_per,"C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_monthlyGov_per.RDS")

######### Attempt to estimate R Number
##example with Hajjah
WHO_weekly_gov <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_gov.RDS")
Hajjah_incid <- WHO_weekly_gov %>% filter(Governorate == "Hajjah") %>% select(c(Date, S.Cases))
colnames(Hajjah_incid) <- c("dates","I")

Hajjah_incid$dates <- Hajjah_incid$dates %>% as.Date()
datesH <- Hajjah_incid$dates
IH <- Hajjah_incid$I

#specify a 4-week sliding window
t_start <- seq(2, nrow(Hajjah_incid)-4)
t_end <- t_start + 4

R <- estimate_R(Hajjah_incid, method = "parametric_si", config = make_config(list(mean_si = 5, std_si = 8,
                                                                                  t_start = t_start, t_end = t_end)))
##To conclude, estimate_R does not work with weekly data
###Epicurves!
#Yemen
ggplot(data = WHO_weekly_yem, aes(x = Date)) +
  geom_line(aes(y = S.Cases)) +
  geom_line(aes(y = (Deaths*1000)), col = "blue")

#All Governorates
WHO_weekly_gov %>% ggplot(aes(x = Date, y = S.Cases, group = Governorate, color = Governorate)) +
  geom_line()

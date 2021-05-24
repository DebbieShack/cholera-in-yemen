###Multi_climate_yemen
library(ncdf4)
library(ggplot2)
library(raster)
library(dplyr)
library(reshape2)
library(rgdal) #readOGR
library(sp)
library(lubridate)
library(magrittr)
library(RColorBrewer)
library(janitor)
theme_set(theme_bw())

#Copernicus Data taken from 
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview

file <- "C:/Users/dms228/Downloads/Yemen Data/Copernicus/yemen_pr_sst_t2m_4y.nc"
nc_yemen <- nc_open(file)

shape_file_yemen <- "C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm0_govyem_cso_20191002.shp"
shape_yemen <- readOGR(shape_file_yemen)

shape_file_govs <- "C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm1_govyem_cso_20191002.shp"
shape_govs <- readOGR(shape_file_govs)
gov_names <- shape_govs$ADM1_EN

shape_dists <- readOGR("C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm2_govyem_cso_20191002.shp")
dist_names <- shape_dists$ADM2_EN
dist_govs <- shape_dists$ADM1_EN
dist_df <- data.frame(Governorate = dist_govs, District = dist_names)

######Get Values#######
precip <- ncvar_get(nc_yemen,"tp_0001")
temp <- ncvar_get(nc_yemen, "t2m_0001")
#sst <- ncvar_get(nc_yemen, "sst_0001")

######Get Dimensions#####
lon <- ncvar_get(nc_yemen, "longitude")
lat <- ncvar_get(nc_yemen, "latitude")
time <- ncvar_get(nc_yemen, "time")

#####Now melt them down####
precip_melt <- melt(precip)
temp_melt <- melt(temp)


#####Correct Dimensions###
real_dims <- expand.grid(lon,lat,time)
time_ftd <- as.POSIXct(real_dims[,3]*3600,origin = "1900-01-01 00:00:00")
dates <- as_date(time_ftd)
#weeks <- weeks(time_ftd)
#d1 <- time_ftd[1]
#weeks_since <- as.numeric(floor(difftime(time_ftd,d1, units = "weeks"))) + 1 #Week number starting 2017-01-01
#years <- year(time_ftd)
#months <- month(time_ftd)
temps_celsius <- temp_melt[,4] - 273.15




corr_precip <- cbind(precip_melt[,4], real_dims[,c(1,2)], dates)
colnames(corr_precip) <- c("precip", "lon", "lat", "date")
corr_temp <- cbind(temps_celsius, real_dims[,c(1,2)], dates)
colnames(corr_temp) <- c("temp", "lon", "lat", "date")

####Aggregate by date
temp_daily <- aggregate(corr_temp$temp, by = list(corr_temp$lon, corr_temp$lat, corr_temp$date), FUN = mean)
colnames(temp_daily) <- c("lon", "lat", "date", "temp")
precip_daily <- aggregate(corr_precip$precip, by = list(corr_precip$lon, corr_precip$lat, corr_precip$date), FUN = sum)
colnames(precip_daily) <- c("lon", "lat", "date", "precip")

#temp_df <- cbind(real_dims[,c(1,2)],temps_celsius, dates)
#colnames(temp_df) <- c("lon", "lat", "temp", "date")
#temp_daily <-  aggregate(temp_df$temp, by = list(temp_df$lon, temp_df$lat, temp_df$date), FUN = mean)
#colnames(temp_daily) <- c("lon", "lat", "date", "temp")

unique_dates <- unique(temp_daily$date)

daily_temp_rasters <- vector(mode = "list", length = length(unique_dates))
daily_precip_rasters <- vector(mode = "list", length = length(unique_dates))

for(i in 1:length(unique_dates)){
  df <- temp_daily[which(temp_daily$date == unique_dates[i]),]
  df <- df[-c(3)]
  daily_temp_rasters[[i]] <- rasterFromXYZ(df)
}

for(i in 1:length(unique_dates)){
  df <- precip_daily[which(precip_daily$date == unique_dates[i]),]
  df <- df[-c(3)]
  daily_precip_rasters[[i]] <- rasterFromXYZ(df)
}



# i = date, j = region
mean_temp <- data.frame(matrix(nrow = length(unique_dates), ncol = 23))

for(i in 1:length(unique_dates)){
  print(i)
  for(j in 1:23){
    if(j == 23){
      single_gov <- shape_yemen
    } else{
      single_gov <- shape_govs[j,]
    }
    extent_square <- crop(daily_temp_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    mean_temp[i,j] <- mean(gov_ar[,3])
  }
}
colnames(mean_temp) <- c(gov_names, "Yemen")
rownames(mean_temp) <- unique_dates

mean_precip <- data.frame(matrix(nrow = length(unique_dates), ncol = 23))

for(i in 1:length(unique_dates)){
  for(j in 1:23){
    if(j == 23){
      single_gov <- shape_yemen
    } else{
      single_gov <- shape_govs[j,]
    }
    extent_square <- crop(daily_precip_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    mean_precip[i,j] <- mean(gov_ar[,3])
  }
}
colnames(mean_precip) <- c(gov_names, "Yemen")
rownames(mean_precip) <- unique_dates

mean_precip <- mean_precip * 1000

#saveRDS(mean_temp_daily, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/mean_temp_daily.RDS")

colnames(mean_temp) <- c(gov_names, "Yemen")
mean_temp$Date <- rownames(mean_temp) %>% as.POSIXct(format = "%Y-%m-%d") 

colnames(mean_precip) <- c(gov_names, "Yemen")
mean_precip$Date <- rownames(mean_precip) %>% as.POSIXct(format = "%Y-%m-%d") 

##Change gov names to match WHO Weekly

mean_temp <- mean_temp %>%
  dplyr::rename(AmanatAlAsimah =`Sana'a City`) %>%
  dplyr::rename(AlBayda =`Al Bayda`) %>%
  dplyr::rename(Taizz =`Ta'iz`) %>%
  dplyr::rename(AlJawf =`Al Jawf`) %>%
  dplyr::rename(AlHudaydah =`Al Hodeidah`) %>%
  dplyr::rename(Saada =`Sa'dah`) %>%
  dplyr::rename(Marib =`Ma'rib`) %>%
  dplyr::rename(AlMahwit =`Al Mahwit`) %>%
  dplyr::rename(AlMaharah = `Al Maharah`) %>%
  dplyr::rename(AlDhalee = `Ad Dali'`) %>%
  dplyr::rename(Sanaa = `Sana'a`)

mean_precip <- mean_precip %>%
  dplyr::rename(AmanatAlAsimah =`Sana'a City`) %>%
  dplyr::rename(AlBayda =`Al Bayda`) %>%
  dplyr::rename(Taizz =`Ta'iz`) %>%
  dplyr::rename(AlJawf =`Al Jawf`) %>%
  dplyr::rename(AlHudaydah =`Al Hodeidah`) %>%
  dplyr::rename(Saada =`Sa'dah`) %>%
  dplyr::rename(Marib =`Ma'rib`) %>%
  dplyr::rename(AlMahwit =`Al Mahwit`) %>%
  dplyr::rename(AlMaharah = `Al Maharah`) %>%
  dplyr::rename(AlDhalee = `Ad Dali'`) %>%
  dplyr::rename(Sanaa = `Sana'a`)


saveRDS(mean_temp, "C:/Users/dms228/github/cholera-in-yemen/saved_data/daily_temp.RDS")
#mean_temp <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/mean_temp.RDS")
saveRDS(mean_precip, "C:/Users/dms228/github/cholera-in-yemen/saved_data/daily_precip.RDS")

####Aggregate temp by week
daily_temp_long <- melt(mean_temp, id = "Date") %>% set_colnames(c("date", "region", "temp"))
d1 <- daily_temp_long$date[1]
daily_temp_long$weeks_since <- as.numeric(floor(difftime(daily_temp_long$date,d1, units = "weeks"))) + 1 #Week number starting 2017-01-01

weekly_temp <- aggregate(daily_temp_long$temp, 
                         by = list(daily_temp_long$region, daily_temp_long$weeks_since), 
                         FUN = mean) %>%
  set_colnames(c("gov", "weeks_since", "temp"))

weekly_temp_yem <- aggregate(weekly_temp$temp, by = list(weekly_temp$weeks_since), FUN = mean) %>% 
  set_colnames(c("weeks_since", "temp"))
weekly_dates <- seq(from = as.Date("2017-01-01"), by = "week", length.out = nrow(weekly_precip_yem)) 
weekly_temp_yem$date <- weekly_dates
weekly_temp$date <- rep(weekly_dates, each = 23)

saveRDS(weekly_temp, "C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_temp.RDS")
saveRDS(weekly_temp_yem, "C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_temp_yem.RDS")
weekly_temp <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_temp.RDS")
####Aggregate precip by week

daily_precip_long <- melt(mean_precip, id = "Date") %>% set_colnames(c("date", "region", "precip"))
d1 <- daily_precip_long$date[1]
daily_precip_long$weeks_since <- as.numeric(floor(difftime(daily_precip_long$date,d1, units = "weeks"))) + 1 #Week number starting 2017-01-01

weekly_precip <- aggregate(daily_precip_long$precip, 
                           by = list(daily_precip_long$region, daily_precip_long$weeks_since), 
                           FUN = sum) %>%
  set_colnames(c("gov", "weeks_since", "precip"))

weekly_precip_yem <- aggregate(weekly_precip$precip, by = list(weekly_precip$weeks_since), FUN = mean) %>% 
  set_colnames(c("weeks_since", "precip"))


weekly_precip_yem$date <- weekly_dates
weekly_precip$date <- rep(weekly_dates, each = 23)

saveRDS(weekly_precip_yem, "C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_precip_yem.RDS")
saveRDS(weekly_precip, "C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_precip.RDS")
#weekly_precip <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/weekly_precip.RDS")



###Multi_climate_yemen
library(ncdf4)
library(ggplot2)
library(raster)
#library(zoo)
library(dplyr)
library(reshape2)
library(rgdal) #readOGR
library(sp)
#library(rgeos)
#library(rnaturalearth)
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
#sst_melt <- melt(sst)


#####Correct Dimensions###
real_dims <- expand.grid(lon,lat,time)
time_ftd <- as.POSIXct(real_dims[,3]*3600,origin = "1900-01-01 00:00:00")
dates <- as_date(time_ftd)
weeks <- weeks(time_ftd)
d1 <- time_ftd[1]
weeks_since <- as.numeric(floor(difftime(time_ftd,d1, units = "weeks"))) + 1 #Week number starting 2017-01-01
years <- year(time_ftd)
months <- month(time_ftd)
temps_celsius <- temp_melt[,4] - 273.15




corr_precip <- cbind(precip_melt[,4], real_dims[,c(1,2)], dates, weeks, weeks_since, months, years)
colnames(corr_precip) <- c("precip", "lon", "lat", "date", "week","weeks_since","month","year")
corr_temp <- cbind(temps_celsius, real_dims[,c(1,2)], dates, weeks, weeks_since, months, years)
colnames(corr_temp) <- c("temp", "lon", "lat", "date", "week","weeks_since", "month","year")

####Aggregate by date
temp_df <- cbind(real_dims[,c(1,2)],temps_celsius, dates)
colnames(temp_df) <- c("lon", "lat", "temp", "date")
temp_daily <-  aggregate(temp_df$temp, by = list(temp_df$lon, temp_df$lat, temp_df$date), FUN = mean)
colnames(temp_daily) <- c("lon", "lat", "date", "temp")

unique_dates <- unique(temp_daily$date)
daily_temp_rasters <- vector(mode = "list", length = 1413)


i = 4
j = 1
for(i in 1:1413){
  df <- temp_daily[which(temp_daily$date == unique_dates[i]),]
  df <- df[-c(3)]
  daily_temp_rasters[[i]] <- rasterFromXYZ(df)
}

# i = date, j = region
mean_temp_daily <- data.frame(matrix(nrow = 1413, ncol = 23))

for(i in 1:1413){
  for(j in 1:23){
    if(j == 23){
      single_gov <- shape_yemen
    } else{
      single_gov <- shape_govs[j,]
    }
    extent_square <- crop(daily_temp_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    mean_temp_daily[i,j] <- mean(gov_ar[,3])
  }
}
colnames(mean_temp_daily) <- c(gov_names, "Yemen")
rownames(mean_temp_daily) <- unique_dates
saveRDS(mean_temp_daily, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/mean_temp_daily.RDS")

colnames(mean_temp) <- c(gov_names, "Yemen")
mean_temp$Date <- rownames(mean_temp) %>% as.POSIXct(format = "%Y-%m-%d") 

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
  dplyr::rename(AlDhalee = `Ad Dali'`) 

sum_precip <- sum_precip %>%
  dplyr::rename(AmanatAlAsimah =`Sana'a City`) %>%
  dplyr::rename(AlBayda =`Al Bayda`) %>%
  dplyr::rename(Taizz =`Ta'iz`) %>%
  dplyr::rename(AlJawf =`Al Jawf`) %>%
  dplyr::rename(AlHudaydah =`Al Hodeidah`) %>%
  dplyr::rename(Saada =`Sa'dah`) %>%
  dplyr::rename(Marib =`Ma'rib`) %>%
  dplyr::rename(AlMahwit =`Al Mahwit`) %>%
  dplyr::rename(AlMaharah = `Al Maharah`) %>%
  dplyr::rename(AlDhalee = `Ad Dali'`) 


setwd("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data")
saveRDS(mean_temp, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/mean_temp.RDS")
#mean_temp <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/mean_temp.RDS")
saveRDS(sum_precip, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_precip.RDS")

####Aggregate by week
precip_weekly <- aggregate(corr_precip$precip, 
                           by = list(corr_precip$lon, corr_precip$lat, corr_precip$weeks_since, corr_precip$year), 
                           FUN = sum)
temp_weekly <- aggregate(corr_temp$temp, 
                         by = list(corr_temp$lon, corr_temp$lat, corr_temp$weeks_since, corr_temp$year), 
                         FUN = mean)
colnames(temp_weekly) <- c("lon", "lat", "weeks_since","year", "temp")
colnames(precip_weekly) <- c("lon", "lat", "weeks_since", "year","precip")

# setwd("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data")
# saveRDS(precip_weekly, 'precip_weekly.RDS')
# saveRDS(temp_weekly, 'temp_weekly.RDS')
# 
# precip_weekly <- readRDS('precip_weekly.RDS')
# temp_weekly <- readRDS('temp_weekly.RDS')

###Prepare raster lists for temp and precipitation datasets
all_temp_dfs <- vector(mode = "list", length = 202)
all_temp_rasters <- vector(mode = "list", length = 202)
all_precip_dfs <- vector(mode = "list", length = 202)
all_precip_rasters <- vector(mode = "list", length = 202)

i = 4
for(i in 1:202){
  df <- temp_weekly[which(temp_weekly$weeks_since == i),]
  df <- df[-c(3,4)]
  all_temp_dfs[[i]] <- df
  all_temp_rasters[[i]] <- rasterFromXYZ(df)
}

for(i in 1:202){
  df <- precip_weekly[which(precip_weekly$weeks_since == i),]
  df <- df[-c(3,4)]
  all_precip_dfs[[i]] <- df
  all_precip_rasters[[i]] <- rasterFromXYZ(df)
}

# i = week, j = region
mean_temp <- data.frame(matrix(nrow = 202, ncol = 22))
sum_precip <- data.frame(matrix(nrow = 202, ncol = 22))

for(i in 1:202){
  for(j in 1:22){
    single_gov <- shape_govs[j,]
    extent_square <- crop(all_temp_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    mean_temp[i,j] <- mean(gov_ar[,3])
  }
}

for(i in 1:202){
  for(j in 1:22){
    single_gov <- shape_govs[j,]
    extent_square <- crop(all_precip_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    sum_precip[i,j] <- sum(gov_ar[,3]) * 1000
  }
}


colnames(mean_temp) <- gov_names
colnames(sum_precip) <- gov_names
sum_precip <- dplyr::select(sum_precip, -Socotra)

sum_precip <- adorn_totals(sum_precip, where = "col")
sum_precip$weeks_since <- seq(1:202)
mean_temp$weeks_since <- seq(1:202)
sum_precip$stdate <- ymd("2017-01-01") + weeks(sum_precip$weeks_since - 1)
mean_temp$
  
  setwd("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data")
saveRDS(mean_temp, 'mean_temp.RDS')
saveRDS(sum_precip, 'C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_precip.RDS')



####Aggregate by month
precip_monthly <- aggregate(corr_precip$precip, 
                            by = list(corr_precip$lon, corr_precip$lat, corr_precip$month, corr_precip$year), 
                            FUN = sum)
temp_monthly <- aggregate(corr_temp$temp, 
                          by = list(corr_temp$lon, corr_temp$lat, corr_temp$month, corr_temp$year), 
                          FUN = mean)
colnames(temp_monthly) <- c("lon", "lat", "month","year", "temp")
colnames(precip_monthly) <- c("lon", "lat", "month", "year","precip")
precip_monthly %<>% mutate(months_since = month + 12*(year - 2017))
temp_monthly %<>% mutate(months_since = month + 12*(year - 2017))

###Prepare raster lists for temp and precipitation datasets for monthly data
monthly_temp_dfs <- vector(mode = "list", length = 47)
monthly_temp_rasters <- vector(mode = "list", length = 47)
monthly_precip_dfs <- vector(mode = "list", length = 47)
monthly_precip_rasters <- vector(mode = "list", length = 47)

#Raster data for whole Yemen square
i = 4
for(i in 1:47){
  df <- temp_monthly[which(temp_monthly$months_since == i),]
  df <- df[-c(3,4)]
  monthly_temp_dfs[[i]] <- df
  monthly_temp_rasters[[i]] <- rasterFromXYZ(df)
}


for(i in 1:47){
  df <- precip_monthly[which(precip_monthly$months_since == i),]
  df <- df[-c(3,4)]
  monthly_precip_dfs[[i]] <- df
  monthly_precip_rasters[[i]] <- rasterFromXYZ(df)
}

# i = month, j = region
monthly_mean_temp <- data.frame(matrix(nrow = 47, ncol = 23))
monthly_sum_precip <- data.frame(matrix(nrow = 47, ncol = 23))

for(i in 1:47){
  for(j in 1:22){
    single_gov <- shape_govs[j,]
    extent_square <- crop(monthly_temp_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    monthly_mean_temp[i,j] <- mean(gov_ar[,3])
  }
  yemen <- shape_yemen
  extent_square <- crop(monthly_temp_rasters[[i]], yemen)
  gov_shaped <- mask(extent_square, yemen)
  gov_ar <- rasterToPoints(gov_shaped)
  monthly_mean_temp[i,23] <- mean(gov_ar[,3])
}

for(i in 1:47){
  for(j in 1:22){
    single_gov <- shape_govs[j,]
    extent_square <- crop(monthly_precip_rasters[[i]], single_gov)
    gov_shaped <- mask(extent_square, single_gov)
    gov_ar <- rasterToPoints(gov_shaped)
    monthly_sum_precip[i,j] <- sum(gov_ar[,3]) * 1000
  }
  yemen <- shape_yemen
  extent_square <- crop(monthly_precip_rasters[[i]], yemen)
  gov_shaped <- mask(extent_square, yemen)
  gov_ar <- rasterToPoints(gov_shaped)
  monthly_sum_precip[i,23] <- sum(gov_ar[,3]) * 1000
}




colnames(monthly_mean_temp) <- append(gov_names, "Yemen")
colnames(monthly_sum_precip) <- gov_names




monthly_sum_precip$months_since <- seq(1:47)
monthly_mean_temp$months_since <- seq(1:47)
setwd("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data")
saveRDS(monthly_mean_temp, 'monthly_mean_temp.RDS')
saveRDS(monthly_sum_precip, 'monthly_sum_precip.RDS')
monthly_mean_temp <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/monthly_mean_temp.RDS")
monthly_sum_precip <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/monthly_sum_precip.RDS")

################# Experimental Choropleth ################
wk1 <- data.frame(cbind(append(gov_names, "Yemen"), as.numeric(monthly_mean_temp[1,]), as.numeric(monthly_sum_precip[1,])))
colnames(wk1) <- c("gov", "temp", "precip")

shp_wk1 <- merge(shape_govs, wk1, by.x = "ADM1_EN", by.y = "gov")
shp_wk1

spplot(shp_wk1, "precip")





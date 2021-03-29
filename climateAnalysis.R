library(forecast) #tslm()
library(rgdal) #readOGR()
library(dplyr)
library(ggplot2)
library(cowplot)

WHO_weekly_yem <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_yem.RDS")
WHO_weekly_gov <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_gov.RDS")
WHO_weekly <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_weekly_dist.RDS")
mean_temp <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/daily_mean_temp.RDS")
sum_precip <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_precip.RDS")
shape_govs <- readOGR("C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm1_govyem_cso_20191002.shp")
monthly_sum_expl <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/monthly_sum_expl.RDS")
gov_names <- shape_govs$ADM1_EN
govs_noHa <- gov_names[gov_names != "Hadramawt" & gov_names != "Socotra"] %>% append("Yemen")
pop_2017 <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/eDEWS_2020.RDS")

WHO_weekly_yem$Date <- WHO_weekly_yem$Date %>% as.Date()
WHO_weekly_gov$Date <- WHO_weekly_gov$Date %>% as.Date()
mean_temp$Date <- mean_temp$Date %>% as.Date()

WHO_weekly_yem$Date <- WHO_weekly_yem$Date %>% as.POSIXct()

###Precipitation, Temperature and Cases Epicurve at National Levels ###############
#Multiplot admin
min_date <- min(WHO_weekly_yem$Date) - 21
max_date <- max(WHO_weekly_yem$Date)

##Suspected cholera cases
cases_plt <- ggplot(data = WHO_weekly_yem, aes(x = Date, y = S.Cases)) + 
  geom_bar(stat= "identity") + 
  labs(title = "Suspected Cholera Cases", y = "Suspected Cases") +
  #scale_x_date(date_breaks = "3 months", date_labels = "%b %y", 
  #minor_breaks = "1 month", limits = c(min_date, max_date)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))
cases_plt

##temp
temp_plt <- ggplot(data = mean_temp, aes(x = Date, y = Yemen)) + 
  geom_line() + 
  labs(title = "Temperature", y = "Temp (C)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", 
               minor_breaks = "1 month", limits = c(min_date, max_date)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))
temp_plt

##Precip
precip_plt <- ggplot(data = sum_precip, aes(x = stdate, y = Total)) + 
  geom_line() + 
  labs(title = "Precipitation", x = "Date", y = "Total weekly rainfall (mm)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", 
               minor_breaks = "1 month", limits = c(min_date, max_date)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))
precip_plt

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/WHO_weekly_data/epicurve_wtempprecip.png", width = 1800, height = 900)  
plot_grid(cases_plt, temp_plt, precip_plt, align = "v", ncol = 1)
dev.off()
#################


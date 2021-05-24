##ts_analysis
library(raster)
library(forecast) #tslm()
library(rgdal)
library(magrittr)
library(ggplot2)
library(ggpubr) #ggarrange()
library(cowplot) ##align_plots
library(dplyr)
#library(rlist) #list.append()
daily_temp <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/daily_temp.RDS")
#eDEWS_temp <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/eDEWS_temp.RDS")
#daily_mean_temp <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/daily_mean_temp.RDS")
daily_precip <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/daily_precip.RDS")
WHO_weekly_dist <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_dist.RDS")
WHO_weekly_gov <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_gov.RDS")
WHO_weekly_yem <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_yem.RDS")
#WHO_monthly_cases <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_monthly_cases.RDS")
#WHO_daynorm_cases <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/WHO_daynorm_cases.RDS")
#eDEWS <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/df_full.RDS")
#eDEWS_perCap <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/df_full_perCap.RDS")
shape_govs <- readOGR("C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm1_govyem_cso_20191002.shp")
#sum_expl <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/sum_expl.RDS")
gov_names <- shape_govs$ADM1_EN
pop_data <- "C:/Users/dms228/github/cholera-in-yemen/saved_data/yemen_popData.RDS"
#pop_2017 <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/eDEWS_2020.RDS")

#WHO_weekly_dist <- readRDS("C:/Users/dms228/github/cholera-in-yemen/saved_data/WHO_weekly_dist.RDS")
###Epicurves
#mean_temp
#daily_temp$date <- rownames(daily_temp) %>% as.Date()
#WHO_weekly$Date <- WHO_weekly$Date %>% as.Date()
#dates <- as.POSIXct(eDEWS$Date)

#colnames_vector <- colnames(WHO_weekly)
#colnames_vector[25] <- "Date"
#colnames_vector[26] <- "All"
#colnames(eDEWS) <- colnames_vector
#eDEWS$All <- eDEWS[,26]

#dates2 <- WHO_weekly$Date %>% as.Date()
#ymd_date <- WHO_weekly$Date %>% ymd()

#Weekly eDEWS, temp, precip
min_date <- min(WHO_weekly_yem$Date, na.rm = TRUE) 
max_date <- max(WHO_weekly_yem$Date, na.rm = TRUE)

#eDEWS bar chart
cases_plt <- ggplot(data = WHO_weekly_yem, aes(x = Date, y = S.Cases)) +
  #geom_bar(stat = "identity", size = 3, col = "darkgrey", fill = "darkgrey") +
  geom_segment(aes(xend = Date, y = 0, yend = S.Cases)) +
  labs(x = "",
       y = "Total Weekly Cases") +
  scale_x_date(date_labels = "%b %y", limits = c(min_date, max_date), 
               date_breaks = "1 month", minor_breaks = NULL) +
  ggtitle("Suspected cholera cases") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))
cases_plt



temp_plt <- ggplot(data = filter(daily_temp, weeks_since >= 31 & weeks_since <= 173)) +
  geom_line(aes(x = date, y = Yemen)) +
  ylab("24h Average Temperature (C)") +
  xlab("") +
  ggtitle("Temperature") +
  scale_x_date(date_labels = "%b %y", limits = c(min_date, max_date), 
               date_breaks = "1 month", minor_breaks = NULL) +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))


precip_plt <- ggplot(data = filter(sum_precip, weeks_since >= 31 & weeks_since <= 173)) +
  geom_line(aes(x = stdate, y = Total)) +
  ylab("Total Weekly Precipitation in Yemen (mm)") +
  xlab("Date") +
  scale_x_date(date_labels = "%b %y", limits = c(min_date, max_date), 
               date_breaks = "1 month", minor_breaks = NULL) +
  ggtitle("Precipation") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 14, face = "bold"))

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/epicurve_wtempprecip.png", width = 1800, height = 900)  
plot_grid(cases_plt, temp_plt, precip_plt, align = "v", ncol = 1)
dev.off()

png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/eDEWSepicurve_wtempprecip.png", width = 1800, height = 900)  
plot_grid(eDEWS_plt, temp_plt, precip_plt, align = "v", ncol = 1)
dev.off()

dfs = vector(mode = "list", length = 21)
for(i in 1:21){
  tryCatch({
    dfs[[i]] <- data.frame(temp = ts(mean_temp[31:173, gov_names[i]]), 
                           temp_lag2 = ts(mean_temp[33:175, gov_names[i]]),
                           precip = ts(sum_precip[31:173, gov_names[i]]),
                           precip_lag2= ts(sum_precip[33:175, gov_names[i]]),
                           cases = ts(eDEWS[,gov_names[i]]),
                           cases_perCap = ts(eDEWS_perCap[,gov_names[i]]),
                           expl = ts(sum_expl[31:173, gov_names[i]]),
                           expl_lag2 = ts(sum_expl[33:175, gov_names[i]]),
                           week = sum_precip$weeks_since[31:173])
  }, error=function(e){cat("ERROR in iteration",gov_names[i], ":", conditionMessage(e), "\n")})
}
names(dfs) <- gov_names[1:21]


############ABSOLUTE CHOLERA CASES###########################################
#Get Regression Coefficietns
coeffs <-matrix(nrow = 21, ncol = 6)

for(j in 1:21){
  coeffs[[j,1]] <- tslm(temp ~ cases, data = dfs[[j]])$coefficients[2]
  coeffs[[j,2]] <- tslm(temp_lag2 ~ cases, data = dfs[[j]])$coefficients[2]
  coeffs[[j,3]] <- tslm(precip ~ cases, data = dfs[[j]])$coefficients[2]
  coeffs[[j,4]] <- tslm(precip_lag2 ~ cases, data = dfs[[j]])$coefficients[2]
  coeffs[[j,5]] <- tslm(expl ~ cases, data = dfs[[j]])$coefficients[2]
  coeffs[[j,6]] <- tslm(expl_lag2 ~ cases, data = dfs[[j]])$coefficients[2]
}

coeffs %<>% as.data.frame()
rownames(coeffs) = gov_names[1:21]
colnames(coeffs) <- c("temp", "temp_lag2", "precip", "precip_lag2", "expl", "expl_lag2")
coeffs$gov <- gov_names[1:21]
saveRDS(coeffs, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/reg_coeffs.RDS")

##Initialize plot lists
temp_plts <- list()
temp_lag2_plts <- list()
precip_plts <- list()
precip_lag2_plts <- list()
expl_plts <- list()
expl_lag2_plts <- list()

for(i in 1:21){
  temp_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = temp, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Weekly Cases") +
    xlab("Weekly Temperature Avg.") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  temp_lag2_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = temp_lag2, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Weekly Cases") +
    xlab("Weekly Temperature Avg. (lag = 2 weeks)") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  precip_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = precip, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Cases") +
    xlab("Precip") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  precip_lag2_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = precip_lag2, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Cases") +
    xlab("Precipitation (Lag = 2 weeks)") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  expl_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = expl, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Cases") +
    xlab("Explosions") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  expl_lag2_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = expl_lag2, y = cases)) +
    ggtitle(gov_names[i]) +
    ylab("Cases") +
    xlab("Explosions Lagged 2 Weeks") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
}

rpub_args <- list(ncol = 7, nrow = 3)
#Temperature Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_vs_temp.png", width = 1800, height = 900)
do.call(ggarrange, append(temp_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Temperature Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Temperature Lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_vs_tempLag2.png", width = 1800, height = 900)
do.call(ggarrange, append(temp_lag2_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Temperature Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Precipitation Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_vs_temp.png", width = 1800, height = 900)
do.call(ggarrange, append(precip_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Precipitation Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Precipitation lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_vs_precipLag2.png", width = 1800, height = 900)
do.call(ggarrange, append(precip_lag2_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Precipitation (lag = 2 weeks) Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Conflict Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/expl_vs_temp.png", width = 1800, height = 900)
do.call(ggarrange, append(expl_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Conflict Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Conflict Lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/expllag2_vs_temp.png", width = 1800, height = 900)
do.call(ggarrange, append(expl_lag2_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Conflict Lag 2 Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

###### Create a Time-Series plot for cholera cases in each governate
ts_plts <- vector(mode = "list", length = 21)

for(i in 1:21){
  mini_df <- data.frame(week = eDEWS$weeks_since, cases = eDEWS[,gov_names[i]])
  ts_plts[[i]] <- ggplot(mini_df, aes(week, cases)) + geom_bar(stat = "identity", na.rm = TRUE) +
    ggtitle(paste(gov_names[i], "cases")) +
    annotate(geom = "rect", xmin = 79, xmax = 104, ymin = 0, ymax = 500, fill = "bisque", alpha = 0.7) +
    
    xlab("Week Number") + ylab("Number of Cases") +
    theme_bw()
}

#Multiplot of the time series for each governate
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/ts_cases.png", width = 1800, height = 900)
do.call(ggarrange, append(ts_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Cholera outbreaks in each region of Yemen", face = "bold", size = 20))
dev.off()


############ CHOLERA CASES PER 100000 POPULATION###########################################
#Get Regression Coefficietns
coeffs <-matrix(nrow = 21, ncol = 6)

for(j in 1:21){
  coeffs[[j,1]] <- tslm(temp ~ cases_perCap, data = dfs[[j]])$coefficients[2]
  coeffs[[j,2]] <- tslm(temp_lag2 ~ cases_perCap, data = dfs[[j]])$coefficients[2]
  coeffs[[j,3]] <- tslm(precip ~ cases_perCap, data = dfs[[j]])$coefficients[2]
  coeffs[[j,4]] <- tslm(precip_lag2 ~ cases_perCap, data = dfs[[j]])$coefficients[2]
  coeffs[[j,5]] <- tslm(expl ~ cases_perCap, data = dfs[[j]])$coefficients[2]
  coeffs[[j,6]] <- tslm(expl_lag2 ~ cases_perCap, data = dfs[[j]])$coefficients[2]
}

coeffs %<>% as.data.frame()
rownames(coeffs) = gov_names[1:21]
colnames(coeffs) <- c("temp", "temp_lag2", "precip", "precip_lag2", "expl", "expl_lag2")
coeffs$gov <- gov_names[1:21]
saveRDS(coeffs, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/reg_coeffs.RDS")

##Initialize plot lists
temp_perCap_plts <- list()
temp_lag2_perCap_plts <- list()
precip_perCap_plts <- list()
precip_lag2_perCap_plts <- list()
expl_perCap_plts <- list()
expl_lag2_perCap_plts <- list()

for(i in 1:21){
  temp_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = temp, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("Weekly cases_perCap") +
    xlab("Weekly Temperature Avg.") +
    geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  temp_lag2_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = temp_lag2, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("Weekly cases_perCap") +
    xlab("Weekly Temperature Avg. (lag = 2 weeks)") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  precip_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = precip, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("cases_perCap") +
    xlab("Precip") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  precip_lag2_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = precip_lag2, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("cases_perCap") +
    xlab("Precipitation (Lag = 2 weeks)") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  expl_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = expl, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("cases_perCap") +
    xlab("Explosions") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
  expl_lag2_perCap_plts[[i]] <- dfs[[i]] %>% 
    as.data.frame() %>%
    ggplot(aes(x = expl_lag2, y = cases_perCap)) +
    ggtitle(gov_names[i]) +
    ylab("cases_perCap") +
    xlab("Explosions Lagged 2 Weeks") +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
}

rpub_args <- list(ncol = 7, nrow = 3)
#Temperature Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_temp.png", width = 1800, height = 900)
do.call(ggarrange, append(temp_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Temperature Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Temperature Lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_tempLag2.png", width = 1800, height = 900)
do.call(ggarrange, append(temp_lag2_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Temperature Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Precipitation Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_precip.png", width = 1800, height = 900)
do.call(ggarrange, append(precip_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Precipitation Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Precipitation lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_precipLag2.png", width = 1800, height = 900)
do.call(ggarrange, append(precip_lag2_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Precipitation (lag = 2 weeks) Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Conflict Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_expl.png", width = 1800, height = 900)
do.call(ggarrange, append(expl_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Conflict Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

#Conflict Lag 2 Multiplot
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/cases_perCap_vs_expllag2.png", width = 1800, height = 900)
do.call(ggarrange, append(expl_lag2_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Conflict Lag 2 Vs. Cholera Linear Regression", face = "bold", size = 20))
dev.off()

###### Create a Time-Series plot for cholera cases per 100,000 populationeach governate
ts_perCap_plts <- vector(mode = "list", length = 21)

for(i in 1:21){
  mini_df <- data.frame(week = eDEWS_perCap$weeks_since, cases = eDEWS_perCap[,gov_names[i]])
  ts_perCap_plts[[i]] <- ggplot(mini_df, aes(week, cases)) + geom_bar(stat = "identity", na.rm = TRUE) +
    ggtitle(paste(gov_names[i], "cases")) +
    annotate(geom = "rect", xmin = 79, xmax = 104, ymin = 0, ymax = 300, fill = "bisque", alpha = 0.7) +
    xlab("Week Number") + ylab("Number of Cases") +
    geom_hline(yintercept = 10, linetype = "dashed", color = "red")
  theme_bw()
}

#Multiplot of the time series for each governate
png(file = "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/plots/ts_perCap_cases.png", width = 1800, height = 900)
do.call(ggarrange, append(ts_perCap_plts, rpub_args)) %>%
  annotate_figure(top = text_grob("Cholera cases per 100,000 Population in each region of Yemen", face = "bold", size = 20))
dev.off()


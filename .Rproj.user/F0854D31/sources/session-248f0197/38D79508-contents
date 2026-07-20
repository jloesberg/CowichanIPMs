## What this script does:

# Reads in cleaned weather station data, makes a single df with seasonal summaries for use for the IPM perennial project
## Here, we need a df that gives climate from all years (for now)

#################################################################################

library(tidyverse)
library(lubridate)
library(wesanderson)
library(patchwork)

#library(ggpubr)
theme_set(theme_classic())
Z <- wes_palettes$Zissou1

#################################################################################
## read in cleaned, extrapolated weather station data from WeatherStnInterp...
weather <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/weather station/WS_cleaned/cgop_weather_daily_interp.csv")
weather$Date <- parse_date_time(weather$Date, "Y-m-d") #may need to change the format to whatever R reads it in as
weather$month_year <- format(as.Date(weather$Date), "%Y-%m")

#because the 2012 data has NA's, only include the December values (no na's)

weather <- weather %>% filter(Date >= as.Date("2012-12-01"),
                              Date <= as.Date("2025-09-30")) #update this to when we have all of the cliamte data we want!


#################################################################################

##Spring
# growing season values
### I'm classifying "growing season" as March - May, so the same as meteorological spring for the climate NA data
spring <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 2,
         month < 6)%>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(spring.tot.precip = sum(total_precip_mm),
            spring.mean.min.temp = round(mean(minTemp_C), 3),
            spring.mean.max.temp = round(mean(maxTemp_C),3),
            spring.mean.temp = round(mean(AveTemp_C),3))

# #################################################################################

##WINTER
##  Dec, Jan, Feb. This is hard bc it spans 2 years
winter <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month < 3 | month == 12) %>% 
  mutate(year = if_else(month == 12, year+1, year)) %>% # need to make December the same time frame as Jan and Feb
  group_by(year) %>%
  dplyr::summarize(winter.tot.precip = sum(total_precip_mm),
            winter.mean.min.temp = round(mean(minTemp_C), 3),
            winter.mean.max.temp = round(mean(maxTemp_C), 3),
            winter.mean.temp = round(mean(AveTemp_C), 3)) 


#################################################################################

##SUMMER
#summer values and lagged 1 year and 2 year values
## summer is June-August
summer <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 5,
         month < 9) %>% 
  group_by(year) %>%
  dplyr::summarize(summer.tot.precip = sum(total_precip_mm),
            summer.mean.min.temp = round(mean(minTemp_C), 3),
            summer.mean.max.temp = round(mean(maxTemp_C), 3),
            summer.mean.temp = round(mean(AveTemp_C), 3))

#################################################################################

##FALL
#fall values and lagged 1 year and 2 year values
## fall is September-November
fall <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 9,
         month < 12) %>% 
  group_by(year) %>%
  dplyr::summarize(fall.tot.precip = sum(total_precip_mm),
                   fall.mean.min.temp = round(mean(minTemp_C), 3),
                   fall.mean.max.temp = round(mean(maxTemp_C), 3),
                   fall.mean.temp = round(mean(AveTemp_C), 3))

###################################################################################

## put these together:
climate <- left_join(left_join(left_join(spring, winter), summer), fall) %>% 
  mutate(year = as.character(year)) 

#climate_percentiles <- rbind(GS_percentile, s_percentile, w_percentile, F_percentile)
#climate_percentiles$season <- c("growseason", "summer", "winter", "fall")
remove(spring, summer, winter, fall, weather) # take away the dfs I dont need

###################################################################################
#here I'm scaling the climate variables
climate_scale <- climate %>% 
  dplyr::select(1, c(ends_with("tot.precip"), ends_with(".temp")))

j <- sapply(climate_scale, is.numeric)
climate_scale[j] <- scale(climate_scale[j])
climate_scale <- climate_scale %>% 
    mutate_if(is.numeric, round, 3)
remove(j)



###################################################################################
# #histogram for distribution of climate variables
# histograms <- climate %>%
#   dplyr::select(year, summer.mean.max.temp, winter.mean.min.temp, summer.tot.precip, spring.tot.precip, fall.mean.temp, spring.mean.temp)
# 
# 
# par(mfrow=c(4,2))
# 
# hist(histograms$summer.mean.max.temp, xlab = "degrees C", main = "Summer Maximum", col = 'white', border = "black")
# abline(v = mean(histograms$summer.mean.max.temp, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$summer.mean.max.temp, na.rm = T) + sd(histograms$summer.mean.max.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$summer.mean.max.temp, na.rm = T) - sd(histograms$summer.mean.max.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# #points(x = clip.summer, y = 0, pch = 19, col = "darkorchid4", cex = 1.75)
#  # abline(v = mean(histograms$summer.mean.temp, na.rm = T) - 2*(sd(histograms$summer.mean.temp, na.rm = T)),
#  #        col = "grey", lty = 2, lwd = 2)
#  # abline(v = mean(histograms$summer.mean.temp, na.rm = T) + 2*(sd(histograms$summer.mean.temp, na.rm = T)),
#  #        col = "grey", lty = 2, lwd = 2)
# 
# 
# 
# hist(histograms$winter.mean.min.temp, xlab = "degrees C", main = "Winter Minimum", col = 'white', border = "black")
# abline(v = mean(histograms$winter.mean.min.temp, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$winter.mean.min.temp, na.rm = T) + sd(histograms$winter.mean.min.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$winter.mean.min.temp, na.rm = T) - sd(histograms$winter.mean.min.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# #points(x = clip.winter, y = 0, pch = 19, col = "darkorchid4", cex = 1.75)
# 
# 
# 
# hist(histograms$spring.tot.precip, xlab = "Total Precip (mm)", main = "Spring Total Precip", col = 'white', border = "black")
# abline(v = mean(histograms$spring.tot.precip, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$spring.tot.precip, na.rm = T) + sd(histograms$spring.tot.precip, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$spring.tot.precip, na.rm = T) - sd(histograms$spring.tot.precip, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# #points(x = clip.spring,y = 0, pch = 19, col = "darkorchid4", cex = 1.75)
# 
# 
# hist(histograms$summer.tot.precip, xlab = "Total Precip (mm)", main = "Summer Total Precip", col = 'white', border = "black")
# abline(v = mean(histograms$summer.tot.precip, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$summer.tot.precip, na.rm = T) + sd(histograms$summer.tot.precip, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$summer.tot.precip, na.rm = T) - sd(histograms$summer.tot.precip, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# #points(x = clip.precip,y = 0, pch = 19, col = "darkorchid4", cex = 1.75)
# 
# hist(histograms$fall.mean.temp, xlab = "degrees C", main = "Fall Mean Temp", col = 'white', border = "black")
# abline(v = mean(histograms$fall.mean.temp, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$fall.mean.temp, na.rm = T) + sd(histograms$fall.mean.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$fall.mean.temp, na.rm = T) - sd(histograms$fall.mean.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# #points(x = clip.precip,y = 0, pch = 19, col = "darkorchid4", cex = 1.75)
# 
# hist(histograms$spring.mean.temp, xlab = "degrees C", main = "Spring Mean Temp", col = 'white', border = "black")
# abline(v = mean(histograms$spring.mean.temp, na.rm = T),
#        col = "steelblue", lty = 2, lwd = 4)
# abline(v = mean(histograms$spring.mean.temp, na.rm = T) + sd(histograms$spring.mean.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# abline(v = mean(histograms$spring.mean.temp, na.rm = T) - sd(histograms$spring.mean.temp, na.rm = T),
#        col = "grey", lty = 2, lwd = 3)
# 
# #remove(histograms)

#write.csv(climate_scale, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/Primula/data_for_publication/CGOP_climatevalues_scaled.csv", row.names=FALSE)
#write.csv(climate, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/Primula/data_for_publication/CGOP_climatevalues.csv", row.names=FALSE)


#write.csv(climate_scale, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/chapter2/data/CGOP_climatevalues_scaled.csv", row.names=FALSE)
#write.csv(climate, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/chapter2/data/CGOP_climatevalues.csv", row.names=FALSE)

write.csv(climate, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/CowichanIPMs/clean_data/CGOP_climatevalues.csv", row.names=FALSE)
write.csv(climate_scale, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/CowichanIPMs/clean_data/CGOP_climatevalues_scaled.csv", row.names=FALSE)

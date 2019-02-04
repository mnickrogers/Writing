# Clear the working space
rm(list = ls())

#install.packages("lubridate")
#install.packages("dplyr")

# Load packages
library(lubridate)
library(readxl)
library(AER)
library(car)
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(knitr)
library(lmtest)
library(openintro)
library(OIdata)
library(readstata13)
library(reshape)
library(sandwich)
library(stargazer)
library(ggmap)
library(ggplot2)
library(maps)
library(mapdata)

options(scipen = 9)

cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

setwd("/Users/nicholasrogers/Desktop/RainFireData")

# ------ Precipitation data ------

pcpnData = read.csv("precipitation_data_sum.csv")
pcpnData$Date = as.Date(pcpnData$Date, "%m/%d/%y")

rainMonthData = pcpnData %>% group_by(month = floor_date(Date, "month")) %>% summarize(pcpn = sum(Pcpn))
rainYearData = pcpnData %>% group_by(year = floor_date(Date, "year")) %>% summarize(pcpn = sum(Pcpn))

# Format the date so that it only includes the year
rainYearData$year = format(rainYearData$year, "%Y")
rainMonthData$year = format(rainMonthData$month, "%Y")

# Divide the data set into yearly totals for winter + spring and just summer
winterSpringRain = subset(rainMonthData, (format.Date(month, "%m") >= "01" & format.Date(month, "%m") <= "06"))
winterSpringRainYears = winterSpringRain %>% group_by(year = floor_date(month, "year")) %>% summarize(pcpn = sum(pcpn))
winterSpringRainYears$year = format(winterSpringRainYears$year, "%Y")

summerRain = subset(rainMonthData, (format.Date(month, "%m") >= "07" & format.Date(month, "%m") <= "09"))
summerRainYears = summerRain %>% group_by(year = floor_date(month, "year")) %>% summarize(pcpn = sum(pcpn))
summerRainYears$year = format(summerRainYears$year, "%Y")

# Clean up interim data frames
rm(winterSpringRain)
rm(summerRain)

# I think this is obsolete now...
earlyMonths = subset(rainMonthData, 
                                    format.Date(month, "%m") == "01" | 
                                    format.Date(month, "%m") == "02" | 
                                    format.Date(month, "%m") == "03" | # Also, this is dumb... there's a better way to do this, duh
                                    format.Date(month, "%m") == "04" |
                                    format.Date(month, "%m") == "05" |
                                    format.Date(month, "%m") == "05"
                     )

# ------ Fire data ------

fireDataYear = read.csv("fire_data_year_87-16.csv")

# Trim previous data off of data frame so that it matches up with the precipitation data by year.
fireDataYear = fireDataYear[with(fireDataYear, !(year < 2001)), ]

# Do the same to the precipitation data
rainYearData = rainYearData[with(rainYearData, !(year == 2017)), ]

# And for the winter / spring yearly data...
winterSpringRainYears = winterSpringRainYears[with(winterSpringRainYears, !(year == 2017)), ]

# ----- Merge data ------

winterSpringSummerRainYears = merge(winterSpringRainYears,
                                    summerRainYears,
                                    by.x="year",
                                    by.y="year",
                                    all.x = TRUE)
colnames(winterSpringSummerRainYears)[colnames(winterSpringSummerRainYears) == "pcpn.x"] = "winterSpringPrecip"
colnames(winterSpringSummerRainYears)[colnames(winterSpringSummerRainYears) == "pcpn.y"] = "summerPrecip"

rainFireYears = merge(rainYearData, 
                      fireDataYear, 
                      by.x="year", 
                      by.y="year", 
                      all.x = TRUE)

winterSpringRainFireYears = merge(winterSpringRainYears,
                                  fireDataYear,
                                  by.x="year",
                                  by.y="year",
                                  all.x = TRUE)

# ------ Analysis -------

stargazer(
  winterSpringRainFireYears,
  title = "",
  type = "text",
  digits = 3,
  median = T
)

ggplot(data=earlyMonths, aes(y=pcpn, x=month)) +
  geom_point(shape=0) +
  labs(title="") + 
  labs(x="")

ggplot(data=rainFireYears, aes(y=pcpn, x=total_fire_acres)) +
  geom_point(shape=0) +
  geom_smooth(method = "lm") +
  labs(title="") + 
  labs(x="Total acres burned", y="Precipitation")

ggplot(data=winterSpringRainFireYears, aes(y=pcpn, x=total_fire_acres)) +
  geom_point(shape=0) +
  geom_smooth(method = "lm") +
  labs(title="") + 
  labs(x="Total acres burned", y="Precipitation in Winter & Spring")

ggplot(data=winterSpringSummerRainYears, aes(y=summerPrecip, x=winterSpringPrecip)) +
  geom_point(shape=0) +
  labs(title="") + 
  labs(x="Winter and Spring Precipitation", y="Summer Precipitation")


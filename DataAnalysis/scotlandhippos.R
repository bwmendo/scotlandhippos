library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tseries)
library(lmtest)
library(tidyr)
library(nnet)

spd <- read.csv("spd.csv")

spd$Offense.Start.DateTime <- as.POSIXct(spd$Offense.Start.DateTime,
                              format="%m/%d/%Y %I:%M:%S %p")

spd$Date.Of.Crime <- as.Date(spd$Offense.Start.DateTime)

recentSpd <- spd %>% 
  filter(year(spd$Offense.Start.DateTime) >= 2008 & year(spd$Offense.Start.DateTime) <= 2020)

crime_counts <- recentSpd %>%
  group_by(Date.Of.Crime) %>%
  summarize(Total.Offenses = n()) %>%
  arrange(Date.Of.Crime)

yearly_crime <- recentSpd %>%
  group_by(year(Date.Of.Crime)) %>%
  summarize(Total.Offenses = n())

crime_ts_data <- ts(crime_counts$Total.Offenses, start = c(year(crime_counts$Date.Of.Crime)[1],
              month(crime_counts$Date.Of.Crime)[1]), frequency = 365)

plot(crime_ts_data, xlab = "Time (Days)", ylab = "Total Crimes",
     main = "Total Number Of Daily Reported Crime (2008 - 2023)")
abline(h = mean(crime_ts_data), col="blue", lwd = 3)
legend("topleft", legend="Long-Run Mean", col="blue", lty=1:2, cex=0.8)

postCovidData <- spd %>%
  filter(spd$Date.Of.Crime >= as.POSIXct("2020-03-11"))

covid_crime <- postCovidData %>%
  group_by(Date.Of.Crime) %>%
  summarize(Total.Offenses = n()) %>%
  arrange(Date.Of.Crime)

covid_crime_ts <- ts(covid_crime$Total.Offenses, start = c(year(covid_crime$Date.Of.Crime)[1],
                   month(covid_crime$Date.Of.Crime)[1]), frequency = 365)

plot(covid_crime_ts, xlab = "Time (Days)", ylab = "Total Crimes",
     main = "Total Number Of Daily Reported Crime (2020 - 2023)")
abline(h = mean(covid_crime_ts), col="red", lwd = 3)
legend("topleft", legend="Long-Run Mean", col="red", lty=1:2, cex=0.8)

covidOffense <- postCovidData %>%
  group_by(Offense) %>%
  summarise(TotalOffenses = n())

covidOffenseGroup <- postCovidData %>%
  group_by(Offense.Parent.Group, Precinct) %>%
  summarise(TotalOffenses = n())

may2020 <- spd %>%
  filter(month(spd$Date.Of.Crime) == 5)

mayCovidOffense <- may2020 %>%
  group_by(Offense) %>%
  summarise(TotalOffenses = n())

mayCovidOffenseGroup <- may2020 %>%
  group_by(Offense.Parent.Group) %>%
  summarise(TotalOffenses = n())

timeOfReport <- as.numeric(format(postCovidData$Report.DateTime, "%H%M%S"))
postCovidData$TimeGroup <- cut(timeOfReport, breaks = c(0, 30000, 60000, 90000, 120000, 150000, 180000, 210000, 240000), 
                     labels = c("12 AM to 3 AM", "3 AM to 6 AM", "6 AM to 9 AM", "9 AM to 12 PM", "12 PM to 3 PM", "3 PM to 6 PM", "6 PM to 9 PM", "9 PM to 12 AM"), 
                     include.lowest = TRUE)

violentData <- postCovidData %>%
  filter(Precinct != "UNKNOWN" & Precinct != "" & Precinct != "<Null>" & Precinct != "OOJ") %>%
  mutate(Violent.Crime = Offense %in% c("Robbery", "Rape", "Murder & Nonnegligent Manslaughter",
                                        "Aggravated Assault")) %>%
  group_by(Precinct, Violent.Crime) %>%
  summarise(TotalOffenses = n()) %>%
  pivot_wider(names_from = Precinct, values_from = TotalOffenses)

postCovidData$Report.DateTime <- as.POSIXct(postCovidData$Report.DateTime, format="%m/%d/%Y %I:%M:%S %p")

compare <- postCovidData %>%
  filter(Precinct == "W" | Precinct == "N") %>%
  group_by(Precinct, TimeGroup) %>%
  summarise(TotalOffenses = n()) %>%
  arrange(desc(TotalOffenses))
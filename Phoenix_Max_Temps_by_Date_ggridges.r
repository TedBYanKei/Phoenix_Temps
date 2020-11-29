# Created by Ted Braun
# For academic purposes only
#Data from NOAA (National Oceanic and Atmospheric Adminstration)
#https://www.ncdc.noaa.gov/
#Home > Climate Data Online
#Stations GHCND:USW00023183
#Begin Date	1945-01-01 00:00
#End Date	2020-11-23 23:59
#Data Types	TMAX
library(tidyverse)
library(lubridate)
library(ggridges)
library(here)
CurrDir <- here::here()
setwd(CurrDir)

DataDirFile <- paste(CurrDir, "/Phoenix_SkyHarbor_Daily_MaxTemp.csv", sep = "", collapse = "")
Temp_Hist_Data <- read_csv(DataDirFile)
str(Temp_Hist_Data)
Temp_Hist_Data_SS <- Temp_Hist_Data %>% 
  select(DATE, TMAX) %>% 
  rename(HistDt = DATE,
         MaxTemp = TMAX) %>% 
  mutate(HistYear = year(HistDt),
         HistMonths = months(HistDt)) %>%
  arrange(HistDt) %>% 
  filter(HistYear == max(HistYear)) 

Temp_Hist_Data_SS_All <- Temp_Hist_Data %>% 
  select(DATE, TMAX) %>% 
  rename(HistDt = DATE,
         MaxTemp = TMAX) %>% 
  mutate(HistYear = year(HistDt),
         HistMonths = months(HistDt)) %>%
  arrange(HistDt) 

ggplot(Temp_Hist_Data_SS, aes(x = MaxTemp, y = HistMonths, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Phoenix in 2020') +
  ylab("Month") +
  xlab("Daily High Temperature") +
  scale_y_discrete(limits = rev(month.name))



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Lets visualize the above over time, I bet that would be cool  -  Start
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Temp_Hist_Data_SS <- Temp_Hist_Data %>% 
  select(DATE, TMAX) %>% 
  rename(HistDt = DATE,
         MaxTemp = TMAX) %>% 
  mutate(HistYear = year(HistDt),
         HistMonths = months(HistDt)) %>%
  arrange(HistDt)

CurrDirImages <- paste(CurrDir, "/images5", sep = "", collapse = "")
vecYears <- sort(unique(as.vector(Temp_Hist_Data_SS$HistYear)))

vecYearsF <- vecYears %>% .[.>=1948]

for (aYear in vecYears) {
  Hist_aYear <- Temp_Hist_Data_SS %>% 
    filter(HistYear == aYear)
  str(Hist_aYear)
  OutputDirFile <- paste(CurrDirImages, "/Monthly_Temps_", aYear, ".png", sep = "", collapse = "")
  strTitle <- paste("Temperatures in Phoenix: ", aYear, sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 800)
  p <- ggplot(Hist_aYear, aes(x = MaxTemp, y = HistMonths, fill = stat(x), height = stat(density))) +
    geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, stat = "density") +
    scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
    labs(title = strTitle) +
    ylab("Month") +
    xlab("Daily High Temperature") +
    scale_y_discrete(limits = rev(month.name)) + 
    xlim(25, 125)
  print(p)
  dev.off()
}

command <- paste("cd ", CurrDirImages, " && convert -delay 35 *.png Monthly_Temps.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirImages, " && convert -delay 35 *.png Monthly_Temps.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Lets visualize the above over time, I bet that would be cool  -  End
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#The above is a headache to look at.... Ugh.  
#  Need to figure out a better way to visualize this information.
#  Start
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Temp_Hist_Data_SS <- Temp_Hist_Data %>% 
  select(DATE, TMAX) %>% 
  rename(HistDt = DATE,
         MaxTemp = TMAX) %>% 
  mutate(HistYear = year(HistDt),
         HistMonths = months(HistDt)) %>%
  arrange(HistDt)

CurrDirImages <- paste(CurrDir, "/images6", sep = "", collapse = "")
vecYears <- sort(unique(as.vector(Temp_Hist_Data_SS$HistYear)))

vecYearsF <- vecYears %>% .[.>=1948]

for (aYear in vecYearsF) {
  Hist_aYear <- Temp_Hist_Data_SS %>% 
    filter(HistYear == aYear) %>% 
    group_by(HistMonths) %>% 
    summarise(AvgHigh = round((mean(MaxTemp)), digits = 0))
  OutputDirFile <- paste(CurrDirImages, "/Monthly_Avg_High_Temp_", aYear, ".png", sep = "", collapse = "")
  strTitle <- paste("Average High Temp by Month in Phoenix: ", aYear, sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 800)
  p <- ggplot(Hist_aYear, aes(x=AvgHigh, y=HistMonths, fill=HistMonths)) +
    geom_dotplot(binaxis='y', stackdir='center')  + 
    scale_y_discrete(limits = rev(month.name)) + 
    ggtitle(strTitle) +  
    labs(title = strTitle) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 24, face = "bold"), 
          axis.text.x = element_text(face="bold", size=16), 
          axis.text.y = element_text(face="bold", size=16), 
          axis.title.x = element_text(face="bold", size=16), 
          axis.title.y = element_text(face="bold", size=16)) +
    ylab("Month") +
    xlab("Daily High Temperature")  + 
    scale_x_continuous(limits=c(40, 120), breaks = scales::pretty_breaks(n = 8))
  print(p)
  dev.off()
}

command <- paste("cd ", CurrDirImages, " && convert -delay 35 *.png Monthly_High_Avg_Temps.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirImages, " && convert -delay 35 *.png Monthly_High_Avg_Temps.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#The above is a headache to look at.... Ugh.  
#  Need to figure out a better way to visualize this information.
#  End
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Lets do Year-Month facet wrap graphs  -  Start
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

OutputGraphFunction <- function(recDataFrame, recMonths, recFN) {
  recDataFrame_Filt <- recDataFrame %>% filter(HistMonths %in% filter_months)
  recDataFrame_Filt$HistMonths = factor(recDataFrame_Filt$HistMonths, levels=month.name)
  OutputDirFile <- paste(CurrDirImages, "/", recFN, ".png", sep = "", collapse = "")
  strTitle <- paste("Phoenix Average High Temperature through the Years by Month", sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 800)
  
  p <- ggplot(data = recDataFrame_Filt, aes(x=HistYear, y=AvgHigh)) +
    geom_line(color = "steelblue", size = 1.5) +
    geom_point(color ="steelblue", size = 2) + 
    facet_wrap(~ HistMonths, ncol = 1) + 
    labs(title = strTitle, y = "Average High Temperature", x = "") +
    theme(plot.title = element_text(size = 20, face = "bold"), 
          axis.text.x = element_text(face="bold", size=16, angle=50, hjust=1), 
          axis.text.y = element_text(face="bold", size=16), 
          axis.title.x = element_text(face="bold", size=14), 
          axis.title.y = element_text(face="bold", size=16), 
          strip.text = element_text(face="bold", size=18)) + 
    scale_x_continuous(limits=c(1950, 2020), breaks = scales::pretty_breaks(n = 10))
  print(p)
  dev.off()
}


Mean_High_YearlyMonthly <- Temp_Hist_Data_SS_All %>% 
  filter(HistYear >= 1948) %>% 
  group_by(HistYear, HistMonths) %>% 
  summarise(AvgHigh = round((mean(MaxTemp)), digits = 0))
CurrDirImages <- paste(CurrDir, "/images7", sep = "", collapse = "")

filter_months <- c("January", "February", "March")
OutputGraphFunction(Mean_High_YearlyMonthly, filter_months, "Jan-Mar")

filter_months <- c("April", "May", "June")
OutputGraphFunction(Mean_High_YearlyMonthly, filter_months, "Apr-Jun")

filter_months <- c("July", "August", "September")
OutputGraphFunction(Mean_High_YearlyMonthly, filter_months, "Jul-Sep")

filter_months <- c("October", "November", "December")
OutputGraphFunction(Mean_High_YearlyMonthly, filter_months, "Oct-Dec")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Lets do Year-Month facet wrap graphs  -  End
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




#Amanda Mitchel
#LIS 572 Final Project
# Autumn 2024

# Loading relevant libraries: 
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)

# Loading data from computer:
FCC_data <- read.csv("county_connections.csv")
ACS_2012 <- read.csv("ACSDT5Y2012.B01003-Data.csv")
ACS_2013 <- read.csv("ACSDT5Y2013.B01003-Data.csv")
ACS_2014 <- read.csv("ACSDT5Y2014.B01003-Data.csv")
ACS_2015 <- read.csv("ACSDT5Y2015.B01003-Data.csv")
ACS_2016 <- read.csv("ACSDT5Y2016.B01003-Data.csv")
ACS_2017 <- read.csv("ACSDT5Y2017.B01003-Data.csv")
ACS_2018 <- read.csv("ACSDT5Y2018.B01003-Data.csv")
ACS_2019 <- read.csv("ACSDT5Y2019.B01003-Data.csv")
ACS_2020 <- read.csv("ACSDT5Y2020.B01003-Data.csv")
ACS_2021 <- read.csv("ACSDT5Y2021.B01003-Data.csv")
ACS_2022 <- read.csv("ACSDT5Y2022.B01003-Data.csv")

#reminder that FCC numbers are thousands

# Cleaning FCC data so there is only one entry per year: 
FCC_clean <- FCC_data%>%filter(month == 12)

# Re-naming population column; splitting county and state values into two separate columns.
ACS_2012_clean <- ACS_2012%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2013_clean <- ACS_2013%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2014_clean <- ACS_2014%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2015_clean <- ACS_2015%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2016_clean <- ACS_2016%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2017_clean <- ACS_2017%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2018_clean <- ACS_2018%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2019_clean <- ACS_2019%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2020_clean <- ACS_2020%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2021_clean <- ACS_2021%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

ACS_2022_clean <- ACS_2022%>%mutate(population = B01003_001E)%>%separate(col = NAME, sep = ",", c("countyname","statename"))

# Removing whitespace in statename values:
ACS_2012_clean <- ACS_2012_clean%>% mutate(statename1 = str_trim(statename))

ACS_2013_clean <- ACS_2013_clean%>% mutate(statename1 = str_trim(statename))

ACS_2014_clean <- ACS_2014_clean%>% mutate(statename1 = str_trim(statename))

ACS_2015_clean <- ACS_2015_clean%>% mutate(statename1 = str_trim(statename))

ACS_2016_clean <- ACS_2016_clean%>% mutate(statename1 = str_trim(statename))

ACS_2017_clean <- ACS_2017_clean%>% mutate(statename1 = str_trim(statename))

ACS_2018_clean <- ACS_2018_clean%>% mutate(statename1 = str_trim(statename))

ACS_2019_clean <- ACS_2019_clean%>% mutate(statename1 = str_trim(statename))

ACS_2020_clean <- ACS_2020_clean%>% mutate(statename1 = str_trim(statename))

ACS_2021_clean <- ACS_2021_clean%>% mutate(statename1 = str_trim(statename))

ACS_2022_clean <- ACS_2022_clean%>% mutate(statename1 = str_trim(statename))


#Creating a FCC data frame for each year to facilitate joining FCC and ACS data; cleaning to only include relevant variables.

FCC_clean_2012<- FCC_clean%>%filter(year == 2012)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2013<- FCC_clean%>%filter(year == 2013)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2014<- FCC_clean%>%filter(year == 2014)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2015<- FCC_clean%>%filter(year == 2015)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2016<- FCC_clean%>%filter(year == 2016)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2017<- FCC_clean%>%filter(year == 2017)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2018<- FCC_clean%>%filter(year == 2018)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2019<- FCC_clean%>%filter(year == 2019)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2020<- FCC_clean%>%filter(year == 2020)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2021<- FCC_clean%>%filter(year == 2021)%>% select(countyname, statename, year, consumer, non_consumer)

FCC_clean_2022<- FCC_clean%>%filter(year == 2022)%>% select(countyname, statename, year, consumer, non_consumer)

# Cleaning ACS dataframe to only include relevant variables.
ACS_2012_clean_pretty <- ACS_2012_clean%>% select(countyname, statename1, population)

ACS_2013_clean_pretty <- ACS_2013_clean%>% select(countyname, statename1, population)

ACS_2014_clean_pretty <- ACS_2014_clean%>% select(countyname, statename1, population)

ACS_2015_clean_pretty <- ACS_2015_clean%>% select(countyname, statename1, population)

ACS_2016_clean_pretty <- ACS_2016_clean%>% select(countyname, statename1, population)

ACS_2017_clean_pretty <- ACS_2017_clean%>% select(countyname, statename1, population)

ACS_2018_clean_pretty <- ACS_2018_clean%>% select(countyname, statename1, population)

ACS_2019_clean_pretty <- ACS_2019_clean%>% select(countyname, statename1, population)

ACS_2020_clean_pretty <- ACS_2020_clean%>% select(countyname, statename1, population)

ACS_2021_clean_pretty <- ACS_2021_clean%>% select(countyname, statename1, population)

ACS_2022_clean_pretty <- ACS_2022_clean%>% select(countyname, statename1, population)


# Combining FCC and ACS dataframes:
FCC_ACS_2012 <- inner_join(FCC_clean_2012, ACS_2012_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2013 <- inner_join(FCC_clean_2013, ACS_2013_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2014 <- inner_join(FCC_clean_2014, ACS_2014_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2015 <- inner_join(FCC_clean_2015, ACS_2015_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2016 <- inner_join(FCC_clean_2016, ACS_2016_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2017 <- inner_join(FCC_clean_2017, ACS_2017_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2018 <- inner_join(FCC_clean_2018, ACS_2018_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2019 <- inner_join(FCC_clean_2019, ACS_2019_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2020 <- inner_join(FCC_clean_2020, ACS_2020_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2021 <- inner_join(FCC_clean_2021, ACS_2021_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

FCC_ACS_2022 <- inner_join(FCC_clean_2022, ACS_2022_clean_pretty, by = c("countyname"="countyname","statename"= "statename1"))

# Combining years:

FCC_ACS_2012to2022 <- bind_rows(FCC_ACS_2012, FCC_ACS_2013, FCC_ACS_2014, FCC_ACS_2015, FCC_ACS_2016, FCC_ACS_2017, FCC_ACS_2018, FCC_ACS_2019, FCC_ACS_2020, FCC_ACS_2021, FCC_ACS_2022)


#Removing numeric placeholders:

plottable_FCC_ACS_2012to2022 <- FCC_ACS_2012to2022%>% filter(consumer != "-9999")

plottable_FCC_ACS_2012to2022 <- plottable_FCC_ACS_2012to2022%>% filter(non_consumer != "-9999")

# Making population values into integers:

plottable_FCC_ACS_2012to2022$population <- as.integer(plottable_FCC_ACS_2012to2022$population) 

#Inspecting the curated dataset:

n_distinct(FCC_ACS_2012to2022$statename)
n_distinct(FCC_ACS_2012to2022$year)
max(plottable_FCC_ACS_2012to2022$consumer)
min(plottable_FCC_ACS_2012to2022$consumer)
max(plottable_FCC_ACS_2012to2022$non_consumer)
min(plottable_FCC_ACS_2012to2022$non_consumer)
max(plottable_FCC_ACS_2012to2022$population)
min(plottable_FCC_ACS_2012to2022$population)


#Creating visualizations:

#reminder that FCC numbers are thousands

# Visualization #1

viz1 <- plottable_FCC_ACS_2012to2022%>%filter(year == 2022)%>%slice_max(n=10, order_by = consumer)
  
ggplot(data = viz1)+geom_col(mapping = aes(x= consumer, y = countyname),fill = "pink")+labs(title = "Top 10 Counties by Consumer Broadband Connections")+labs(x="Consumer Connections in Thousands for 2022")+labs(y = "County Name")

#Visualization #2

viz2 <- plottable_FCC_ACS_2012to2022%>%filter(countyname == "Hennepin County")

ggplot(data = viz2)+geom_line(mapping = aes(x= year, y = consumer), color = "purple")+labs(title = "Hennepin County Broadband Growth from 2012 to 2022")+labs(x="Year")+labs(y = "Consumer Broadband Connections in Thousands")

#Visualization #3

ggplot(data = viz2)+geom_line(mapping = aes(x= year, y = population), color = "turquoise")+labs(title = "Hennepin County Population Growth from 2012 to 2022")+labs(x="Year")+labs(y = "Population")


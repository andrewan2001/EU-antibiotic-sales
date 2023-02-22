#Antibiotic resistance presents a serious problem to global health. In part it is 
# due to the use of antibiotics in livestock, which tend to receive similar antibiotics 
# to humans - known as medically important antibiotics. Essentially, use of antibiotics 
# has been used for growth promotion and feed efficiency to produce larger animals 
# with less feed costs. The European countries have monitored antibiotic sale 
# since 2010 to better understand this issue. The following analysis is based on
# the European Medicines Agency's European Surveillance of Veterinary Antimicrobial 
# Consumption database (ESVAC) and captures sale data from 2010 - 2021. The following 
# visualizes total antibiotic sale for livestock in European countries and determine 
# if there have been significant reductions to antibiotic sale in the past decade. 

library(tidyverse)
library(tidyselect)
library(rmarkdown)
library(rstudioapi)
library(ggformula)
library(ggplot2)
library(ggforce)
library(readr)

EU_antimicrobial_livestock_use_country_ <- read_csv("Documents/Job search/EU_antimicrobial_livestock_use_country_ Updated.csv")

#Initial Observation of data
View(EU_antimicrobial_livestock_use_country_)

#Plotting all countries + total together
ggplot(EU_antimicrobial_livestock_use_country_, aes(x=Year, y=`Antibiotic sales (tonnes)`,color=Country))+ 
  geom_col()+facet_wrap(~Country, scales="free")

#Identification of high-buyer countries (>100 tonnes of antibiotic sale)
Big_user <- filter(EU_antimicrobial_livestock_use_country_, `Antibiotic sales (tonnes)` > 100)
Big_user
Big_user$Country
unique(Big_user$Country)
Big_user_country <- unique(Big_user$Country)
Big_user_country

#Plotting Big buyer, although filtering out entries <100 tonnes)

  #Plot function
CountryPlot <- function(x,y){
  ggplot(x, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill =Country)) +
    geom_col() + facet_wrap(~Country,scales=y)+
    scale_x_continuous(limits=c(2009,2022), breaks=c(2010,2012,2014,2016,2018,2020))+
    labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")
}

#Plotting Big Buyers
CountryPlot(Big_user,"free")

#Plotting all EU countries w/o total
Only_EU_Countries<- filter(EU_antimicrobial_livestock_use_country_, Country != "Total")
Only_EU_Countries

#Plotting all EU countries with free, relative scaling of y-axis
CountryPlot(Only_EU_Countries, "free")

#Plotting with rigid scale of y-axis (same scale for all countries)
CountryPlot(Only_EU_Countries, "fixed")

#Indexing countries with sale of 100 or more tonnes of antibiotics at any time
EU_big_users_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_country, ]

#Plotting big buyers (>100 tonnes)

  #Plotting with fixed scale (>100)
CountryPlot(EU_big_users_cleaned, "fixed")

  #Plotting with free scale (>100)
CountryPlot(EU_big_users_cleaned, "free")

#Determining Top buyers > 200 tonnes
Big_user200 <- filter(Only_EU_Countries, `Antibiotic sales (tonnes)` > 200)
Big_user200
Big_user_200country <- unique(Big_user200$Country)
Big_user_200country

  #Filtering for any country that has ever bought >200 tonnes of antibiotics
EU_big_users200_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_200country, ]

  #Free scale plotting >200 tonnes buyers
CountryPlot(EU_big_users200_cleaned, "free")

  #Fixed scale
CountryPlot(EU_big_users200_cleaned, "fixed")

#Determining Top buyers (>300 tonnes of antibiotics)
Big_user300 <- filter(Only_EU_Countries, `Antibiotic sales (tonnes)` > 300)
Big_user300
Big_user_300country <- unique(Big_user300$Country)
Big_user_300country

  #Indexing for Top 8 Buyers data
EU_big_users300_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_300country, ]

  #Plotting Top 8 buyers, free scale
CountryPlot(EU_big_users300_cleaned, "free")

  #Plotting Top 8 buyers, fixed scale
CountryPlot(EU_big_users300_cleaned, "fixed")

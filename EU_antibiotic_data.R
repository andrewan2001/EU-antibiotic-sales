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
library(ggplot2)
library(ggformula)
library(ggforce)
library(readr)

EU_antimicrobial_livestock_use_country_ <- read_csv("Documents/Job search/EU_antimicrobial_livestock_use_country_ Updated.csv")

#Filtering antibiotic sale by individual country. 
CountryFunc <- function(x){filter(EU_antimicrobial_livestock_use_country_, Country == x)}
Austria <- CountryFunc("Austria")
Belgium <- CountryFunc("Belgium")
Croatia <- CountryFunc("Croatia")
Cyprus <- CountryFunc("Cyprus")
Czech <- CountryFunc("Czech Republic")
Denmark <- CountryFunc("Denmark")
Estonia <- CountryFunc("Estonia")
Finland <- CountryFunc("Finland")
Germany <- CountryFunc("Germany")
France <- CountryFunc("France")
Greece <- CountryFunc("Greece")
Hungary <- CountryFunc("Hungary")
Italy <- CountryFunc("Italy")
Iceland <- CountryFunc("Iceland")
Ireland <- CountryFunc("Ireland")
Latvia <- CountryFunc("Latvia")
Lithuania <- CountryFunc("Lithuania")
Luxembourg <- CountryFunc("Luxembourg")
Malta <- CountryFunc("Malta")
Netherlands <- CountryFunc("Netherlands")
Poland <-CountryFunc("Poland")
Portugal <- CountryFunc("Portugal")
Romania <- CountryFunc("Romania")
Slovakia <- CountryFunc("Slovakia")
Slovenia <- CountryFunc("Slovenia")
Spain <- CountryFunc("Spain")
Sweden <- CountryFunc("Sweden")
Switzerland <- CountryFunc("Switzerland")
UK <- CountryFunc("United Kingdom")

#Initial Observation of data - checking on Europe's largest countries.

View(EU_antimicrobial_livestock_use_country_)

View(France)
View(Germany)
View(Italy)
View(Spain)
View(UK)


#Identification of high-buyer countries (>100 tonnes of antibiotic sale)
Big_user <- filter(EU_antimicrobial_livestock_use_country_, `Antibiotic sales (tonnes)` > 100)
Big_user
Big_user$Country
unique(Big_user$Country)
Big_user_country <- unique(Big_user$Country)
Big_user_country


#Plotting all countries + total together
ggplot(EU_antimicrobial_livestock_use_country_, aes(x=Year, y=`Antibiotic sales (tonnes)`,color=Country))+ 
  geom_col()+facet_wrap(~Country, scales="free")

#Plotting Big buyer, although filtering out entries <100 tonnes)
ggplot(Big_user, aes(x=Year, y =  `Antibiotic sales (tonnes)`, color=Country)) +
  geom_col() + facet_wrap(~Country,scales="free")+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020))+
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Plotting all EU countries w/o total
Only_EU_Countries<- filter(EU_antimicrobial_livestock_use_country_, Country != "Total")
Only_EU_Countries

#Plotting all EU countries with free, relative scaling of y-axis
ggplot(Only_EU_Countries, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) + 
  geom_col() + facet_wrap(~Country,scales="free") + 
  scale_x_continuous(limits=c(2009,2022)) + 
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Plotting with rigid scale of y-axis (same scale for all countries)
ggplot(Only_EU_Countries, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) + 
  geom_col() + facet_wrap(~Country)+ scale_x_continuous(limits=c(2009,2022)) + 
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Identifying countries with sale of 100 or more tonnes of antibiotics at any time
EU_big_users_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_country, ]

#Plotting big buyers (>100 tonnes)

  #Plotting with fixed scale (>100)
ggplot(EU_big_users_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country) + 
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) + 
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

  #Plotting with free scale (>100)
ggplot(EU_big_users_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country, scale = "free") +
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) +
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

  #Plotting with fill color for aesthetics
ggplot(EU_big_users_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country, scale = "free") + 
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) + 
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Determining Top buyers > 200 tonnes
Big_user200 <- filter(Only_EU_Countries, `Antibiotic sales (tonnes)` > 200)
Big_user200
Big_user_200country <- unique(Big_user200$Country)
Big_user_200country

  #Filtering for any country that has ever bought >200 tonnes of antibiotics
EU_big_users200_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_200country, ]

  #Free scale plotting >200 tonnes buyers
ggplot(EU_big_users200_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country, scale = "free") + 
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) +
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

  #Fixed scale
ggplot(EU_big_users200_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country) + 
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) +
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Determining Top buyers (>300 tonnes of antibiotics)
Big_user300 <- filter(Only_EU_Countries, `Antibiotic sales (tonnes)` > 300)
Big_user300
Big_user_300country <- unique(Big_user300$Country)
Big_user_300country

  #Top 8 buyers identified
EU_big_users300_cleaned<-Only_EU_Countries[Only_EU_Countries$Country %in% Big_user_300country, ]

  #Plotting Top 8 buyers, free scale
ggplot(EU_big_users300_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country, scale = "free") +
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) +
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

  #Plotting Top 8 buyers, fixed scale
ggplot(EU_big_users300_cleaned, aes(x=Year, y =  `Antibiotic sales (tonnes)`, fill=Country)) +
  geom_col() + facet_wrap(~Country) +
  scale_x_continuous(limits = c(2009,2022),breaks=c(2010,2012,2014,2016,2018,2020)) +
  labs(title = "Livestock antibiotic sale in EU, 2010 - 2021", y = "Antibiotic sale, tonnes")

#Conversion to single word variable for ease
Belgium$Antibiotic<- Belgium$`Antibiotic sales (tonnes)`
France$Antibiotic <- France$`Antibiotic sales (tonnes)`
Germany$Antibiotic <- Germany$`Antibiotic sales (tonnes)`
Italy$Antibiotic <- Italy$`Antibiotic sales (tonnes)`
Netherlands$Antibiotic <- Netherlands$`Antibiotic sales (tonnes)`
Poland$Antibiotic<-Poland$`Antibiotic sales (tonnes)`
Spain$Antibiotic <- Spain$`Antibiotic sales (tonnes)`
UK$Antibiotic <- UK$`Antibiotic sales (tonnes)`

#Plotting by country - Top 8
ggplot(data=Belgium, aes(x=Year, y = Antibiotic)) + geom_col(fill = "green2") +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in Belgium, 2011 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=France, aes(x=Year, y = Antibiotic)) + geom_col(fill = "blue4") +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2020))  +
  labs(title = "Livestock antibiotic sale in France, 2011 - 2018", y = "Antibiotic sale, tonnes")+ 
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=Germany, aes(x=Year, y = Antibiotic)) + geom_col(fill = "goldenrod") +
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in Germany, 2011 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=Netherlands, aes(x=Year, y = Antibiotic)) + geom_col(fill = "darkorchid3") +
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in the Netherlands, 2011 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=Spain, aes(x=Year, y = Antibiotic)) + geom_col(fill = "deeppink") +
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in Spain, 2011 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=UK, aes(x=Year, y = Antibiotic)) + geom_col(fill = "brown") +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in the UK, 2010 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=Italy, aes(x=Year, y = Antibiotic)) + geom_col(fill = "forestgreen") +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in Italy, 2010 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))

ggplot(data=Poland, aes(x=Year, y = Antibiotic)) + geom_col(fill = "cyan2") +
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2020)) +
  labs(title = "Livestock antibiotic sale in Poland, 2011 - 2018", y = "Antibiotic sale, tonnes") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size=18))



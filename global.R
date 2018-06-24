# -------- #
# global.R #
# -------- #

# DESC: global.R script used to create static objects that app needs to run.
#       Includes loading and manipulating daya, and defining functions to be used in server.R

# Install these packages before running the app:
# install.packages(c("shiny", "shinydashboard", "DT", "RODBC", "tidyverse", "scales"))
# install.packages("shinyBS")


# Packages ------------------------------------------------------------

# shiny app development and appearance
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)

# data import and manipulation
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(scales)

# load external functions
source("functions.R")

# Data Import -------------------------------------------------------------
data_gdp <- read_csv(file = "data/ADO_GDP_201819AreForecasts.csv")
data_basicstats <- read_csv(file = "data/BasicStatistics_2018.csv")
data_exchangerate <- read_csv(file = "data/ADO_ExchangeRates_2018.csv")
data_tradebalance <- read_csv(file = "data/ADO_TradeBalance_201819AreForecasts.csv")
data_externaldebtoutstanding <- read_csv(file = "data/ADO_ExternalDebtOutstanding_2018.csv")


# Data Preparation --------------------------------------------------------
data_spread_gdp <- data_gdp %>% 
  spread(key = Year, value = GrossDomesticProductGrowthPerYearPercentage) %>% 
  rename(GDPRate201819 = `2019`,
         GDPRate201718 = `2018`,
         GDPRate201617 = `2017`,
         GDPRate201516 = `2016`,
         GDPRate201415 = `2015`,
         GDPRate201314 = `2014`,
         GDPRate201213 = `2013`)
data_spread_tradebalance <- data_tradebalance %>% 
  spread(key = Year, value = TradeBalanceInUSDollarMillion) %>% 
  rename(TradeBalance2019 = `2019`,
         TradeBalance2018 = `2018`,
         TradeBalance2017 = `2017`,
         TradeBalance2016 = `2016`,
         TradeBalance2015 = `2015`,
         TradeBalance2014 = `2014`,
         TradeBalance2013 = `2013`)
data_spread_exchangerate <- data_exchangerate %>% 
  spread(key = Year, value = ExchangeRatesInUSDollar) %>% 
  rename(RateUSDollar2017 = `2017`,
         RateUSDollar2016 = `2016`,
         RateUSDollar2015 = `2015`,
         RateUSDollar2014 = `2014`,
         RateUSDollar2013 = `2013`)
data_spread_debtleft <- data_externaldebtoutstanding %>% 
  spread(key = Year, value = ExternalDebtOutstandingInUSDollarMillion) %>% 
  rename(OutstandingDebtUSDollar2017 = `2017`,
         OutstandingDebtUSDollar2016 = `2016`,
         OutstandingDebtUSDollar2015 = `2015`,
         OutstandingDebtUSDollar2014 = `2014`,
         OutstandingDebtUSDollar2013 = `2013`)


# Master Dataframe --------------------------------------------------------

# join data together
data_consolidate <- data_spread_gdp %>% 
  left_join(y = data_spread_tradebalance, by = "CountryCode") %>% 
  select(RegionalMember.x:GDPRate201819, TradeBalance2013:TradeBalance2019) %>% 
  left_join(y = data_spread_exchangerate, by = "CountryCode") %>% 
  select(RegionalMember.x:TradeBalance2019, Currency:RateUSDollar2017) %>% 
  left_join(y = data_spread_debtleft, by = "CountryCode") %>% 
  select(RegionalMember.x:RateUSDollar2017, OutstandingDebtUSDollar2013:OutstandingDebtUSDollar2017) %>% 
  rename(RegionalMember = RegionalMember.x, Subregion = Subregion.x)

# Create string of all subregions
vec_subregions <- unique(x = data_tradebalance$Subregion)

# create valueBox and infoBox information
data_consolidate <- data_consolidate %>% 
  add_columns_gdp(column = data_consolidate$GDPRate201819) %>% 
  add_columns_debt(column = data_consolidate$OutstandingDebtUSDollar2017) %>% 
  add_columns_trade(column = data_consolidate$TradeBalance2019) %>% 
  # remove subregions in RegionalEconomy field
  filter(RegionalMember %!in% vec_subregions & RegionalMember != "Developing Asia excluding the Newly Industrialized Economies")


# Plot Dataframe ----------------------------------------------------------
# 1. Transform each dataframe to right format for row-binding
# NOTE (Need to functionalise this by lazy evaluation - check phone web-broswer)
data_gdp <- data_gdp %>% 
  mutate(key = "GDPGrowthperYearPercent") %>%
  rename(value = GrossDomesticProductGrowthPerYearPercentage)
data_tradebalance <- data_tradebalance %>% 
  mutate(key = "TradeBalanceInUSDollarMillion") %>% 
  rename(value = TradeBalanceInUSDollarMillion)
data_externaldebtoutstanding <- data_externaldebtoutstanding %>% 
  mutate(key = "DebtOutstandingUSDollarMillion") %>%
  rename(value = ExternalDebtOutstandingInUSDollarMillion) 

data_plots <- data_gdp %>% 
  rbind(x = data_externaldebtoutstanding) %>% 
  rbind(x = data_tradebalance) %>% 
  # add TRUE FALSE so can get red and blue colours if below or above zero
  mutate(colour = ifelse(value < 0, TRUE, FALSE)) %>% 
  # remove 'Developing Asia excluding NIE
  filter(Subregion %!in% c("Developing Asia", "Developing Asia excluding NIEs"))


# Scaffold ----------------------------------------------------------------
scaffold_country_details <- tibble(
  `Country Details` = c("Country Code", "Country Name", "Subregion"),
  `Info 1` = rep(x = NA, times = 3),
  `Currency Details` = c("Currency Symbol", "Currency", ""),
  `Info 2` = rep(x = NA, times = 3)
)


# Table: Basic Stats ------------------------------------------------------
# remove fields not of interest
vec_basicstats_fields <- unique(x = data_basicstats$Statistic)
vec_remove_basicstats_fields <- c(
  "Material Footprint, Million Metric Tons",
  "Domestic Material Consumption, Million Metric Tons",
  "Coverage of Protected Areas in Relation to Marine Areas",                                                                                   
  "Forest Area as a Proportion of Total Land Area",                                                                                            
  "Proportion of Children under 5 Years of Age Whose Births Have Been Registered with a Civil Authority",
  "Neonatal Mortality Rate",                                                                                                                   
  "Tuberculosis Incidence",                                                                                                                    
  "Mortality Rate Attributed to Cardiovascular Disease, Cancer, Diabetes, or Chronic Respiratory Disease",                                     
  "Participation Rate in Organized Learning (1 year before the official primary entry age), Female",                                           
  "Participation Rate in Organized Learning (1 year before the official primary entry age), Male",                                             
  "Participation Rate in Organized Learning (1 year before the official primary entry age), Total",                                            
  "Proportion of Teachers Who Have Received at Least the Minimum Organized Teacher Training, Lower Secondary",                                 
  "Proportion of Teachers Who Have Received at Least the Minimum Organized Teacher Training, Preprimary",                                      
  "Proportion of Teachers Who Have Received at Least the Minimum Organized Teacher Training, Primary",                                         
  "Proportion of Teachers Who Have Received at Least the Minimum Organized Teacher Training, Upper Secondary",                                 
  "Proportion of Seats Held by Women in National Parliaments",                                                                                 
  "Proportion of Population Using Safely Managed Drinking Water Services, Rural",                                                              
  "Proportion of Population Using Safely Managed Drinking Water Services, Total",                                                              
  "Proportion of Population Using Safely Managed Drinking Water Services, Urban",
  "Number of ATMs",                                                                                                                            
  "Number of Commercial Bank Branches",
  "Annual Growth Rate of Real GDP per Employed Person",
  "Total Official Flows for Infrastructure, Official Development Assistance (ODA)",                                                            
  "Total Official Flows for Infrastructure, Other Official Flows (OOF)",                                                                       
  "Proportion of Population Covered by Mobile Network, 2G",
  "Age Dependency Ratio",
  "Annual Growth Rate of Gross Domestic Product (GDP)",                                                                                        
  "Annual Real Growth Rates on Value Added, Agriculture",                                                                                      
  "Annual Real Growth Rates on Value Added, Industry",                                                                                         
  "Annual Real Growth Rates on Value Added, Services",                                                                                         
  "Average Annual Population Growth Rate",
  "External Debt, Total Outstanding",
  "Per Capita Gross National Income (GNI), Atlas Method",
  "Trade Balance",                                                                                                                             
  NA
  )
vec_basicstats_fields <- vec_basicstats_fields[-which(vec_basicstats_fields %in% vec_remove_basicstats_fields)]

# remove unecessary Statistic entries
data_basicstats <- data_basicstats %>% 
  filter(Statistic %in% vec_basicstats_fields) %>% 
  mutate(
    Statistic = as.factor(Statistic),
    Year = as.factor(Year),
    SustainableDevelopmentGoal = as.factor(SustainableDevelopmentGoal)
  )

# Subregion: Plot Dataframe ---------------------------------------------
data_plots_region <- data_plots %>% 
  select(-c(RegionalMember, colour)) %>% 
  group_by(Subregion, key) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE))
data_plots_region$mean_value <- round(x = data_plots_region$mean_value, digits = 2)

rm(data_exchangerate, data_externaldebtoutstanding, data_gdp, data_tradebalance,
   data_spread_debtleft, data_spread_exchangerate, data_spread_gdp, data_spread_tradebalance)

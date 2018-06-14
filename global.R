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

# data import and manipulation
library(readr)
library(dplyr)
library(ggplot2)



# Data Import -------------------------------------------------------------
data_gdp <- read_csv(file = "data/ADO_GDP_201819AreForecasts.csv")
data_basicstats <- read_csv(file = "data/BasicStatistics_2018.csv")
data_exchangerates <- read_csv(file = "data/ADO_ExchangeRates_2018.csv")
data_tradebalance <- read_csv(file = "data/ADO_TradeBalance_201819AreForecasts.csv")
data_externaldebtoutstanding <- read_csv(file = "data/ADO_ExternalDebtOutstanding_2018.csv")




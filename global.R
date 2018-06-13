# -------- #
# global.R #
# -------- #

# DESC: global.R script used to create static objects that app needs to run.
#       Includes loading and manipulating daya, and defining functions to be used in server.R

# Install these packages before running the app:
# install.packages(c("shiny", "shinydashboard", "DT", "RODBC", "tidyverse", "scales"))
# install.packages("shinyBS")

# shiny app development and appearance
library(shiny)
library(shinydashboard)
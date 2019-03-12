# Create an account on shiniapps.io to deploy your first app. This
# procedure together will take approximately 10 minutes.

# - Go to: https://www.shinyapps.io/admin/#/signup
# 
# - Create an account (name + password). You can use whatever email
#   you like.
# 
# - Fill in your tokens (accountname + token + secret) in the code
#   below
# 
# - Run the code until line 15 (the rest is for the workshop today)
# 
# - In Line 21, set appDir= an empty folder that you will use for
#   today’s class. Use complete path (example: appDir =
#   "C:/Users/david/Google
#   Drive/R_learning/shiny/shiny.rodrigo/nda-assignment-similarity” )



install.packages("shiny")
install.packages("rsconnect") # used to deploy
library(igraph)
library(shiny)
library(data.table)
library(rsconnect) 
library(markdown)
library(shinydashboard)
library(data.table)     
library(ggplot2)        
library(stringr)
#library(dygraphs)
library(plyr)
library(scales)
library(plotly)
library(shinyWidgets)
# library(dashboardthemes)



rsconnect::setAccountInfo(name='social-network-explorer',
                          token='5CC775A5ADDC535623D94AA1E93508DE',
                          secret='1RTTI8b+aLukwMbh1HUFi22RNzXnF/lp4Asx3Nuz')
setwd("/Users/Desktop/Depoly/")
deployApp('Deploy')

#version 0.91

##############################################
###install required libraries if neccessary###
##############################################

# install.packages("leaflet")
# install.packages("shiny")

###########################
###set working directory###
###########################

# if you haven't already set the working directory to whereever you put "argoslook-dev"
#setwd("/path/to/argoslook-dev")

#########################
###download argos data###
#########################

#download latest data on argos as PRV/A DS
#extract .DSA file using DAP processor to their own directory.
#make sure to use a filter so you can add the GmTagXXX, ZcTagXXX easy names
#this generates a DeployID column in the output files which I use
#
#the files should be "*-Argos.csv", "*-Behavior.csv", "*-Corrupt.csv",
#"*-Locations.csv", "*-Histos.csv", "*-RTC.sv", "*-Status.csv", "*-Summary.csv"
#

#############
###the app###
#############

#start shiny app by running these four lines
#hit quit button to shutdown and then close the browser window
#if you accidentally quit the browser without exiting the app, just hit escape in the R console window to get R back or just exit R.


source("guts/argoslook.R")
if(interactive()) {
shinyApp(ui, server)
}


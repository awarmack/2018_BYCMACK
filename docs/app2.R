#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(leaflet)
library(ggplot2)
library(tidyverse)

load("expdat.rda")
load("mark.rda")

expdat$polar_bsp_perc[is.infinite(expdat$polar_bsp_perc)] <- NA

#plot variables
plotchoices <- c("bsp", "awa", "aws", "twa", "tws", "twd", "cog", "sog", "polar_bsp_target", "polar_bsp_perc")


ui <- fluidPage(
  fluidRow(
   Column(6, 
          titlePanel("2018_longDistanceRace")
          ), 
   Column(6
        
          )
  )
  

)

server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

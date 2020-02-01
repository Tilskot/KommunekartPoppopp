
library(shiny)
library(highcharter)

shinyUI(fluidPage(
  mainPanel(
    highchartOutput("kart",height = "500px")
  )
))
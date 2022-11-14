library(shiny)
library(shinyWidgets)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)

ui <- bootstrapPage(title = "Maternal Immunisation Coverage",
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    absolutePanel(top = 10, right = "auto", left = 10, bottom = "auto", draggable = TRUE, width = 300, height = "auto", style = "opacity: 0.85;z-index: 1000",
                                  wellPanel(
                                    # h2("PM in Chch"),
                                    # radio buttons
                                    div(style = "margin-bottom: 0px; margin-top: 0px; text-align: center;", HTML("<a href='https://www.canterbury.ac.nz/science/research/geohealth/'><img style='width: 227px;' src='GHL_plum.png' /></a>")),
                                    radioButtons("category","I want to show immunisation rates:", c("Overall" = "Total", "By ethnicity" = "eth"), selected = "Total", inline = T),
                                    # conditional checklist panel
                                    # Filtering by topic based on the conditions from selectInput - important is to name all inputs differently
                                    conditionalPanel(
                                      condition = "input.category == 'eth'",
                                      selectInput("variableeth", "Choose ethnicity:", choices = c("NZ European" = "European", "M\u101ori" = "Maori", "Pasifika", "Asian", "Other"))
                                    ),
                                    radioButtons("imms", "Choose immunisation type:", choices = c("Influenza", "Pertussis"), inline = T),
                                    # slider time using text to define the intervals precisely
                                    sliderTextInput("datetime", "Select year", choices = 2013:2021, animate = animationOptions(interval=1000, loop=T), grid = T), 
                                  )
                                  ),
                    absolutePanel(top = 10, right = 10, left = "auto", bottom = "auto", draggable = TRUE, width = 450, height = "auto", style = "opacity: 0.85;z-index: 1000",
                              wellPanel(
                                    # plotting graph
                                    tabsetPanel(
                                      tabPanel("Immunisation rates by year",
                                               # plotting graph
                                               # plotOutput("empty", height = 5),
                                               plotlyOutput("taplot", height = 600)
                                      ),
                                      tabPanel("Immunisation rates by TA",
                                               # plotting graph
                                               plotlyOutput("timeplot", height = 500)
                                      )
                                  )
                              )#,
                                  # style = "opacity: 0.9;z-index: 1000"
                    )
)
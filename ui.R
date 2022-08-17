
library(shiny)
library(tidyverse)
library(plotly)
library(magrittr)
library(leaflet)
library(shinyWidgets)
library(DT)


ui <- fluidPage(
  
  titlePanel("Go Grid"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      pickerInput('state',
                  'State', 
                  choices = c('AL','AK','AZ','AR','CA','CO','CT','DC','DE','FL',
                              'GA','HI','ID','IL','IN','IA','KS','KY','LA','ME',
                              'MD','MA','MI','MN','MS','MO','MT','NE','NV','NH',
                              'NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI',
                              'SC','SD','TN','TX','UT','VT','VA','WA','WV','WI',
                              'WY'),
                  options = list(`actions-box` = TRUE),
                  selected = c('DC', 'DE', 'MD', 'WV', 'VA'),
                  multiple = TRUE),
      
      selectInput(
        inputId = 'case',
        label = 'Case',
        choices = c('REF22', 'HOG22', 'LCR22'),
        selected = 'REF22',
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      selectInput(
        inputId = 'year',
        label = 'Year (Base Year = 2016)',
        choices = c('2016', '2023', '2026', '2031'),
        selected = '2016',
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      selectInput(
        inputId = 'variable',
        label = 'Variable',
        choices = c('Annual NOx (tons)', 'Annual SO2 (tons)', 'OS NOx (tons)', 'NonOS NOx (tons)'),
        selected = 'Annual NOx (tons)',
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      pickerInput('data_type',
                  'Data type/status to include', 
                  choices = c('FULL', 'RETIRED', 'NEW', 'PARTIAL', 'SWITCH'),
                  options = list(`actions-box` = TRUE),
                  selected = c('FULL', 'NEW', 'PARTIAL', 'SWITCH'),
                  multiple = TRUE),
      
      selectInput(inputId = 'conus',
                  label = 'Include off-grid industrial units?',
                  choices = c('Yes', 'No'),
                  selected = 'No'),
      
      selectInput(
        inputId = 'circle_size',
        label = 'Scale Circle Size?',
        choices = c('Yes', 'No'),
        selected = 'Yes',
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      

      textOutput('info')
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots & Map", plotlyOutput("plot1"), leafletOutput("map")),
        tabPanel("More Plots", plotlyOutput("plot")),
        tabPanel("Table - Selected Data Used in Map", DT::dataTableOutput("table"))
      )
    )
  )
)
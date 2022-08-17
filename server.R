#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(magrittr)
library(leaflet)
library(shinyWidgets)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$info <- renderText({ 
    '
    
    For more information:  
    https://www.marama.org/technical-center/ertac-egu-projection-tool/  
    https://www.eia.gov/outlooks/aeo/
    https://www.eia.gov/outlooks/aeo/assumptions/pdf/case_descriptions_2022.pdf
    '
    })

    output$map <- renderLeaflet({
      
      data1 <- read_csv('REF-HOG-LCR_AEO22_AEO20_GoGrid.csv') %>%
        select(-c(1)) %>% filter(State %in% input$state & 
                                 `Case?` == input$case &
                                 Year == input$year &
                                 variable == input$variable &
                                `data type` %in% input$data_type) %>%
        arrange(-value)
        
      conus <- input$conus
      data1 <- if(conus == 'No') filter(data1, `ertac region` != 'CONUS') else data1

      
      data1$value_lvl <- cut(data1$value, 
                             c(0,100,625,1250,2500,5000,7500,10000, 100000), include.lowest = T,
                             labels = c('<100', '100-625', '625-1250', '1250-2500', '2500-5000', '5000-7500', '7500-10000', '10000+'))

      
      mycolors <- colorFactor(palette = c("#ED513EFF", "#D92847FF", "#B91657FF", "#921C5BFF", "#6A1F56FF", "#451C47FF", "#221331FF", "#03051AFF"), data1$value_lvl)
      
      

      
      
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addLegend('bottomright', pal = mycolors, values = data1$value_lvl,
                  title = paste0(input$year, ' ', input$variable),
                  opacity = .85)
      
      ifelse(input$circle_size == 'Yes', 
             
             map <- map %>% addCircleMarkers(lng=data1$Longitude, 
                               lat=data1$Latitude, 
                               popup=paste(paste(data1$value, data1$variable),
                                           paste('Facility:',data1$`Facility Name`), 
                                           data1$fuel, paste('Retirement:', data1$`Retirement Date`), 
                                           sep = "<br />"), 
                               radius = log(data1$value)*1.5,
                               color = mycolors(data1$value_lvl),
                               opacity = .75),
             
             map <- map %>% addCircleMarkers(lng=data1$Longitude, 
                                lat=data1$Latitude, 
                                popup=paste(paste(data1$value, data1$variable),
                                            paste('Facility:',data1$`Facility Name`), 
                                            data1$fuel, paste('Retirement:', data1$`Retirement Date`), 
                                            sep = "<br />"), 
                                radius = 2, 
                                color = mycolors(data1$value_lvl),
                                opacity = .5
                                )
             )
        
      map
             
      
      
      
    })
    
    
    output$plot <- renderPlotly({
      
      data <- read_csv('REF-HOG-LCR_AEO22_AEO20_GoGrid.csv') %>%
        select(-c(1))
      
      
      data %<>% filter(State %in% input$state & 
                       variable == input$variable &
                       `Case?` %in% c('REF22', 'HOG22', 'LCR22'))
      
      conus <- input$conus
      data <- if(conus == 'No') filter(data, `ertac region` != 'CONUS') else data
      palette <- if(conus == 'No') viridis::rocket(5, alpha = 1, begin = 0, end = 1, direction = -1) else viridis::rocket(9, alpha = 1, begin = 0, end = 1, direction = -1)
      
      
      data <- aggregate(data$value, by = data[c('Case?', 'Year', 'fuel', 'variable')], FUN = sum) %>%
        mutate(value = x) %>%
        select(-x) 
      
      data$fuel %<>% factor(levels = c('Institutional', 'Industrial','Simple Cycle Gas',  'Oil', 'Boiler Gas', 'EGU-Other','Combined Cycle Gas', 'Coal'))
      
      
      p <- data %>% ggplot(aes(x= Year, y= value, color = fuel))  +
        geom_line(size = .5, alpha = .75) +
        geom_point(alpha = .8) +
        facet_wrap(~`Case?`) +
        theme_minimal() + 
        ggtitle(paste0(input$variable, ' from EGUs in Selected States - Case Comparison')) +
        xlab('Year') +
        ylab(paste0(input$variable,' by facility')) +
        scale_color_manual(values=palette)
        

        
      
      ggplotly(p)
      
      
    })
    
    output$plot1 <- renderPlotly({
      
      data <- read_csv('REF-HOG-LCR_AEO22_AEO20_GoGrid.csv') %>%
        select(-c(1))
      
      
      data %<>% filter(State %in% input$state & 
                         variable == input$variable &
                         `Case?` %in% c('REF22', 'HOG22', 'LCR22'))
      
      conus <- input$conus
      data <- if(conus == 'No') filter(data, `ertac region` != 'CONUS') else data
      palette <- if(conus == 'No') viridis::rocket(5, alpha = 1, begin = 0, end = 1, direction = -1) else viridis::rocket(9, alpha = 1, begin = 0, end = 1, direction = -1)
      
      data <- aggregate(data$value, by = data[c('Case?', 'Year', 'fuel', 'variable')], FUN = sum) %>%
        mutate(value = x) %>%
        select(-x) 
      
     # label_sum <- aggregate(data$value, by = data[c('Case?', 'Year', 'variable')], FUN = sum)
      
      data$fuel %<>% factor(levels = c('Institutional', 'Industrial','Simple Cycle Gas',  'Oil', 'Boiler Gas', 'EGU-Other','Combined Cycle Gas', 'Coal'))
      
      
      p <- data %>% 
        ggplot(aes(x= as.factor(Year), y= value, fill = fuel)) +
        geom_col() +
        facet_wrap(~`Case?`) +
        theme_minimal() + 
        ggtitle(paste0(input$variable, ' from EGUs in Selected States - Case Comparison')) +
        xlab('Year') +
        ylab(paste0(input$variable,' by facility')) +
        scale_fill_manual(values=palette) 

      ggplotly(p)
      
      
    })
    
    output$table <- DT::renderDataTable({

      data1 <- read_csv('REF-HOG-LCR_AEO22_AEO20_GoGrid.csv') %>%
        select(-c(1))

      data1 <- data1 %>% filter(State %in% input$state & 
                               `Case?` == input$case &
                               Year == input$year &
                               variable == input$variable &
                               `data type` %in% input$data_type)
      
      conus <- input$conus
      data1 <- if(conus == 'No') filter(data1, `ertac region` != 'CONUS') else data1

      
      data1$value_lvl <- cut(data1$value,
                             c(0,100,625,1250,2500,5000,7500,10000, 100000), include.lowest = T,
                             labels = c('<100', '100-625', '625-1250', '1250-2500', '2500-5000', '5000-7500', '7500-10000', '10000+'))
        
      data1 %<>% arrange(value)
        
      data1
      
    })

})

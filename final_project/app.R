#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(kableExtra)
library(tidytext)
library(knitr)
library(janitor)
library(lubridate)
library(stringr)
library(fs)
library(formattable)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(leaflet)
library(rgdal)
library(maps)
library(mapproj)
library(fiftystater)
library(sf)

leading_deaths <- read.csv("https://data.cdc.gov/api/views/bi63-dtpu/rows.csv?accessType=DOWNLOAD")

us_states_leading_deaths_2 <- read_rds("us_states_leading_deaths.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(fluidPage(theme = shinytheme("cerulean")),
                
                # Application title
                titlePanel("Leading Causes of Death in the United States"),
                
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                   
                    selectInput(inputId = "x",
                                label = "Year:",
                                choices = unique(leading_deaths$Year),
                                selected = "1999"),
                    
                  
                    
                    
                    selectInput(inputId = "Cause.Name", 
                                label = "Cause of Death:",
                                choices = unique(leading_deaths$Cause.Name),
                                selected = "Unintentional injuries")),
                  
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    
                    
                    tabsetPanel(type = "tabs",
                                tabPanel("About", htmlOutput("about")),
                                tabPanel("The Top Causes", htmlOutput("top_causes")),
                                tabPanel("Map", plotOutput("myMap")),
                                tabPanel("Bar Graph", plotlyOutput("myPlot")),
                                tabPanel("Scatter Plot", plotlyOutput("myPlot2")),
                                tabPanel("Resources", htmlOutput("resources")))
                    
                    
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  

  output$myPlot <- renderPlotly({
    
    myPlot1 <- leading_deaths %>%
      select(Year, Deaths, State, Cause.Name) %>%
      filter(Year == input$x) %>%
      filter(State == "United States", Cause.Name != "All causes") %>%
      ggplot(aes(x = Cause.Name, y = Deaths, fill = Cause.Name)) + geom_bar(stat = "identity") +
      xlab("Year") + ylab("Number of Deaths") + 
      ggtitle("Leading Causes of Death in the United States 1999-2016") +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    
    require(scales)
    myPlot1 + scale_y_continuous(labels = comma)
    
    
  })
  
  output$myPlot2 <- renderPlotly({
    
    
    
    
    myPlot2 <- leading_deaths %>%
      filter(State == "United States", Cause.Name != "All causes") %>%
      filter(Cause.Name == input$Cause.Name) %>%
      ggplot(aes(x = Year, y = Deaths)) + geom_point() + geom_smooth()
    
    require(scales)
    myPlot2 + scale_y_continuous(labels = comma)
    
    
    
    
    
  })
  
  
  
  output$myMap <- renderPlot({
    
    myMap <- 
      
     us_states_leading_deaths_2 <-  us_states_leading_deaths_2 %>%
      filter(Year == input$x) %>%
      filter(Cause.Name == input$Cause.Name)
    
    ggplot(data = us_states_leading_deaths_2) +
      geom_sf(aes(fill = Deaths))
    
    
  })
  
  output$about <- renderUI({
    
    # Provide users with a summary of the application and instructions
    # Provide users with information on the data source
    
    str1 <- paste("About")
    str2 <- paste("Leading Causes of Death")
    str3 <- paste("What to explore?") 
    str4 <- paste("Graphs. Data")
    str5 <- paste("Source")
    str6 <- paste("CDC")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
  
  output$resources <- renderUI({
    
    #Resources about the leading causes of death 
    
    
    str1 <- paste("Fill")
    str2 <- paste("Fill")
    str3 <- paste("Fill") 
    str4 <- paste("Fill")
    str5 <- paste("Fill")
    str6 <- paste("Fill.")
    str7 <- paste("Fill")
    str8 <- paste("Fill")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p(str8)))})
  
  
  output$top_causes <- renderUI({
    
    #Explaining the leading causes of death
    
    str1 <- paste("CLRD")
    str2 <- paste("Chronic Lower Respiratory Disease")
    str3 <- paste("Fill") 
    str4 <- paste("Fill")
    str5 <- paste("Fill")
    str6 <- paste("Fill.")
    str7 <- paste("Fill")
    str8 <- paste("Fill")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p(str8)))})}
  
  





# Run the application 
shinyApp(ui = ui, server = server)

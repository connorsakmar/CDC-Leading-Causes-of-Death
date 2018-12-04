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
library(tidyverse)
library(leaflet)
library(rgdal)
library(maps)
library(mapproj)
library(fiftystater)
library(sf)

leading_deaths <- read.csv("https://data.cdc.gov/api/views/bi63-dtpu/rows.csv?accessType=DOWNLOAD")

leading_deaths <- leading_deaths


us_states_leading_deaths_2 <- read_rds("us_states_leading_deaths.rds")


#Most of the comments for this code are included in the markdown file. 

# Define UI for application that draws a histogram
ui <- fluidPage(fluidPage(theme = shinytheme("cerulean")),
                
                # Application title
                titlePanel("Leading Causes of Death in the United States"),
                
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                   
                    #Allows every year in the dataset to be part of a  pulldown tab. 
                    selectInput(inputId = "x",
                                label = "Year:",
                                choices = unique(leading_deaths$Year),
                                selected = "1999"),
                    
                  
                    
                    #This code allows every cause of death in the dataset to be part of a pulldown *except* for all.causes. 
                    #As stated in the markdown file, the 2016 data for all causes is incomplete and there is such a high
                    #death toll for all causes compared to the singular causes of death that the scaling for the graphs 
                    #are not as balanced. 
                    selectInput(inputId = "Cause.Name", 
                                label = "Cause of Death:",
                                choices = c("Alzheimer's Disease" = "Alzheimer's disease", "Cancer" = "Cancer", "CLRD" = "CLRD", "Diabetes" = "Diabetes",
                                            "Heart Disease" = "Heart disease", "Influenza and Pneumonia" = "Influenza and pneumonia", "Kidney Disease" = "Kidney disease",
                                            "Stroke" = "Stroke", "Suicide" = "Suicide", "Unintentional Injuries" = "Unintentional injuries"),
                                selected = "Unintentional Injuries")),
                  
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    
                    #Created tabs for each plot in addition to informational tabs for people that have no idea what the app is.
                    #Also made a tab that makes general conclusions about each plot.
                    tabsetPanel(type = "tabs",
                                tabPanel("About", htmlOutput("about")),
                                tabPanel("Map", plotOutput("myMap")),
                                tabPanel("Bar Graph", plotlyOutput("myPlot")),
                                tabPanel("Scatter Plot", plotlyOutput("myPlot2")),
                                tabPanel("General Conclusions", htmlOutput("conclusions")))
                    
                    
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
      #Added a better gradient so that the differences between states are more obvious. 
      geom_sf(aes(fill = Deaths)) + scale_fill_gradient(low = "white", high = "#009E73")
    
    
  })
  
  output$about <- renderUI({
    
    # Provide users with a summary of the application and what it is meant to 
    # Provide useres with a some details about each plot. 
    # Provide users with information on the data source
    
    str1 <- paste("About")
    str2 <- paste("This shiny shows the leading causes of death in the United States from 1999-2016. The data includes the top 10 causes 
                  of death over that time, which are: Alzheimer's Disease, Cancer, CLRD (Chronic Lower Respiratory Disease), Diabetes,
                  Heart Disease, Influenza and Pneumonia, Kidney Disease, Stroke, Suicide, and Unintentional Injuries.")
    str3 <- paste("What to explore?") 
    str4 <- paste("There are three plots that you can look at: a map, a bar graph, and a scatter plot. The map, as one might guess, maps
                  the causes of death across the United States. A user can select both the year and cause of death that they want to see
                  mapped across all 50 states. The bar graph allows the user to compare all of the leading of causes of death
                  in the United States for a given year. The user can select which year they would like to compare the total deaths in the United
                  States for each cause of death. Finally, the scatter plot allows the user to see the trend in deaths for any given cause of death
                  from 1999-2016.")
    str5 <- paste("Source")
    str6 <- paste("This dataset came from the Center for Disease Control which compiled data from the National Center for Health Statistics.
                  Cause of death statistics are based on the underlying cause of death.")
    
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6)))})
  
  output$conclusions <- renderUI({
    
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

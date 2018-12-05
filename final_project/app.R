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


#Importing the RDS file made in the markdown file. 
us_states_leading_deaths_2 <- read_rds("us_states_leading_deaths.rds")



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
  
  
  #See markdown file for additional comments on this code.

  output$myPlot <- renderPlotly({
    
    myPlot1 <- leading_deaths %>%
      select(Year, Deaths, State, Cause.Name) %>%
      filter(Year == input$x) %>%
      filter(State == "United States", Cause.Name != "All causes") %>%
      ggplot(aes(x = Cause.Name, y = Deaths, fill = Cause.Name)) + geom_bar(stat = "identity") +
      xlab("Year") + ylab("Number of Deaths") + 
      ggtitle("Comparison of Leading Causes of Death in the U.S. 1999-2016") +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      
    #  Changed the legend title to "Cause of Death" rather than Cause.Name
      guides(fill=guide_legend(title="Cause of Death"))
    
    require(scales)
    myPlot1 + scale_y_continuous(labels = comma)
    
    
  })
  
  output$myPlot2 <- renderPlotly({
    
    
  #See markdwon file additional comments on this code.
    
    myPlot2 <- leading_deaths %>%
      filter(State == "United States", Cause.Name != "All causes") %>%
      filter(Cause.Name == input$Cause.Name) %>%
      
      #Added a title to the scatter plot
      ggplot(aes(x = Year, y = Deaths)) + geom_point() + geom_smooth() + ggtitle("Trends of Leading Causes of Death in the U.S. 1999-2016") 
    
    require(scales)
    myPlot2 + scale_y_continuous(labels = comma)
    
    
    
    
    
  })
  
  
  
  output$myMap <- renderPlot({
    
    #See markdown file for additional comments on this code. 
    
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
    str3 <- paste("What to Explore") 
    str4 <- paste("There are three plots that you can look at: a map, a bar graph, and a scatter plot. The map, as one might guess, maps
                  the causes of death across the United States. A user can select both the year and cause of death that they want to see
                  mapped across all 50 states. The bar graph allows the user to compare all of the leading of causes of death
                  in the United States for a given year. The user can select which year they would like to compare the total deaths in the United
                  States for each cause of death. Finally, the scatter plot allows the user to see the trend in deaths for any cause of death
                  that they select from 1999-2016.")
    str5 <- paste("Source")
    str6 <- paste("This dataset came from the Center for Disease Control which compiled data from the National Center for Health Statistics.
                  Cause of death statistics are based on the underlying cause of death.")
    
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6)))})
  
  output$conclusions <- renderUI({
    
    #General conclusions made from looking at the three plots. 
    
    
    str1 <- paste("Map")
    str2 <- paste("There is obviously a high correlation with population size and the amount of deaths in a state, which explains why California and
                  Texas are always the most darkly shaded. However, the map is really interesting because it shows differences between eastern states and
                  those in the Midwest; states that have relatively similar populations. It appears the many states in the midwest tend to be on the lower
                  end of the death spectrum for most causes than some of the smaller eastern states.")
    str3 <- paste("Bar Graph") 
    str4 <- paste("The most distinctive aspect of the box plot is that heart disease and cancer are by far the two highest causes of death in the United States
                  in comparison to the other eight top causes of death.CLRD, stroke, and unintentional injuries have also consistenly been in the top five causes of death,
                  yet each of these accounts for less than a third of the deaths caused by heart disease (the leading cause of death) each year.")
    str5 <- paste("Scatter Plot")
    str6 <- paste("The scatter plot shows some striking trends in the leading causes of death. Seven of the ten causes have generally trended upward since 1999. 
                  The three causes that don't show this trend are stroke, heart disease, and influenza and pneumonia. These all trended downards until around 2010,
                  when they began to slowly trend upwards again. While it is hard to speculate as to why this trend happens with all 3, one might speculate that there
                  might have been due decline in deaths in the eldery population prior to 2010, as they are especially susceptible to these three causes. 
                   ")
    str7 <- paste("Conclusion")
    str8 <- paste("The leading causes of death continue to rise and trend upward. We can hope that modern science can advance far enough that deaths from these
                  causes will decline in the near future. Hopefully you enjoyed looking at this data and that it has encouraged you to look more closely and
                  investigate some of the trends or patterns that you found!")
    
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6), h1(str7), p(str8)))})}
  
  

  



# Run the application 
shinyApp(ui = ui, server = server)

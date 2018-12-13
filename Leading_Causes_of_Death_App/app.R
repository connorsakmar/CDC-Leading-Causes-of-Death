#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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



#Importing the RDS file made in the markdown file.
us_states_leading_deaths_2 <- read_rds("us_states_leading_deaths.rds")

 
  ui <- fluidPage(fluidPage(theme = shinytheme("superhero")),
                  
                  # Title of the Shiny App
                  titlePanel("Leading Causes of Death in the United States 1999-2016"),
                  
                  
                  # Sidebar that contains the input functions for Year, Death Statistic, and Cause of Death.
                  sidebarLayout(
                    sidebarPanel(
                      
                      #This ensures that the Death Statistic pull-down menu appears when either the map or the scatterplot is selected.
                      conditionalPanel(condition = "input.tabs == 'Map'|| input.tabs =='Scatter Plot'",
                                       
                      #Allows the user to choose between total deaths and age-adjusted death rate. 
                      selectInput(inputId = "z",
                                  label = "Death Statistic:",
                                  choices = c("Total Deaths" = "Deaths", "Age-Adjusted Death Rate" = "Age.adjusted.Death.Rate"),
                                  selected = "Total Deaths")),
                      
                      #This ensures that the Year pull-down menu appears when either the map or the bar graph is selected. 
                      conditionalPanel(condition = "input.tabs == 'Map'||input.tabs == 'Bar Graph'",
                      
                      #Allows every year in the dataset to be part of a pull-down tab. 
                      selectInput(inputId = "x",
                                  label = "Year:",
                                  choices = unique(leading_deaths$Year),
                                  selected = "1999")),
                      
                      
                      
                      #This ensures that the cause of death pull-down menu appears when either the map or the scatterplot is selected.
                      conditionalPanel(condition = "input.tabs == 'Map'|| input.tabs == 'Scatter Plot'",
                      
                      #This code allows every cause of death in the dataset to be part of a pull-down menu *except* for all.causes. 
                      #As stated in the markdown file, the 2016 data for all causes is incomplete and there is such a high
                      #death toll for all causes compared to the singular causes of death that the scaling for the graphs 
                      #is not as balanced. 
                      selectInput(inputId = "Cause.Name", 
                                  label = "Cause of Death:",
                                  choices = c("Alzheimer's Disease" = "Alzheimer's disease", "Cancer" = "Cancer", "CLRD" = "CLRD", "Diabetes" = "Diabetes",
                                              "Heart Disease" = "Heart disease", "Influenza and Pneumonia" = "Influenza and pneumonia", "Kidney Disease" = "Kidney disease",
                                              "Stroke" = "Stroke", "Suicide" = "Suicide", "Unintentional Injuries" = "Unintentional injuries"),
                                  selected = "Unintentional Injuries"))),
                      
                      
                 
                    # The following code makes the distinct tabs of the shiny app and also contains my conclusions for each visual. 
                    mainPanel(
                      
                      #Created tabs for each plot in addition to informational tabs for people who are unfamiliar with the subject matter.
                      #Also made a tab that makes conclusions based on the the data as a whole. 
                      tabsetPanel(id = "tabs",
                                  tabPanel("About", htmlOutput("about")),
                                  tabPanel("Map", plotOutput("myMap"),
                                          
                                            
                                  #Adding notable observations for the map underneath the visual. 
                                           br(),
                                           br(),
                                           br(),
                                           p("There is obviously a high correlation with population size and the amount of deaths in a state, which explains why California and
                                              Texas are always the most darkly shaded when total deaths is selected. For this reason, it is hard to tell if there are any correlations between causes of death 
                                              and certain geographic ares. However, when age-adjusted death rate is selected, certain states and areas become distinctive. For example, New York,
                                              in 2016, had the lowest age-adjusted death rate in the country for Alzheimer's Disease, while Utah has consistently had the lowest age-adjusted death
                                              rate in the country for cancer. Perhaps trends like these indicate a healthier lifestyle for people within those states, and if so, could lead to further 
                                              research into how those lifestyles lower death rates.
                                             ")),
                                  
                                  tabPanel("Bar Graph", plotlyOutput("myPlot"),
                                           
                                  #Adding notable observations for the bar graph underneath the graphic. 
                                  
                                           br(),
                                           br(),
                                           br(),
                                           p("The most distinctive aspect of the bar graph is that heart disease and cancer are by far the two highest causes of death in the United States
                                             in comparison to the other eight top causes of death.CLRD, stroke, and unintentional injuries have also consistenly been in the top five causes of death,
                                             yet each of these accounts for less than a third of the deaths caused by heart disease (the leading cause of death) each year.")),
                                  
                                  tabPanel("Scatter Plot", plotlyOutput("myPlot2"),
                                           
                                  #Adding notable observations for the scatter plot underneath the graphic. 
                                  
                                           br(),
                                           br(),
                                           br(),
                                           p("The scatter plot shows some striking trends in the leading causes of death. Seven of the ten causes have generally trended upward since 1999 in terms of toatal deaths.
                                              The three causes that don't show this trend are stroke, heart disease, and influenza and pneumonia. These all trended downards until around 2010,
                                             when they began to slowly trend upwards again. However, only three causes are trending upwards in terms of age-adjusted death rate. This means that deaths are increasing because population size is increasing,
                                              yet the actual number of deaths per year in proportion to the population increase is actually decreasing for most causes of death. Suicide and unintentional injuries
                                              are two of the causes that are trending upwards, which makes sense as they are not causes of death that are readily curable or treatable like most of the other diseases.
                                              Alzheimer's disease is the only disease that has an age-adjusted death rate that has increased over the last several years, which indicates that American medicine
                                             has not yet figured out a viable treatment for the disease, or at least not one that has made a noticeable impact on the age-adjusted death rate." 
                                            )),
                                  tabPanel("General Conclusions", htmlOutput("conclusions")))
                      
                      
                    )
                      )
  )
                  

# Defines server logic required to draw the bar graph, scatter plot, and map. 
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
    
    
  #See markdown file for additional comments on this code.
    
    scatter_plot <- leading_deaths %>%
      filter(State == "United States") %>%
      filter(Cause.Name == input$Cause.Name) %>%
  
      
  #Added a title to the scatter plot.
  #Added the option to choose between total deaths and age-adjusted death rates for the scatter plot.
  #Added a y lab title that made since for when a user switches between total deaths and age-adjusted death rate.
     ggplot(aes_string(x = "Year" , y = input$z)) + geom_point() + geom_smooth() + 
      ggtitle("Trends of Leading Causes of Death in the U.S. 1999-2016") + labs( y = "Death Statistic", x = "Year")
    
    
    require(scales)
    scatter_plot + scale_y_continuous(labels = comma)
    
    
    
    
    
  })
  
  
  
  output$myMap <- renderPlot({
    
    #See markdown file for additional comments on this code. 
    
    myMap <- 
      
      #Applying filters with input functions to allow for drop down menus for year and cause of death. 
    us_states_leading_deaths_2 <-  us_states_leading_deaths_2 %>%
      filter(Year == input$x) %>%
      filter(Cause.Name == input$Cause.Name)
    
    
    
  ggplot(data = us_states_leading_deaths_2) +
     #Allows the user to choose between "death statistics," either Total Deaths or Age-Adjusted Death Rate
      geom_sf(aes_string(fill = input$z)) + 
        
     #Added a better gradient so that the differences between states are more obvious. 
      scale_fill_gradient(low = "white", high = "#009E73") +
    
    
    #  Changed the legend title to "Death Statistic" so that it makes sense for both Total Deaths and Age-Adjusted Death Rate.
    guides(fill=guide_legend(title="Death Statistic"))
 

  
  
    
  })
  
  output$about <- renderUI({
    
    # The about tab is straightforward and is meant to:
    # Provide users with a summary of the application and what it is meant to 
    # Provide users with a some details about each plot.
    # Clarify some terms that several people asked about when this data was initially presented. 
    # Provide users with information on the data source
    
    
    #Makes it possible to create a URL hyperlink to useful information - in this case a link 
    #that explains age-adjusted death rates. 
    url <- a("CDC Statistical Notes", href="https://www.cdc.gov/cancer/uscs/technical_notes/stat_methods/rates.htm")
    
    str1 <- paste("About")
    str2 <- paste("This shiny focuses on characteristics of the leading causes of death in the United States for 1999 -2016. This data is comprised of the
                  top ten causes of death during that time period, which includes Alzheimer's Disease, Cancer, CLRD (Chronic Lower Respiratory Disease), Diabetes,
                  Heart Disease, Influenza and Pneumonia, Kidney Disease, Stroke, Suicide, and Unintentional Injuries.")
    str3 <- paste("What to Explore") 
    str4 <- paste("Several graphical representations were employed to highlight various aspects of the data: maps, bar graphs, and scatter plots. The map, as one might guess, maps
                  the causes of death across the United States. A user can see the age-adjusted death rate and total deaths in each state for any
                  given year or cause of death. The bar graph allows the user to compare all of the leading of causes of death
                  in the United States for a given year. The user can select which year they would like to compare the total deaths in the United
                  States for each cause of death. Finally, the scatter plot allows the user to see the trend in total deaths and age-adjusted death rate
                  for any cause of death that they select from 1999-2016.")
    
    #Needed this clarification of terms heading because numerous people asked about CLRD and unintentional injuries during the intial presentation of this app. 
    #It therefore seems important to add this info to the app since I will not always be there to explain it. Also, age-adjusted death rates are an 
    #important statistic for this app so they needed to be explained fully. 
    str5 <- paste("Clarification of Terms")
    str6 <- paste("CLRD stands for Chronic Lower Respiratory Disease and is a broad term that consists of several lung diseases such as COPD, emphysema, and chronic bronchitis.
                  Unintentional Injuries refers to unplanned injuries such as falls, vehicle accidents, drownings, and other deaths of that nature. Age-adjusted death rates are important
                  because as the CDC states, they ensure 'that differences in incidence or deaths from one year to another, or between one geographic area and another, are not due to 
                  differences in the age distribution of the populations being compared.' The death rates in this data are calculated as deaths per 100,000 total population. This is especially important when mapping data, because the age distribution of a population can change
                  over time or be different in different geographic locations. For more information on age-adjustment for causes of death click here:")
    strL <- tagList(url)
    str7 <- paste("Source")
    str8 <- paste("This dataset came from the Center for Disease Control which compiled data from the National Center for Health Statistics.
                  Cause of death statistics are based on the underlying cause of death.")
    
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6), p(strL), h1(str7), p(str8)))})
  
  output$conclusions <- renderUI({
    
   #Creating a link to my github
    url2 <- a("Connor Sakmar Github", href="https://github.com/connorsakmar/CDC-Leading-Causes-of-Death")
   
    
    #General conclusions made from looking at the the three plots. These conclusions are a bit more general
    #so it makes sense to keep them distinct from the conclusions below each graphic. 
    str1 <- paste("Deceiving Statistics")
    str2 <- paste("As seen with both the map and the scatter plot, it is important to not make assumptions about certain causes of death just based off of total death
                  count alone. While yes, it is worrisome that many of the leading causes continue to rise in total deaths, the actual death rate from year to year is decreasing
                  for most of them. This is a useful thing to keep in mind the next time death statistics are used on tv or in the media.")
    str3 <- paste("Are Certain Areas Healthier?") 
    str4 <- paste("As noted on the map page, some states show much lower age-adjusted death rates than others. While it is impossible to say definitively why this may be 
                  the case, a lot of cultural and environmental factors need to be taken into account. For instance, perhaps the large Mormon population in Utah accounts for
                  healthier overall lifestyles and therefore the age-adjusted death rate due to cancer is lower than other states. The purpose of this app is to point out these
                  interesting trends and to encourage further research in the area.")
    str5 <- paste("Conclusion")
    str6 <- paste("While the leading causes of death continue to rise and trend upward, it seems that many of the death rates are lowering as medical technology advances.  We can hope 
                  that this trend will continue and that deaths from all of these causes will be in decline. Hopefully you enjoyed looking at this data and that it has encouraged you to look more closely and
                  investigate some of the trends or patterns that you found!If interested in further research on this topic, look at the data and links provided in 
                  my github:")
    str7 <- tagList(url2)
    
    
    #Creating headers with certain strings and then pasting the text below it. 
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6), h3(str7)))})}
  
  

  



# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)
library(rsconnect)
library(shinythemes)
library(plotly)
library(scales)

data <- read_rds("results.rds")

specific <- data %>% 
  filter(state_district == "nj 03")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Upshot Data and Midterm Results"),
  
  # Sidebar with a slider input for  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "characteristic",
        label = "Choose Characteristic:",
        choices = c("Age" = "ager", "Race/Ethnicity" = "race_eth"),
        selected = "ager"
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("About this app", htmlOutput("about")),
                  tabPanel("Total Polling Data", plotlyOutput("barPlot")),
                  tabPanel("NJ-03 Demographics", plotOutput("piechart")))
      )))
  

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$barPlot <- renderPlotly({
    data %>%
      ggplot(aes_string(x = input$characteristic)) + 
      geom_bar() +
      xlab(input$characteristic) + ylab("Number of Interviewees") + 
      ggtitle("Age and Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
  output$piechart <- renderPlot({
    specific %>% 
    ggplot(aes_string(x = input$characteristic, color = input$characteristic)) + geom_bar() + 
      coord_polar("y", start=0) + ggtitle("New Jersey District 3, by characteristic")})
  
  output$about <- renderUI({
    
    # Provide users with a summary of the application and instructions
    # Provide users with information on the data source
    
    str1 <- paste("Summary")
    str2 <- paste("This app shows the interviewees that Upshot used in their poll.")
    str3 <- paste("Instructions") 
    str4 <- paste("Click through the tabs to see the data in different ways and use the drop-down menu to go between different characteristics.")
    str5 <- paste("How to read the graphs")
    str6 <- paste("The first plot is a bar graph of the total interviews that Upshot conducted to create their data source.  This includes all states and districts. 
                  The graphs shows that they interviewed white people above 65 the most, this could potentially have created a bias dataset from the beginning.
                  The pie chart just shows New Jersey's 3rd district.  The graph is by count, not percentage, so the full circles were the most frequent.  In this case
                  the most interviewed people were white and between 50 and 64.  This shows the lack of diversity in the polling data and shows the discrepency in US polling data.
                  New Jersey's 3rd district demographics show that the district is much younger with an average age of 43, but it is 75% white.  This shows that the aging variable
                  could lead to bias, but race/ethnicity bias is not present.")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
}

# Run the application 
shinyApp(ui = ui, server = server)


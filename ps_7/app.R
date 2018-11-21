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
  filter(state_district == "az 06"|
         state_district == "nj 03")

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
                  tabPanel("barplot1", plotlyOutput("barPlot1")),
                  tabPanel("barplot2", plotOutput("barPlot2")))
      )))
  

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$barPlot1 <- renderPlotly({
    data %>%
      ggplot(aes_string(x = input$characteristic)) + 
      geom_bar() +
      xlab(input$characteristic) + ylab("Number of Interviewees") + 
      ggtitle("Age and Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
  output$barPlot2 <- renderPlot({
    specific %>% 
    ggplot(aes(x = "", fill = input$characteristic)) +
      geom_bar(width = 1) + coord_polar(theta = "y" , start=0)})
  
  output$about <- renderUI({
    
    # Provide users with a summary of the application and instructions
    # Provide users with information on the data source
    
    str1 <- paste("Summary")
    str2 <- paste("This app shows the interviewees that Upshot used in their poll.")
    str3 <- paste("Instructions") 
    str4 <- paste("Click through the tabs to see the data in different ways and use the drop-down menu to go between different characteristics.")
   
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4)))})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


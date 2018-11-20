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
specific <- read_rds("upshot.rds")

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
                  tabPanel("barplot1", plotlyOutput("barPlot1")),
                  tabPanel("barplot2", plotOutput("barPlot2")))
      )))
  

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$barPlot1 <- renderPlotly({
    data %>%
      ggplot(aes_string(x = input$characteristic)) + 
      geom_bar(aes_string(fill= input$ager)) +
      xlab(input$characteristic) + ylab("Number of Interviewees") + 
      ggtitle("Age and Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
  output$barPlot2 <- renderPlotly({
      ggplot(data = specific, aes_string(x = "state_district")) +
      geom_bar(aes(fill = input$characteristic), width = .1) + 
      labs(title = "Histogram of States Voting Distribution")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


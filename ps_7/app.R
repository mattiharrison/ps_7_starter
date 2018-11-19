library(shiny)
library(tidyverse)
library(ggplot2)
library(janitor)

data <- read_rds("results.rds")
midterm <- read_rds("upshot.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Upshot Data and Midterm Results"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "characteristic",
        label = "Choose Characteristic:",
        choices = c("Age" = "ager", "Sex" = "gender", "Race/Ethnicity" = "race_eth"),
        selected = "ager"
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$barPlot <- renderPlot({
    
    data %>%
      ggplot(aes_string(x = input$characteristic)) + 
      geom_bar(aes_string(fill= input$ager)) +
      xlab(input$characteristic) + ylab("Number of Interviewees") + 
      ggtitle("Age, Sex, and Race/Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

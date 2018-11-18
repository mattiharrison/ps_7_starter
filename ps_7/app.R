library(shiny)
library(tidyverse)
library(ggplot2)
library(janitor)

data <- read_rds("results.rds")

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
        choices = c("Age" = "data2$ager", "Sex" = "data2$gender", "Race/Ethnicity" = "data2$race_eth"),
        selected = "data$ager"
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  datasetInput <- reactive({
    switch(input$data,
           "ager" = ager,
           "gender" = gender,
           "race_eth" = race_eth)
  })
  
 
  output$barPlot <- renderPlot({
    
    data %>%
      ggplot(aes_string(x = "ager")) + 
      geom_bar(aes_string(fill= input$ager)) +
      xlab("Age") + ylab("Number of Interviewees") + 
      ggtitle("Age, Sex, and Race/Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
 
  output$barPlot <- renderPlot({
    
    data %>%
      ggplot(aes_string(x = "gender")) + 
      geom_bar(aes_string(fill= input$gender)) +
      xlab("Gender") + ylab("Number of Interviewees") + 
      ggtitle("Age, Sex, and Race/Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })

  
  output$barPlot <- renderPlot({
    
    data %>%
      ggplot(aes_string(x = "race_eth")) + 
      geom_bar(aes_string(fill= input$race_eth)) +
      xlab("Race/Ethnicity") + ylab("Number of Interviewees") + 
      ggtitle("Age, Sex, and Race/Ethnicity Breakdowns of Interview") + 
      labs(subtitle = "Upshot data and Midterm Results") + 
      theme_minimal() + labs(fill = "")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

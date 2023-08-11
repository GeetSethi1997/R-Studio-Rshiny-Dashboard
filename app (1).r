library(shiny)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Add inputs here
      sliderInput("num_points", "Number of data points:", min = min(ad_count_peryear$Year), max = max(ad_count_peryear$Year), value = nrow(ad_count_peryear), step = 1),
      selectInput("line_select", "Select Brand to display:",
                  choices = c("All Brands", levels(factor(ad_count_peryear$Brand))),
                  selected = "All Brands")
    ),
    mainPanel(
      # Add outputs here
      plotOutput("my_chart")
    )
  )
)

# Server

formatNoGrouping <- function(x) {
  format(x, big.mark = "", decimal.mark = ".", scientific = FALSE)
}

server <- function(input, output) {
  URL_superbowlad <- "/Users/geet/Downloads/superbowl-ads.csv"
  df_sbads <- read.csv(file=URL_superbowlad, header= 1, sep = ",")
  ad_count_peryear <- df_sbads %>% count(year,brand)
  colnames(ad_count_peryear)[1] <- "Year"
  colnames(ad_count_peryear)[2] <- "Brand"
  colnames(ad_count_peryear)[3] <- "Count"
  filtered_data <- reactive({
    data <- ad_count_peryear[1:input$num_points, ]
    if (input$line_select != "All Brands") {
      data <- data[data$brand == input$line_select, ]
    }
    data
  })
  

  output$my_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = Count, colour = brand)) +
      labs(x = "X Label", y = "Y Label", title = "My Chart Title")  # Customize line colors as needed
  })
}


shinyApp(ui, server)


# Please check the final output at https://t7qixt-oscar-cuadros.shinyapps.io/homework-2-cuadrosangeles/

library(tidyverse)
library(sf)
library(shiny)
library(spData)
library(scales)
library(cowplot)
library(plotly)
library(shinythemes)
library(rsconnect)


path <- "https://raw.githubusercontent.com/cuadrosangeles/data/main"
covid <- read_csv(file.path(path, "/Covid.csv"))
dataset_chicago <- read_csv(file.path(path, "/dataset_chicago.csv"))
dataset_chicago$ZIP <- as.character(dataset_chicago$ZIP)


# Creating a Shiny dashboard

ui <- fluidPage(theme = shinytheme("flatly"),
    fluidRow(
      column(width = 8,
             offset = 2,
             align = "center",
             tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9b/Flag_of_Chicago%2C_Illinois.svg/1200px-Flag_of_Chicago%2C_Illinois.svg.png",
                      height = 100,
                      width = 190)
      )
    ), 
      fluidRow(
        column(width = 8,
               offset = 2,
           align = "center", 
           tags$h1("Covid-19 pandemic in Chicago"),
           tags$hr()
    )
),
  
  fluidRow(
    column(width = 8,
           offset = 2, 
           align = "center",
           tags$h4("Welcome! This Shiny dashboard shows basic information regarding the Covid-19 pandemic impact in Chicago between 2020 and 2022. We will analyze deaths related to Covid-19 viral infections and the Covid-19 Community Vulnerability Index (CCVI) by ZIP codes. Start by selecting a CCVI category:")
    ),
    
  fluidRow(
        column(width = 8,
               offset = 2,
           align = "center",
           selectInput(inputId = "Category",
                       label = "Choose a CCVI category",
                       choices = dataset_chicago$Category)
    )
  ),
  
  fluidRow(
        column(width = 3,
               offset = 2,
           align = "center",
           tableOutput("table")
    ),
        column(width = 5,
           align = "center",
           plotlyOutput("graph")  
           )
  ),
  
  fluidRow(
    column(width = 8,
           offset = 2, 
           align = "center",
           tags$h4("Let's analyze Covid-19 deaths on a map. You can select a map with or without streets, depending on your preferences. Please select the map you need:"),
           tags$hr()
    )
    ),
  
  fluidRow(
    column(width = 2,
           offset = 2, 
           align = "center", 
           selectInput(inputId = "type",
                       label = "Select a map",
                       choices = c("Map without streets", "Map with streets"))
    ),
    column(width = 7,
           align = "center",
           imageOutput(outputId = "type")
    )
  )
)
)



server <- function(input, output) {
  
  df <- dataset_chicago
    data <- reactive({
    d <- filter(df, Category == input$Category)
    return(d)
  })
  
  output$graph <- renderPlotly({
    plt <- ggplot(data = data(), aes(x = Cases , y = Score, label = ZIP)) + 
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle("CCVI Score and Covid-19 deaths") +
      labs(x = "Covid-19 cases", 
           y = "CCVI Score",
           caption = "Source: Chicago Covid-19 Community Vulnerability Index") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = comma)
    ggplotly(plt)
  })
  
  output$table <- renderTable({data()})
  
  
  output$type <- renderImage({ 
    if (input$type == "Map without streets") {
      src <- "covid map.png"
    } else if (input$type == "Map with streets") {
      src <- "covid map streets.png"
    } 
    list(src = src,
         width = "100%")
  }, deleteFile = FALSE)
}


shinyApp(ui = ui, server = server)

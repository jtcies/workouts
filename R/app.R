library(shiny)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats) 
library(stringr)

source('../common/jtc_theme.R')

# ----- functions and ref data

NA_to_none <- function(x) {
  # changes NAs to 'none', use with mutate_at
  x[is.na(x)] <- 'none'
  x
}

calc_pace <- function(dist, time) {
  # takes the time of the workout and converts it to a duration object
  # for calcuating pace
  split <- str_split(time, ":")
  split <- lapply(split, as.numeric)
  seconds <- lapply(split, function(x) {
    x[1] * 3600 + x[2] * 60 + x[3]
  })
  dist / unlist(seconds) * 3600
}
  

# ----- read data -----

cardio <- read_csv('../data/bike_run_log.csv')


# ---- tidy and transform ---
# create a pace variable

cardio$pace <- calc_pace(cardio$distance, cardio$time)

# ----- shiny -----

ui <- fluidPage(
  
  # Application title
  titlePanel("JTC workout data"),
  
  # Sidebar with input for chart types 
  sidebarLayout(
    sidebarPanel(
      selectInput("option",
                  "page",
                  c('overview',
                    'pace')
                  )
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

# return the plot that was chosen
server <- function(input, output) {
  
  output$plot <- renderPlot({
    if(input$option == 'overview') {
      
      plot <- ggplot(cardio, aes(x = date)) +
        geom_histogram(binwidth = 7) +
        labs(x = "week") + 
        jtc
      
    }
    
    if(input$option == 'pace') {
      plot <- ggplot(cardio, aes(x = date, y = pace, colour = type)) + 
        geom_line() +
        geom_point(size = 2) + 
        jtc
    }
    
    plot
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


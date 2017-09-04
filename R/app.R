library(shiny)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats) 

source('../common/jtc_theme.R')

# ----- functions and ref data
# all dates from 2017-06-01 for charts

dates <- data.frame(date = seq(as.Date('2017-06-01'), today(), by = '1 day'))

NA_to_none <- function(x) {
  # changes NAs to 'none', use with mutate_at
  x[is.na(x)] <- 'none'
  x
}

# limitsa for time of day charts
lims <- as.POSIXct(strptime(
  c(paste0(today(), " 00:01"), paste0(today(), " 23:59")), 
  format = "%Y-%m-%d %H:%M"))

# ----- read data -----

cardio <- read_csv('../data/bike_run_log.csv')
strength <- read_csv('../data/strength_log.csv')


# ----- shiny -----

ui <- fluidPage(
   
   # Application title
   titlePanel("JTC workout data"),
   
   # Sidebar with input for chart types 
   sidebarLayout(
      sidebarPanel(
         selectInput('option', NULL,
                     c('date and time' = 'date.time',
                       'test' = 'test'))
      ),
      mainPanel(
         plotOutput(outputId = "plot")
      )
   )
)

# return the plot that was chosen
server <- function(input, output) {
  
   output$plot <- renderPlot( {
     if(input$option == 'date.time') {
       
       plot <- strength %>% 
         mutate(type = 'strength') %>% 
         bind_rows(cardio) %>% 
         distinct(date, start, type) %>% 
         mutate(type = fct_relevel(type, 'bike', 'run', 'strength'),
                start = as.POSIXct(strptime(
                  paste0(today(), ' ', start), 
                  format = "%Y-%m-%d %H:%M"))) %>% 
           ggplot(aes(date, start, colour = type)) +
          geom_point(size = 2) +
          scale_y_datetime(limits = lims, date_labels = '%H:%M') +
          labs(y = 'start time') +
          jtc
     }
     
     if(input$option == 'test') {
         plot <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
       }
       
       plot
       
     })
}
   

# Run the application 
shinyApp(ui = ui, server = server)


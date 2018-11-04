#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("U.S. Hotels and Motels Expenditures: Final App Draft"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    expenditures <- read_excel("../data/nominal_expenditures.xlsx", skip = 4) %>%
      clean_names() %>%
      head(21)
    
    expenditures_no_taxes_tips <- read_excel("../data/nominal_expenditures_no_taxes_tips.xlsx", skip = 4) %>%
      clean_names() %>%
      head(21)
    
    expenditures_alcohol <- expenditures %>%
      select(year, drinking_places, hotels_and_motels, liquor_stores, eating_and_drinking_places)
    
    expenditures_nt_alcohol <- expenditures_no_taxes_tips %>%
      select(year, drinking_places, hotels_and_motels, liquor_stores, eating_and_drinking_places)
    
    expenditures_nt_alcohol %>%
      ggplot(aes(x = year, y = hotels_and_motels)) + geom_point() + xlab("Year") + ylab("Hotels and Motels Expenditures (in Millions)") + ggtitle("Expenditures on Hotels and Motels in U.S.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


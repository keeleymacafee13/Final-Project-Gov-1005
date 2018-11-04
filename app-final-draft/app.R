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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Food and Alcohol Expenditure Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        selectInput('Group_select',
                    label = h3('Location'),
                    choices = c('drinking_places', 'hotels_and_motels'),
                    selected = 'drinking_places')
     ),
      # Show a plot of the generated distribution
   
      mainPanel(plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     
     expenditures <- read_excel("./data/nominal_expenditures.xlsx", skip = 4) %>%
       clean_names() %>%
       head(21)
   
   expenditures_alcohol <- expenditures %>%
     select(year, drinking_places, liquor_stores)
   
   expenditures_no_taxes_tips <- read_excel("./data/nominal_expenditures_no_taxes_tips.xlsx", skip = 4) %>%
     clean_names() %>%
     head(21)
   
   expenditures_nt_alcohol <- expenditures_no_taxes_tips %>%
     select(year, drinking_places, hotels_and_motels)
     
 
    ggplot(expenditures_nt_alcohol, aes(x = year)) + 
        geom_bar()
      
   })
   
}

#Run the application 
shinyApp(ui = ui, server = server)

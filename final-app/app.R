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

exp <- read_rds("expenditures.rds")
exp_no_taxes_tips <- read_rds("expenditures_no_taxes_tips.rds")
tot_indiv <- read_rds("total_indiv.rds")
food <- read_rds("food.rds")
indiv_tips_taxes <- read_rds("indiv_tips_taxes.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Choices", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "y", 
                             label = "Select Location:", 
                             choices = c("Grocery Stores", "Convenience Stores", "Mass Merchandisers", 
                                         "Mail Order and Home Delivery","Home Production and Donations",
                                         "Full Service Restaurants",
                                         "Hotels and Motels", 
                                         "Schools and Colleges"))
  
    ) ,
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About", htmlOutput("about")),
                  tabPanel("Plot 1", plotOutput("Plot1")),
                  tabPanel("Plot 2", plotOutput("Plot2")))
    )
  )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot1 <- renderPlot({
    ggplot(food, aes(x = year,  y = switch(input$y,
                                                   "Grocery Stores" = grocery_stores, 
                                                   "Convenience Stores" = convenience_stores,
                                                   "Mass Merchandisers" = mass_merchandisers, 
                                                   "Mail Order and Home Delivery" = mail_order_and_home_delivery,
                                                   "Home Production and Donations" = home_production_and_donations,
                                                   "Full Service Restaurants" = full_service_restaurants,
                                                   "Hotels and Motels" = hotels_and_motels,
                                                   "Schools and Colleges" = schools_and_colleges))) + geom_point() +
      geom_smooth() +
      ggtitle("APP TITLE") +
      xlab("Year") +
      ylab("Places")
  })

  output$Plot2 <- renderPlot({
    
    ggplot(indiv_tips_taxes, aes(x = year, y = switch(input$y,
                                                      "Grocery Stores" = indiv_t_grocery, 
                                                      "Convenience Stores" = indiv_t_conv,
                                                      "Mass Merchandisers" = indiv_t_mass, 
                                                      "Mail Order and Home Delivery" = indiv_t_mail,
                                                      "Home Production and Donations" = indiv_t_home,
                                                      "Full Service Restaurants" = indiv_t_rest,
                                                      "Hotels and Motels" = indiv_t_hotel,
                                                      "Schools and Colleges" = indiv_t_school))) + geom_point() +
      geom_smooth() +
      ggtitle("APP TITLE") +
      xlab("Year") +
      ylab("Places")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("TITLE"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "y", 
                    label = "Select Location:", 
                    choices = c("Grocery Stores", "Convenience Stores", "Other Food Stores", 
                                "Warehouse Clubs and Supercenters", "Mass Merchandisers", 
                                "Other Stores and Foodservice", "Mail Order and Home Delivery", 
                                "Direct Selling", "Home Production and Donations", "Total Food at Home",
                                "Full Service Restaurants", "Limited Service Restaurants", "Drinking Places",
                                "Hotels and Motels: Food", "Retail Stores", "Recreational Places",
                                "Schools and Colleges", "Other Food Away From Home", "Food Furnished and Donated",
                                "Total Food Away from Home", "Liquor Stores", "Food Stores", "Other Alcohol at Home",
                                "Total Alcohol at Home", "Eating and Drinking Places", "Hotels and Motels: Alcohol",
                                "Other Alcohol Away from Home", "Total Alcohol Away from Home"))
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
      ggplot(expenditures, aes(x = year,  y = switch(input$y,
                                                     "Grocery Stores" = grocery_stores, 
                                                     "Convenience Stores" = convenience_stores, 
                                                     "Other Food Stores" = other_food_stores, 
                                                     "Warehouse Clubs and Supercenters" = warehouse_clubs_and_supercenters, 
                                                     "Mass Merchandisers" = mass_merchandisers, 
                                                     "Other Stores and Foodservice" = other_stores_and_foodservice, 
                                                     "Mail Order and Home Delivery" = mail_order_and_home_delivery, 
                                                     "Direct Selling" = direct_selling_by_farmers_manufacturers_and_wholesalers, 
                                                     "Home Production and Donations" = home_production_and_donations, 
                                                     "Total Food at Home" = total_fah,
                                                     "Full Service Restaurants" = full_service_restaurants, 
                                                     "Limited Service Restaurants" = limited_service_restaurants, 
                                                     "Drinking Places" = drinking_places,
                                                     "Hotels and Motels: Food" = hotels_and_motels, 
                                                     "Retail Stores" = retail_stores_and_vending, 
                                                     "Recreational Places" = recreational_places,
                                                     "Schools and Colleges" = schools_and_colleges, 
                                                     "Other Food Away From Home" = other_fafh_sales_nec, 
                                                     "Food Furnished and Donated" = food_furnished_and_donated,
                                                     "Total Food Away from Home" = total_fafh, 
                                                     "Liquor Stores" = liquor_stores, 
                                                     "Food Stores" = food_stores, 
                                                     "Other Alcohol at Home" = other_aah_sales_nec,
                                                     "Total Alcohol at Home" = total_aah, 
                                                     "Eating and Drinking Places" = eating_and_drinking_places, 
                                                     "Hotels and Motels: Alcohol" = hotels_and_motels_1,
                                                     "Other Alcohol Away from Home" = other_aafh_nec, 
                                                     "Total Alcohol Away from Home" = total_aafh))) + geom_point() +
                                                                                                      geom_smooth() +
                                                                                                      ggtitle("APP TITLE") +
                                                                                                     xlab("Year") +
                                                                                                     ylab("Places")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


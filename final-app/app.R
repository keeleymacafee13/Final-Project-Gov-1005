

library(shiny)
library(tidyverse)

exp <- read_rds("expenditures.rds")
exp_no_taxes_tips <- read_rds("expenditures_no_taxes_tips.rds")
tot_indiv <- read_rds("total_indiv.rds")
food <- read_rds("food.rds")
indiv_tips_taxes <- read_rds("indiv_tips_taxes.rds")
indiv_food1 <- read_rds("indiv_food.rds")


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
                                         "Schools and Colleges"),
                             selected = "Mass Merchandisers"),
                 selectInput(inputId = "z",
                              label = "Select Location:", 
                              choices = c("Grocery Stores", "Convenience Stores", "Mass Merchandisers", 
                                          "Mail Order and Home Delivery","Home Production and Donations",
                                          "Full Service Restaurants",
                                          "Hotels and Motels", 
                                          "Schools and Colleges"),
                             selected = "Mail Order and Home Delivery")
  
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
    
    
      ggplot(indiv_food1, aes(x = year,  y = switch(input$y,
                                                   "Grocery Stores" = indiv_grocery, 
                                                   "Convenience Stores" = indiv_conv,
                                                   "Mass Merchandisers" = indiv_mass, 
                                                   "Mail Order and Home Delivery" = indiv_mail,
                                                   "Home Production and Donations" = indiv_home,
                                                   "Full Service Restaurants" = indiv_rest,
                                                   "Hotels and Motels" = indiv_hotel,
                                                   "Schools and Colleges" = indiv_school), color = input$y)) +
        geom_point() +
        geom_smooth() +
        geom_smooth(aes(y = switch(input$z,
                                   "Grocery Stores" = indiv_grocery, 
                                   "Convenience Stores" = indiv_conv,
                                   "Mass Merchandisers" = indiv_mass, 
                                   "Mail Order and Home Delivery" = indiv_mail,
                                   "Home Production and Donations" = indiv_home,
                                   "Full Service Restaurants" = indiv_rest,
                                   "Hotels and Motels" = indiv_hotel,
                                   "Schools and Colleges" = indiv_school), color = input$z)) +
        ggtitle("Average Expenditure on Food Per Individual in the U.S.") +
        xlab("Year") +
        ylab("Expenditures (in Dollars)")})
    
   
    

    
  output$Plot2 <- renderPlot({ 
    ggplot(indiv_tips_taxes, aes(x = year, y = switch(input$y,
                                                      "Grocery Stores" = indiv_t_grocery, 
                                                      "Convenience Stores" = indiv_t_conv,
                                                      "Mass Merchandisers" = indiv_t_mass, 
                                                      "Mail Order and Home Delivery" = indiv_t_mail,
                                                      "Home Production and Donations" = indiv_t_home,
                                                      "Full Service Restaurants" = indiv_t_rest,
                                                      "Hotels and Motels" = indiv_t_hotel,
                                                      "Schools and Colleges" = indiv_t_school), color = input$y)) + geom_point() +
      geom_smooth() +
      geom_smooth(aes(y = switch(input$z,
                                 "Grocery Stores" = indiv_t_grocery, 
                                 "Convenience Stores" = indiv_t_conv,
                                 "Mass Merchandisers" = indiv_t_mass, 
                                 "Mail Order and Home Delivery" = indiv_t_mail,
                                 "Home Production and Donations" = indiv_t_home,
                                 "Full Service Restaurants" = indiv_t_rest,
                                 "Hotels and Motels" = indiv_t_hotel,
                                 "Schools and Colleges" = indiv_t_school), color = input$z)) +
      ggtitle("Tax and Tips Per Individual in the U.S.") +
      xlab("Year") +
      ylab("Tip and Tax Amount (in Dollars)")
      
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


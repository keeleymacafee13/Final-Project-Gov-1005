

library(shiny)
library(tidyverse)

indiv_food1 <- read_rds("indiv_food.rds")
availability_types <- read_rds("availability_types.rds")


# Define UI for application 
ui <- fluidPage(
  titlePanel("Visualizing Food Expenditure and Availability in the U.S."),
  
  tabsetPanel(
    tabPanel("", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
              selectInput(inputId = "y", 
                  label = "Select Location:", 
                  
                  # I chose the most relevant retailing locations, as there were many possible selections.
                  
                  choices = c("Grocery Stores", "Convenience Stores", "Mass Merchandisers", 
                              "Mail Order and Home Delivery","Home Production",
                              "Full Service Restaurants",
                              "Hotels and Motels", 
                              "Schools and Colleges"),
                  selected = "Grocery Stores"),
              
              # I could not figure out how to change the sidebarPanel per tab, so I added some text to 
              # clarify that the drop-down menu is specifically for the food expenditure plot. 
              
              tags$h6(helpText("Select retail location 
                               to visualize food expenditure per capita 
                               over time."))),
      
      
      
              
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("About", htmlOutput("about")),
                    tabPanel("Food Availability", plotOutput("Plot1")),
                    tabPanel("Food Expenditures", plotOutput("Plot2")))
)))))





# Define server logic 
server <- function(input, output) {
  
  # I wanted to provide further brief clarification that the first plot on food availability
  # is not reactive, but the second plot on food expenditure is. 
  
  output$about <- renderText({
    "The first tab statically displays food availability in pounds per capita over time. 
    I divided the variables into perishable and imperishable to aid visualization.
    
    In the second tab, use the drop-down menu to select a food retailing location and visualize
    American expenditure patterns at the selected location."
  })
  
  
  output$Plot1 <- renderPlot({
    
    ggplot(availability_types, aes(x = year, y = total, color = category)) + geom_point() + geom_smooth() +
      ggtitle("Yearly Food Availability in the United States: Pounds Per Capita")
    
  })

  output$Plot2 <- renderPlot({
    
    
      ggplot(indiv_food1, aes(x = year,  y = switch(input$y,
                                                   "Grocery Stores" = indiv_grocery, 
                                                   "Convenience Stores" = indiv_conv,
                                                   "Mass Merchandisers" = indiv_mass, 
                                                   "Mail Order and Home Delivery" = indiv_mail,
                                                   "Home Production" = indiv_home,
                                                   "Full Service Restaurants" = indiv_rest,
                                                   "Hotels and Motels" = indiv_hotel,
                                                   "Schools and Colleges" = indiv_school))) +
        geom_point() +
        geom_smooth(color = "red") +
        ggtitle("Average Food Expenditure in the United States: Dollars Per Capita") +
        xlab("Year") +
        ylab("Expenditures (in Dollars)")})

  # At first, I made the expenditure plot comparitive, so the user could select two different retailing
  # locations and visualize the difference. However, I felt this was too confusing, especially with the 
  # two lines in the food availability plot. Also, grocery stores are so far above all of the other locations
  # that it was difficult to see the actual trends in a comparitive context. 
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)

indiv_food1 <- read_rds("indiv_food.rds")
availability_types <- read_rds("availability_types.rds")



# I had originally used the basic shiny template, but with some feedback and help from Ms. Paterson
# I used the sandstone theme to refine my app. She also helped me with organizing my tabs 
# so that not everything was wrapped by fluidPage, which was giving me issues before. 

# Define UI for application
ui <- navbarPage(theme = shinytheme("sandstone"), 
                 "Visualizing Food Expenditure and Availability in the United States",
                 
                 # Starting with my about panel gives the app user a little bit of context (similar to my README)
                 # and guides them on how to use the app. 
                 
                 #First tab
                 tabPanel("About",
                          fluidPage(
                            div(class = "well",
                                h3("About This App")),
                            
                            # Sizing font
                            h4("The United States is facing a food revolution. Using data from the Food 
                               Expenditure Series published by the U.S. Department of Agriculture (USDA), 
                               I examine the gap between per capita spending (in dollars) on food and per capita food availability (in pounds) 
                               over the past two decades. I use selected food groups (dairy, grains, fish, etc.) and selected retailing 
                               locations (grocery stores, convenience stores, etc.) with the hopes that this visualization can be applied 
                               to what is occurring in the food industry more generally. We are moving towards a more processed, more automated food industry."),
                            
                            #Adding spaces in between paragraphs
                            hr(),
                            
                            h4("The first tab statically displays food availability in pounds per capita over time. 
                               I divided the variables into perishable and imperishable to aid visualization. However, it is important
                               to note that the imperishable category includes food only food produced on farms that was canned or frozen.
                               It does not account for highly processed foods such as canned soups and frozen dinners.")
                            ,
                            hr(),
                            
                            h4("In the second tab, use the drop-down menu to select a U.S. food retailing location to visualize
                               expenditure patterns at the selected location."),
                            
                            hr(),
                            
                            #Include data sources at the bottom for reference.
                            h5("Data Sources:"),
                            
                            hr(),
                            
                            h5("https://www.ers.usda.gov/data-products/food-expenditure-series/"),
                            
                            hr(),
                            
                            h5("https://www.ers.usda.gov/data-products/food-availability-per-capita-data-system/"),
                            
                            hr(),
                            
                            h5("https://fred.stlouisfed.org/series/POPTOTUSA647NWDB")
                            )),
                 
                 #Second tab: availability
                 tabPanel("Food Availability",
                          
                          fluidPage(
                            
                            div(class="well",
                                h2("Food Availability Per Capita"),
                                h5("Food availability, overall, has been decreasing over time.")),
                            
                            plotOutput("Plot1")
                          )
                 ),
                 
                 
                 #Third tab: Food expenditure
                 tabPanel("Food Expenditure",
                          fluidPage(
                            h2("Food Expenditure Per Capita"),
                            
                            #Add a drop down menu so users can select inputs.
                            #Easier to just put it within the tab instead of using 
                            #something like conditionalPanel. 
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "y", 
                                            label = "Select Location:", 
                                            
                                            # I chose the most relevant/interesting retailing locations, as there were many possible selections.
                                            
                                            choices = c("Grocery Stores", "Convenience Stores", "Mass Merchandisers", 
                                                        "Mail Order and Home Delivery","Home Production",
                                                        "Full Service Restaurants",
                                                        "Hotels and Motels", 
                                                        "Schools and Colleges"),
                                            selected = "Grocery Stores"),
                                
                                #Add a description of choices under the dropdown menu. 
                                div(class="well",
                                    h5("Select retail location 
                                       to visualize food expenditure per capita 
                                       over time."))),
                              mainPanel(plotOutput("Plot2")
                              )
                                    )))
                          )




server <- function(input, output, session) {
  
  #Smooth seemed to be the best way to visualize for both plots 
  
  output$Plot1 <- renderPlot({
    ggplot(availability_types, aes(x = year, y = total, color = category)) + 
      geom_point() + geom_smooth() +
      xlab("Year") + 
      ylab("Total Pounds per Capita")
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
      xlab("Year") +
      ylab("Expenditure per Capita (in Dollars)")
    
    # At first, I made the expenditure plot comparitive, so the user could select two different retailing
    # locations and visualize the difference. However, I felt this was too confusing, especially with the 
    # two lines in the food availability plot. Also, grocery stores are so far above all of the other locations
    # that it was difficult to see the actual trends in a comparitive context. 
    
  })
  
}

shinyApp(ui, server)



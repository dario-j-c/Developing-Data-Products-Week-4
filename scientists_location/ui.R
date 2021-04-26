library(shiny)
library(shinythemes)

library(dplyr)
library(tidyr)
library(leaflet)


# Define UI
shinyUI(
  fluidPage(theme = shinytheme("lumen"),
            titlePanel("Scientists & Their Birthdays"),
            sidebarLayout(
              sidebarPanel(
                h6("Please wait for data to load from Wikipedia (approx. 20 seconds)."),
                h6("The popups on the map refer to the listed birth place of the person in question."),
                h6("Please note, *country* refers to the citizenship of the person. It may not match their birth place."),
                h6("You can click on the popups to see the name of the person it refers to."),
                h4("Options to Filter the Table"),
                h5("Select Filter by Country of Origin and/or Date Range to filter the table."),
                h6("Use the filters to dynamically change the table and map which you can explore afterwards."),
                
                # Select whether to use the Country of Origin to filter
                checkboxInput(inputId = "use_country",
                              label = strong("Filter by Country of Origin"),
                              value = FALSE),
                
                # Display only if use Country to filter is checked
                conditionalPanel(condition = "input.use_country == true",
                                 
                                 # Select the Country of interest
                                 selectInput(inputId = "country",
                                             label = strong("Country of Origin"),
                                             choices = "United States of America", #choices = stringr::str_sort(
                                             #  unique(table_clean$country)
                                             #  ),
                                             selected = NULL
                                 )
                ),
                
                # Select whether to use the Date Range to filter
                checkboxInput(inputId = "use_date",
                              label = strong("Filter by Date Range"),
                              value = FALSE),
                
                conditionalPanel(condition = "input.use_date == true",
                                 
                                 # Select Date Range of interest
                                 dateRangeInput(inputId = "date",
                                                strong("Date Range for Birthday"),
                                                start = as.Date("100-01-01"),
                                                end = as.Date("2021-01-01")
                                 )
                )
              ),
              
              # Output:
              mainPanel(
                h5("The below table's information is directly taken from Wikipedia."),
                tags$a(href = "https://w.wiki/3EfW", "Source: Automated Wikipedia List", target = "_blank"),
                leafletOutput("map"),
                dataTableOutput(outputId = "table")
              )
            )
  )
)

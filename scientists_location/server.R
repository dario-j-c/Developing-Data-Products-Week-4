library(shiny)

library(WikidataQueryServiceR)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)

# Query Wikipedia for scientists
scraped_raw <- query_wikidata('SELECT ?scientist ?scientistLabel ?birthdate ?countryLabel ?coord WHERE {
  ?scientist ?x1 wd:Q901;
  wdt:P569 ?birthdate;
  wdt:P19 ?birthplace;
             wdt:P27 ?country.
 
  ?birthplace wdt:P625 ?coord
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
ORDER BY DESC(?birthdate)')

# Query Wikipedia for fictional scientists
fictional_scrape_raw <- query_wikidata('SELECT ?scientist ?scientistLabel ?birthdate ?countryLabel ?narrative ?coord WHERE {
  ?scientist ?x1 wd:Q901;
  wdt:P569 ?birthdate;
  wdt:P19 ?birthplace;
             wdt:P1080 ?narrative;
             wdt:P27 ?country.
 
  ?birthplace wdt:P625 ?coord
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
ORDER BY DESC(?birthdate)')


# Clean the query results
table_clean <- scraped_raw %>%
  mutate( coord = str_replace_all(string = coord,
                                  pattern = "(Point|\\(|\\))",
                                  replacement = ""),
          birthdate = as.Date(birthdate)
  ) %>%
  separate( col = coord,
            into = c("long", "lat"),
            sep = " ",
            remove = TRUE) %>%
  mutate( long = parse_number(long),
          lat = parse_number(lat)) %>%
  rename(country = countryLabel,
         link = scientist,
         scientist = scientistLabel) %>%
  filter(link != fictional_scrape_raw$scientist &
           str_count(scientist, pattern = " ") >= 1
  ) %>%
  select(scientist,
         birthdate,
         country,
         long,
         lat,
         link)



# Define server logic required to select famous people
shinyServer(function(input, output) {
  
  
  # Verify Dates (currently unused)
  selected_dates <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
  })
  
  # Update input value
  observeEvent(input$date, {
    start_in <- input$date[1]
    end_in <- input$date[2]
    updateDateRangeInput(inputId = "date",
                         start = start_in,
                         end = end_in
    )
  })
  
  # Update list of Countries
  observeEvent(input$country, {
    x <- input$country
    updateSelectInput(inputId = "country",
                      choices = stringr::str_sort(unique(table_clean$country)),
                      selected = x
    )
  })
  
  
  
  # Render Table
  output$table <- renderDataTable({
    if(input$use_country & input$use_date) {
      table_clean %>%
        filter(
          country == input$country,
          birthdate >= input$date[1] & birthdate <= input$date[2]
        )
    } else if (input$use_country) {
      table_clean %>%
        filter(
          country == input$country
        )
    } else if (input$use_date) {
      table_clean %>%
        filter(
          birthdate >= input$date[1] & birthdate <= input$date[2]
        )
    } else {
      table_clean
    }
    
  })
  
  # Render Map
  output$map <- renderLeaflet({
    if(input$use_country & input$use_date) {
      map <- table_clean %>%
        filter(
          country == input$country,
          birthdate >= input$date[1] & birthdate <= input$date[2]
        ) %>% select( long, lat, scientist) %>%
        rename(lng = long,
               popup = scientist)
      map <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat = map$lat,
                   lng = map$lng,
                   popup = map$popup,
                   clusterOptions = markerClusterOptions())
      map
    } else if (input$use_country) {
      map <- table_clean %>%
        filter(
          country == input$country
        ) %>% select( long, lat, scientist) %>%
        rename(lng = long,
               popup = scientist)
      map<- leaflet() %>%
        addTiles() %>%
        addMarkers(lat = map$lat,
                   lng = map$lng,
                   popup = map$popup,
                   clusterOptions = markerClusterOptions())
      map
    } else if (input$use_date) {
      map <- table_clean %>%
        filter(
          birthdate >= input$date[1] & birthdate <= input$date[2]
        ) %>% select( long, lat, scientist) %>%
        rename(lng = long,
               popup = scientist)
      map <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat = map$lat,
                   lng = map$lng,
                   popup = map$popup,
                   clusterOptions = markerClusterOptions())
      map
    } else {
      map <- table_clean %>% select( long, lat, scientist) %>%
        rename(lng = long,
               popup = scientist)
      map <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat = map$lat,
                   lng = map$lng,
                   popup = map$popup,
                   clusterOptions = markerClusterOptions())
      map
    }
    
  })
  
  
  
}
)

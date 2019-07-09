library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

hoteldata <- AllHotels[order(AllHotels$Hotel_Name),]

function(input, output, session) {
  
  ## Hotel Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 6.67, lat = 48.02, zoom = 4)
  })
  
  filteredData <- reactive({
    AllHotelsGrouped[AllHotelsGrouped$Score >= input$range[1] & AllHotelsGrouped$Score <= input$range[2],]
  })
  
  # A reactive expression that returns the set of hotels that are
  # in bounds right now
  hotelsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(hoteldata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(hoteldata,
           lat >= latRng[1] & lat <= latRng[2] &
             lng >= lngRng[1] & lng <= lngRng[2])
  })
  
  # Score Breaks
  scoreBreaks <- hist(plot = F, AllHotels$Reviewer_Score, breaks = 20)$breaks
  
  output$histScore <- renderPlot({
    # If no hotels are in view, don't plot
    if (nrow(hotelsInBounds()) == 0)
      return(NULL)
    
    hist(hotelsInBounds()$Reviewer_Score,
         breaks = scoreBreaks,
         main = "Hotel score (scores in area)",
         xlab = "Score",
         xlim = range(AllHotels$Reviewer_Score),
         col = '#00DD00',
         border = 'white')
  })
  

  
  observe({
    leafletProxy("map", data = filteredData())%>%
      clearMarkers() %>%
      addMarkers(lat = ~Lat, lng = ~Long, layerId = ~Adres)
  })
  
  # Function to show a pop-up for a specific hotel at a given lat/lng
  showHotelPopup <- function(adres, lat, lng) {
    selectedHotel <- AllHotelsGrouped[AllHotelsGrouped$Adres == adres,]
    content <- as.character(tagList(
      tags$h4("Score:", as.numeric(selectedHotel$Score)),
      tags$strong(HTML(sprintf("Name: %s",
                               selectedHotel$Hotel
      ))), tags$br(),
      sprintf("Adres: %s", selectedHotel$Adres), tags$br(),
      sprintf("Reviews: %s", selectedHotel$Reviews)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = adres)
  }
  
  # Listener for clicks to show pop-ups if the user clicks on a marker
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showHotelPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  # Observer to see if the user clicked to see a specific hotel on the map.
  # Places a popup over the marker and fits the map around the location of the hotel.
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.05
      adres <- input$goto$adres
      lat <- input$goto$lat
      lng <- input$goto$lng
      showHotelPopup(adres, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  # Datatable of the entire collection
  output$hotelTable <- DT::renderDataTable({
    df <- cleanTable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$hotels) | Hotel %in% input$hotels,
        Reviewer_Score >= input$minReviewScore,
        Reviewer_Score <= input$maxReviewScore,
        as.Date(Review_Date, format = "%m/%d/%Y") >= input$dateRange[1],
        as.Date(Review_Date, format = "%m/%d/%Y") <= input$dateRange[2],
        is.null(input$nationality) | Reviewer_Nationality %in% input$nationality,
        is.null(input$positive) | Positive %in% input$positive
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, 
                            '" data-adres="', Adres, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
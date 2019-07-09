library(leaflet)

navbarPage("Hotels", id="nav",
           tabPanel("Hotels map",
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gotomap.js")
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Hotel explorer"),
                                      sliderInput("range", "Scores", min(AllHotelsGrouped$Score), max(AllHotelsGrouped$Score),
                                                  value = range(AllHotelsGrouped$Score), step = 0.1),
                                      plotOutput("histScore", height = 200)
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                          selectInput("hotels", "Hotels", c("All hotels"="", as.character(unique(unlist(AllHotelsGrouped$Hotel)))), multiple=TRUE)
                        ),
                        column(3,
                               dateRangeInput("dateRange", "Reviews between: ", 
                                              start = "2015-8-01",
                                              end = "2017-12-31",
                                              min = "2015-8-01",
                                              max = "2017-12-31",
                                              format = "yyyy-mm-dd",
                                              separator = " - ")
                        ),
                        column(3, selectInput("nationality", "Reviewer Nationality", c("All nationalities"="", as.character(unique(unlist(AllHotels$Reviewer_Nationality)))), multiple = TRUE)
                        )
                      ),
                      fluidRow(
                        column(1, numericInput("minScore", "Min hotel score", min=1, max=10, value=1, step = 0.1)
                        ),
                        column(1, numericInput("maxScore", "Max hotel score", min=1, max=10, value=10, step = 0.1)
                        ),
                        column(1, selectInput("positive", "Show reviews", c("All reviews"="", "Positive"="1", "Negative"="0"), multiple = TRUE)
                        ),
                        column(2, numericInput("minReviewScore", "Min review score", min=1, max=10, value=1, step = 0.1)
                        ),
                        column(2, numericInput("maxReviewScore", "Max review score", min=1, max=10, value=10, step = 0.1)
                        )
                      ),
                      hr(),
                      DT::dataTableOutput("hotelTable")
                    
                  ),
           
           conditionalPanel("false", icon("crosshair"))
)
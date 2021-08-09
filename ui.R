library(leaflet)

# Choices for drop-downs
vars <- c(
  "Profit Score" = "revenue",
  "Property value" = "price",
  "Property rent" = "rent",
  "Property Appreciation" = "appreciation",
  "None" = "none"
)


navbarPage("Property Investment App", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      h2("Real Estate Explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars),
                                      
                                      plotOutput("histRevenue", height = 250),
                                      plotOutput("scatterAppreciationYear", height = 350)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(2,
                             numericInput("minPrice", "Min price", min=0, max=5000000, value=1)
                      ),
                      column(2,
                             numericInput("maxPrice", "Max price", min=0, max=5000000, value=5000000)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
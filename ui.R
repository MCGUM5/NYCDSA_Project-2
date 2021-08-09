library(leaflet)

# Choices for drop-downs
vars <- c(
  "Profit Score" = "revenue",
  "Property value" = "price",
  "Property rent" = "rent",
  "Property Appreciation" = "appreciation",
)


navbarPage("Revenue Map", id="nav",
           
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
                                      width = 330, height = "auto",
                                      
                                      h2("ZIP explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "price"),
                                      conditionalPanel("input.color == 'price' || input.size == 'price'",
                                                       # Only prompt for threshold when coloring or sizing by price
                                                       numericInput("threshold", "Price threshold (USD)", 3000000)
                                      ),
                                      
                                      plotOutput("histRevenue", height = 200),
                                      plotOutput("scatterAppreciationYear", height = 250)
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
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
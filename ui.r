ui <- function(country_names,
               cell_size,
               lon_poi,
               lat_poi,
               lat_test,
               lon_test,
               radius,
               n,
               algorithms,
               k,
               m,
               num_rnd_points,
               randomGenerationModes,
               degRadius) {
  ui <- fluidPage(sidebarLayout(
    sidebarPanel(
      style = "overflow-y:scroll;
                max-height: 650px;
                position:relative;
                margin: 8px;",
      
      actionButton("saveParameters", "Save parameters"),
      actionButton("loadParameters", "Load paremeters"),
      
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Area of interest",
          div(
            h5("Select country and define area of interest."),
            
            selectInput(
              "country",
              h6("Choose country to display"),
              choices = country_names,
              selected = "Country"
            ),
            
            numericInput("cell_size",
                         h6("Enter size of cells (in degrees)"),
                         value = cell_size),
            
            numericInput("lon_poi",
                         h6("Enter longitude of POI"),
                         value = lon_poi),
            
            numericInput("lat_poi",
                         h6("Enter latitude of POI"),
                         value = lat_poi),
            
            numericInput("radius",
                         h6("Enter radius of AOI (in meters)"),
                         value = radius),
            
            numericInput("n",
                         h6("Enter number of areas"),
                         value = n)
          )
        ),
        tabPanel("SBF",
                 div(
                   h5("Define SBF options."),
                   
                   selectInput(
                     "algorithm",
                     h6("Choose hash algorithm"),
                     choices = algorithms,
                     selected = "Algorithm"
                   ),
                   
                   numericInput("k",
                                h6("Enter number of hash functions"),
                                value = k),
                   
                   numericInput("m",
                                h6("Enter size of bloom filter vector"),
                                value = m)
                 )),
        tabPanel("Test point",
                 div(
                   h5("Select test point."),
                   
                   numericInput("lon_test",
                                h6("Enter longitude of test point"),
                                value = lon_test),
                   
                   numericInput("lat_test",
                                h6("Enter latitude of test point"),
                                value = lat_test)
                 )),
        tabPanel("CSV multiple test points",
                 div(
                   h5("Select file with multiple test points."),
                   
                   fileInput(
                     inputId = "csvFiles",
                     h6("Drag and drop here"),
                     multiple = TRUE,
                     buttonLabel = "Browse...",
                     placeholder = "No file selected"
                   )
                 )),
        tabPanel(
          "Random test points",
          div(
            h5("Generate and test random number of points"),
            
            numericInput("num_rnd_points",
                         h6("Enter number of random points"),
                         value = num_rnd_points),
            
            selectInput(
              "randomGenerationModes",
              h6("Choose distribution for generating the spatial points"),
              choices = randomGenerationModes,
              selected = "Mode"
            ),
            
            numericInput(
              "degRadius",
              h6(
                "Enter the standard deviation in degrees (Only for Normal distrubution mode)"
              ),
              value = degRadius
            ),
            
            actionButton("saveRndPnts", "Save random points")
          )
        )
      )
    ),
    mainPanel(
      actionButton("go", "Test single point"),
      actionButton("goCSV", "Test points from csv file"),
      actionButton("goRnd", "Test random points"),
      # actionButton("goTst","Test k and m"),
      htmlOutput("SBF_label"),
      htmlOutput("error_text"),
      tableOutput('confusion_matrix'),
      tableOutput('F1'),
      tmapOutput("map")
    )
  ),)
  return(ui)
}
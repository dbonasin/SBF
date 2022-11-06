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
  # Defining layout
  ui <- fluidPage(sidebarLayout(
    sidebarPanel(
      # Defining style of the sidebar panel
      style = "overflow-y:scroll;
                max-height: 650px;
                position:relative;
                margin: 8px;",
      # Defining save parameters button
      actionButton("saveParameters", "Save parameters"),
      # Defining load parameters button
      actionButton("loadParameters", "Load paremeters"),
      
      tabsetPanel(
        type = "tabs",
        # Defining tab Area of interest
        tabPanel(
          "Area of interest",
          div(
            h5("Select country and define area of interest."),
            
            # Defining select input to choose country
            selectInput(
              "country",
              h6("Choose country to display"),
              choices = country_names,
              selected = "Country"
            ),
            
            # Defining numeric input to enter size of cells in the grid
            numericInput("cell_size",
                         h6("Enter size of cells (in degrees)"),
                         value = cell_size),
            
            # Defining numeric input to enter longitude of point of interest
            numericInput("lon_poi",
                         h6("Enter longitude of POI"),
                         value = lon_poi),
            
            # Defining numeric input to enter latitude of point of interest
            numericInput("lat_poi",
                         h6("Enter latitude of POI"),
                         value = lat_poi),
            
            # Defining numeric input to enter radius of the areas
            numericInput("radius",
                         h6("Enter radius of AOI (in meters)"),
                         value = radius),
            
            # Defining numeric input to enter number of areas
            numericInput("n",
                         h6("Enter number of areas"),
                         value = n)
          )
        ),
        # Defining tab SBF
        tabPanel("SBF",
                 div(
                   h5("Define SBF options."),
                   
                   # Defining select input to choose hash algorithm
                   selectInput(
                     "algorithm",
                     h6("Choose hash algorithm"),
                     choices = algorithms,
                     selected = "Algorithm"
                   ),
                   
                   # Defining numeric input to enter number of hash functions
                   numericInput("k",
                                h6("Enter number of hash functions"),
                                value = k),
                   
                   # Defining numeric input to enter size of the SBF vector
                   numericInput("m",
                                h6("Enter size of bloom filter vector"),
                                value = m)
                 )),
        # Defining tab Test point
        tabPanel("Test point",
                 div(
                   h5("Select test point."),
                   
                   # Defining numeric input to enter longitude
                   numericInput("lon_test",
                                h6("Enter longitude of test point"),
                                value = lon_test),
                   
                   # Defining numeric input to enter latitude
                   numericInput("lat_test",
                                h6("Enter latitude of test point"),
                                value = lat_test)
                 )),
        # Defining tab CSV multiple test points
        tabPanel("CSV multiple test points",
                 div(
                   h5("Select file with multiple test points."),
                   
                   # Defining file input to choose csv file
                   fileInput(
                     inputId = "csvFiles",
                     h6("Drag and drop here"),
                     multiple = TRUE,
                     buttonLabel = "Browse...",
                     placeholder = "No file selected"
                   )
                 )),
        # Defining tab Random test points
        tabPanel(
          "Random test points",
          div(
            
            h5("Generate and test random number of points"),
            
            # Defining numeric input for number of random points
            numericInput("num_rnd_points",
                         h6("Enter number of random points"),
                         value = num_rnd_points),
            
            # Defining select input for generating mode
            selectInput(
              "randomGenerationModes",
              h6("Choose distribution for generating the spatial points"),
              choices = randomGenerationModes,
              selected = "Mode"
            ),
            
            # Defining input field for standard deviation
            numericInput(
              "degRadius",
              h6(
                "Enter the standard deviation in degrees (Only for Normal distrubution mode)"
              ),
              value = degRadius
            ),
            
            # Defining button to save randomly generated testing points
            actionButton("saveRndPnts", "Save random points")
          )
        )
      )
    ),
    # Defining main panel
    mainPanel(
      # Defining button to start testing single point
      actionButton("go", "Test single point"),
      # Defining button to start testing points from CSV file
      actionButton("goCSV", "Test points from csv file"),
      # Defining button to start testing randomly generated points
      actionButton("goRnd", "Test random points"),
      # Defining output field
      htmlOutput("SBF_label"),
      # Defining output field
      htmlOutput("error_text"),
      # Defining output table
      tableOutput('confusion_matrix'),
      # Defining output table
      tableOutput('F1'),
      # Defining map output
      tmapOutput("map")
    )
  ),)
  return(ui)
}
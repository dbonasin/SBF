ui <- function(country_names, cell_size, lon_poi, lat_poi, lat_test, lon_test, radius, n, algorithms, k, m){
  
  ui_card_style <- '/* Change background color */
                    background-color: white;
                  
                  /* Add border */
                    border: 1px solid #bacdd8;
                  
                  /* Add space between the border and the content */
                    padding: 8px;
                    
                    margin: 8px;
                  
                  border-radius: 12px;'
  ui <- fluidPage(
    titlePanel("SBF"),
    
    sidebarLayout(
      sidebarPanel(
        style = "overflow-y:scroll; max-height: 425px; position:relative",
        
        div(style=ui_card_style,
            
            h3("Select country and define area of interest."),
            
            selectInput("country", 
                        h5("Choose country to display"),
                        choices = country_names,
                        selected = "Country"
            ),
            
            #TODO there are some issues with cell sizes
            numericInput("cell_size", 
                         h5("Enter size of cells"), 
                         value = cell_size
            ),
            
            numericInput("lon_poi",
                         h5("Enter longitude of POI"),
                         value = lon_poi
            ),
            
            numericInput("lat_poi",
                         h5("Enter latitude of POI"), 
                         value = lat_poi
            ),
            
            numericInput("radius",
                         h5("Enter radius of POI (in meters)"), 
                         value = radius
            ),
            
            numericInput("n", 
                         h5("Enter number of areas"), 
                         value = n
            )
        ),
        
        div(style=ui_card_style,
            
            h3("Define SBF options."),
            
            selectInput("algorithm", 
                        h5("Choose hash algorithm"),
                        choices = algorithms,
                        selected = "Algorithm"
            ),
            
            numericInput("k", 
                         h5("Enter number of hash functions"), 
                         value = k
            ),
            
            numericInput("m", 
                         h5("Enter size of bloom filter vector"), 
                         value = m
            )
        ),
        
        div(style=ui_card_style,
            
            h3("Select test point."),
            
            numericInput("lon_test", 
                         h5("Enter longitude of test point"),
                         value = lon_test
            ),
            
            numericInput("lat_test",
                         h5("Enter latitude of test point"), 
                         value = lat_test
            )
        ),
        
        actionButton("go","Submit"),
        
        div(style=ui_card_style,
            
            h3("Select file with multiple test points."),
            
            fileInput(inputId = "csvFiles",
                      h5("Drag and drop here"),
                      multiple = TRUE,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"
                      ),
            
            actionButton("goCSV","Test multiple")
            )
      ),
      
      mainPanel(
        htmlOutput("SBF_label"),
        htmlOutput("error_text"),
        
        tmapOutput("map")
      )
    )
  )
  return(ui)
}
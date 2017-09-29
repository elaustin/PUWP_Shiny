# Define UI for data upload app ----

shinyUI(navbarPage("PUWP Data Explorer", id="nav",
                   theme=shinytheme("flatly"),

tabPanel(strong("Data Upload"),
         div(class = "outer",
fluidPage(
  
  # App title ----
  titlePanel("PUWP Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("usertz",label=h4("Select timezone"),
                  choices = c("Atlantic" = "America/Halifax",
                              "Eastern" = 	"America/New_York",
                              "Central" = "America/Chicago",
                              "Mountain" = "America/Denver",
                              "Pacific" = "America/Los_Angeles",
                              "Alaska" = "America/Anchorage",
                              "Hawaii" = "US/Hawaii"), selected = "America/Los_Angeles"), 
      
      # Input: Select a file ----
      fileInput("file1", "Choose PUWP data files",
                multiple = TRUE,
                accept = c(".csv")),
      "Use Ctrl to select mutliple files.",
      downloadButton("downloadData", "Download"),
      tags$hr(),
      sliderInput("rowsn", "Select number of rows to display",min=1,max=50,step=10, value=10)
      
      
      
      # Input: Checkbox if file has header ----
      #checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      # radioButtons("sep", "Separator",
      #              choices = c(Comma = ",",
      #                          Semicolon = ";",
      #                          Tab = "\t"),
      #              selected = ","),
      
      # Input: Select quotes ----
      # radioButtons("quote", "Quote",
      #              choices = c(None = "",
      #                          "Double Quote" = '"',
      #                          "Single Quote" = "'"),
      #              selected = '"'),
      
      # Horizontal line ----
    
    ),
    
     
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
  )
)
)
),

tabPanel(strong("Data Visualizer"),
        div(class = "outer",
            fluidPage(
              titlePanel("Data Plots"),
              sidebarLayout(
                sidebarPanel(
                  
                  dateRangeInput("Dates",
                              "Date Range:", min="2017-07-01"),
                  sliderInput("ylimpm", "Select Maximum PM2.5 to plot",min=0,max=200,step=10, value=80)
                  # selectInput("colors", "Color Scheme",
                  #             rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                  # checkboxInput("legend", "Show legend", TRUE)
                  
                 
                ),
            
              mainPanel(
                
                plotOutput("tsplot", height = 400)
              )
              )
            )
        )
),

tabPanel(strong("GPS Track"),
         div(class = "outer",
             fluidPage(
               titlePanel("Map of Activities"),
              
                
                   
                   leafletOutput("map1")
                 
               
             )
         )
)
)
)

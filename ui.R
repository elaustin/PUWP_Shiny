library(shiny)
library(shinythemes)

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
                              "Hawaii" = "US/Hawaii"), selected = "Pacific"), 
      
      # Input: Select a file ----
      fileInput("file1", "Choose PUWP data files",
                multiple = TRUE,
                accept = c(".csv")),
      "Use Ctrl to select mutliple files."
      
      
      
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
  )))
)

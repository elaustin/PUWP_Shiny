# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

# Define server logic to read selected file ----


server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    dflist <- lapply(1:nrow(input$file1), FUN = function(fileind) {
      read.PUWP(file=input$file1[[fileind, "datapath"]], 
                filenameval = input$file1[[fileind, "name"]],
                timezone = input$usertz)
    })
   
     df=rbindlist(dflist,  fill=T)
      
      return( apply(head(df),2, as.character))
  
    
  })
  
}
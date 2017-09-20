# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

# Define server logic to read selected file ----

# Create the map




server <- function(input, output, session) {
  
  observe({
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -122.309179, lat = 47.567692, zoom = 12)
    })
  })
  
  data <- reactive({ 
    
    req(input$file1) ## ?req #  require that the input is available
    
    dflist <- lapply(1:nrow(input$file1), FUN = function(fileind) {
      read.PUWP(file=input$file1[[fileind, "datapath"]], 
                filenameval = input$file1[[fileind, "name"]],
                timezone = input$usertz)
    })
    
    df <- rbindlist(dflist,  fill=T)
    df <- df[,localdatetime := as.POSIXct(localdatetime, "%Y/%m/%d %H:%M:%S", tz=input$usertz)]
    df <- df[!is.na(localdatetime),]
    df <<- df
    
    updateDateRangeInput(session, "Dates", label = NULL, start = as.Date(min(df$localdatetime, na.rm=T)),
                         end = as.Date(max(df$localdatetime, na.rm=T)))
    updateSliderInput(session,"rowsn",max=nrow(df))
    
    if(nrow(df<=10))
      updateSliderInput(session,"rowsn",step=1)
    
    return(df)
  })
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

      
      return( apply(data()[1:max(input$rowsn),],2, as.character))
     
    })


output$tsplot <- renderPlot({


if (is.null(df)) {
  
  ggplot() + 
          annotate("text", 
                   x = 4, y = 25, size=8, color="darkgrey",
                   label = "Please Load Data.") + 
          theme_bw() +
          theme(panel.grid.major=element_blank(),
                panel.grid.minor=element_blank())+
          theme(line = element_blank(),
                text = element_blank(),
                title = element_blank())
  
} else  {
  
  plotdata=data()
  
  p1 <- ggplot(plotdata, 
               aes(as.POSIXct(localdatetime), as.numeric(as.character(pm25m)), color=monitor)) + 
    ylab( expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")) ) + xlab("Time") +
    theme_light(12)+ ylim(0, quantile(plotdata$pm25m,.999, na.rm=T)) 
    xlim(as.POSIXct(input$Dates[1],origin="1970-01-01"), as.POSIXct(input$Dates[2],origin="1970-01-01"))

  p1 + geom_point(alpha =0.3, cex=.75)  + 
    #scale_x_datetime(date_breaks ="2 day", date_labels = "%m/%d") +
    #geom_point(data=df, aes(as.POSIXct(datetime), outdoorPM2.5, color="Outdoor"), size=.75) + 
    
    guides(colour = guide_legend(override.aes = list(size=6)))
}

})


observe({

if(!is.null(df)){
  leafletProxy("map", data = data()) %>% #data = zipdata) %>%
    clearShapes() %>%
    addCircleMarkers(~lon, ~lat, radius= 8, layerId = ~GPS, 
                     stroke=T, color="black",weight=2, opacity=.8,
                     fillOpacity=1, 
                     fillColor=monitor)
}
  })


}
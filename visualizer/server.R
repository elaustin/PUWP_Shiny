# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 20*1024^2)

# Define server logic to read selected file ----

# Create the map




server <- function(input, output,session) {
  
  
  
  
  output$map1 <- renderLeaflet(env=parent.frame(sys.nframe()), {
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% clearBounds()
    
  })
  
  outputOptions(output,"map1",suspendWhenHidden=FALSE)
  
  colorpal <- reactive({
    colorNumeric(input$colors, df$monitor)
  })
  
  
  data <- reactive({ 
    
    #req(input$file1) ## ?req #  require that the input is available
    
    if(!is.null(input$file1)){
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
    updateSliderInput(session, "rowsn",max=nrow(df))
    
    updateSliderInput(session, "ylimpm",max=max(as.numeric(df$pm25m), na.rm=T))
    
    
    if(nrow(df<=10))
      updateSliderInput(session,"rowsn",step=1)
    }
    
    return(df)
  })
  
  
  output$contents <- renderTable({
    
    req(input$file1)
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    mydata<-data()
    
    return( apply(mydata[1:max(input$rowsn),],2, as.character))
    
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
        theme_light(12)+ ylim(0, as.numeric(as.character(input$ylimpm)) )+
      xlim(as.POSIXct(input$Dates[1],origin="1970-01-01"), as.POSIXct(input$Dates[2],origin="1970-01-01"))
      
      p1 + geom_point(alpha =0.3, cex=.75)  + 
        #scale_x_datetime(date_breaks ="2 day", date_labels = "%m/%d") +
        #geom_point(data=df, aes(as.POSIXct(datetime), outdoorPM2.5, color="Outdoor"), size=.75) + 
        
        guides(colour = guide_legend(override.aes = list(size=6)))
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mydata", ".csv", sep = "")
    },
    content = function(con) {
      write.csv(data(), con, row.names = FALSE)
    }
  )
  
  
  
  
  observeEvent(input$file1, {
    
    
    mapdata<-data.table(data())
    mapdata<-mapdata[,lon:=as.numeric(as.character(lon))]
    mapdata<-mapdata[,lat:=as.numeric(as.character(lat))]
    mapdata<-mapdata[!lon==0,]
    mapdata<-mapdata[!lat==0,]
    #mapdataind<-min(length(complete.cases(mapdata$lon)), length(complete.cases(mapdata$lat)))
    
    leafletmap <-leafletProxy("map1", data=mapdata)
    
    leafletmap  %>% clearControls() 
    
    
   # if(mapdataind>0){ 
      
      leafletmap  %>%
        addCircleMarkers(~lon, ~lat , radius=7,
                         stroke=T, weight=2, opacity=.8,
                         fillOpacity=.3, group=~monitor)%>%
        setView(median(mapdata$lon, na.rm=T),median(mapdata$lat, na.rm=T), zoom=11)
      
    #}
    
  })
  
  
  
}

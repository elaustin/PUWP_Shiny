####################################################################
#  TWINS AQ study statistics
#  Edmund Seto modified by Elena Austin
#  8/14/2017
####################################################################

# #use the following if pacman is not installed
# #install.packages("pacman")
 library(pacman)
# 
# #load required libraries
# p_load("ggplot2","plyr","data.table" ,"ggthemes","zoo", "ggmap")
# 
# #use the script directory
 "C:/Users/Johnathan/Google Drive/Seto Lab/Air Pollution/Twins PUWP/version 5/scripts"
# 
# ## set the working diretory for data and script output
# dataDir = "C:/Users/Johnathan/Google Drive/Seto Lab/Air Pollution/Twins PUWP/version 5/August 2017 Deployment"
# outputDir = "C:/Users/Johnthan/Google Drive/Seto Lab/Air Pollution/Twins PUWP/version 5/scripts"

# 
# ## useful variables
 timezone = "America/Los_Angeles"
 minimum_date = as.POSIXct("2017-07-20")
 relevant_years = 2017
# 
# ## change working directory to DataDir
# setwd (dataDir)

####################################################################
## read Twin PUWP data (this is "version 5 - TWINS" version)
####################################################################
# setwd (dataDir)
# 
# # returns dataframe with puwp data, note the "m" (mass) concentrations are in ug/m3.
# # the "c" (count) concentrations are in number > than the specific size per 0.1 L air.
# # the PUWP data are originally stored by device in GPS time (GMT). If you give it another timezone it will convert.
# # timezone names are provided on mac at /usr/share/zoneinfo
read.PUWP <- function(file, timezone="GMT", filenameval)
{

	colnames.val <- c("nul", "GMT", "accelx","accely","accelz","RH",
	                      "Temp","GPStime",
	                      "lat", "lon", "pm1m", "pm25m", "pm10m", "pm03c",
	                    "pm05c", "pm1c", "pm25c", "pm5c", "pm10c", "sound (dB)")
	result <- read.csv (file, header = F)
	result <- data.table(result)
	#remove rows with data overflow
  ncoldata<-1:ncol(result)
  ncolcorrect<-1:length(colnames.val)
  #identify rows with data overflow and REMOVE those rows
  if(ncol(result)>length(colnames.val)){
    overflow_columns <-  result[,!ncoldata%in%ncolcorrect,with=F]
    result<-result[apply(!is.na(overflow_columns),1, max,na.rm=T)==0,][,ncolcorrect,with=F]
  }
  colnames(result) = colnames.val
  result[,GMTdatetime :=
           as.POSIXct(GMT, '%m/%d/%Y %H:%M:%S', tz="GMT" )]
  #result$GMT <- NULL
  result$nul <- NULL
  result[, localdatetime :=
           as.POSIXct(format(GMTdatetime,tz=timezone), tz=timezone)]

  #remove datetime values that are empty and print error
  result$monitor <- unlist(strsplit(filenameval,".CSV"))

  ###remove meaninglsess GPS data and fill gaps
  #convert to datetime
  result[,GPStime:=as.POSIXct(GPStime, "%m/%d/%Y %H:%M:%S", tz="GMT")]
  #remove duplicate values
  result[!duplicated(GPStime)&!is.na(GPStime),GPStimeUnique:= as.POSIXct(GPStime,tz="GMT")]

  i = ifelse(is.na(result$GPStime[1]),
         rle(is.na(result$GPStime))$length[1]+1,
         1)
  i = ifelse(i>nrow(result),nrow(result),i)

  #remove non-sensical values of GPStimeUnique
  result[GPStimeUnique < minimum_date,
         GPStimeUnique:=NA]

  result[i:nrow(result),
         GPStimeUnique := na.approx(GPStimeUnique,na.rm=F, maxgap=Inf)]


  #convert to local time
  result[,GPStimelocal := format(GPStimeUnique, tz=timezone)]


  if(!((year(max(result$localdatetime,na.rm=T)) |
        year(min(result$localdatetime,na.rm=T))) %in% relevant_years)){
    #compare localdatetime and GPS
    result[,localdatetime1:=GPStimelocal]
    print(paste(unlist(strsplit(file,".CSV")),"is deriving datetime from the GPS clock"))
  }

  #remove rows that have no datetime and print message:
  print(paste("There are",nrow(result[is.na(localdatetime),]),
              "rows with missing date values for monitor",
              unlist(strsplit(file,".CSV"))))
  if(nrow(result[!is.na(localdatetime),])>0){
    result <- result[!is.na(localdatetime),]
  }
  result$localdatetime1=NULL
  result$GPStimelocal=NULL
  result$GPStimeUnique=NULL
	return (result)
}
# 
# 
# #### Read in all the files in the directory and combine them into a single dataframe for the instrument
# files <- dir()
# files <- grep(".csv", files, ignore.case=T, value=T)
# #removed files tagged as NotUsed
# files <- grep("NotUsed",files,invert=T, value=T, ignore.case=T)
# puwpdatalist <- list()
# 
# puwpdatalist <- lapply(files, FUN = function (x) {
#   temp <- read.PUWP(x, timezone)
#  })
# 
# puwpdata <- rbindlist(puwpdatalist)
# 
# puwpdata[,localdatetime:=as.POSIXct(localdatetime)]
# puwpdata[,localdatetime1:=as.POSIXct(localdatetime1)]
# 
# puwpdata[,bestlocaldate:= as.POSIXct(ifelse(year(localdatetime) %in% relevant_years,
#                                  localdatetime,
#                                  localdatetime1),
#                                  origin="1970-01-01")]
# 
# #remove unecessary rows
# puwpdata$GPStimeUnique=NULL
# puwpdata$GPStimelocal=NULL
# puwpdata$localdatetime1=NULL
# 
# #remove black monitor
# puwpdata <- puwpdata[!monitor%in%"Twins5_Black"]
# 
# ### clean data
# # force rows with numeric data to be numeric (removes shifts from restarts)
# colnames.val <- c("accelx","accely","accelz","RH",
#                   "Temp","lat", "lon", "pm1m", "pm25m", "PM10m", "pm03c",
#                   "pm05c", "pm1c", "pm25c", "pm5c", "pm10c", "sound (dB)")
# #convert numeric columns
# puwpdata[,(colnames.val) := lapply(.SD, FUN = function(x) as.numeric(as.character(x))),
#                  .SDcols=colnames.val]
# 
# # remove rows with any missing data
# #how many rows are missing
# print(paste("There are missing rows:",nrow(puwpdata) -  nrow(na.omit(puwpdata))))
# puwpdata <- na.omit(puwpdata)
# # remove rows with zero counts for smallest particles
# print(paste("There are:",nrow(puwpdata[puwpdata$pm03c == 0,]),
#             "instances of pm03c equal to zero."))
# puwpdata <- puwpdata[puwpdata$pm03c != 0,]
# # remove zero lat lon
# #puwpdata <- puwpdata[(puwpdata$lat != 0) | (puwpdata$lon != 0),]
# nrow(puwpdata)
# 
# 
# #monitor names
# monitor_names <- unique(puwpdata$monitor)
# 
# #get BH data
# 
# beacon<-read.csv("..\\BeaconHillData.csv")
# 
# beacon <- as.data.table(beacon)
# 
# beacon[, datetime := as.POSIXct(paste(Date, Time), format="%m/%d/%Y %I:%M %p", tz=timezone)]
# 
# #summarize to nearest minute
# puwpdata[,minute_local:=format(bestlocaldate,"%Y-%m-%d %H:%M:00")]
# minute_pupw <-
#   puwpdata[,lapply(.SD, mean), by=c("minute_local","monitor"),.SDcols=colnames.val]
# 
# #merge personal and beacon hill data
# 
# beacon[, datetime := as.POSIXct(datetime)]
# minute_pupw[, minute_local := as.POSIXct(minute_local)]
# 
# uniquetime<-data.table(datetime=unique(minute_pupw$minute_local))
# 
# setkey(beacon, datetime)
# 
# setkey(uniquetime,datetime)
# 
# setkey(minute_pupw, minute_local)
# 
# beacon = beacon[uniquetime]
# beacon$outdoorPM2.5 = na.spline(beacon$FEM_TPM25)
# beacon <- beacon[,c("datetime", "outdoorPM2.5"), with=F]
# 
# merged_data = beacon[minute_pupw]
# 
# #read GPS data from TWINS group
# 
# gpslist <- list()
# 
# gpsfiles <- list.files(paste0(dataDir,"/GPS_data"))
# gpsfiles <- grep(".csv", gpsfiles, value=T)
# 
# gpslist <- lapply(paste0(dataDir,"/GPS_data/",gpsfiles),FUN = function(x)
#   {
#   temp = fread(x)
#   temp = temp[,1:20,with=F]
#   temp$monitor = strsplit(strsplit(x," - ")[[1]][2],".csv")[[1]]
#   temp
# })
# 
# gpsdata<-rbindlist(gpslist)
# 
# 
# reference_vals = data.frame(monitor= unlist(lapply(unique(gpsdata$monitor), FUN = function(x){
#   agrep(x,  c("black",unique(merged_data$monitor)), fixed=F, value=T)
# })))
# 
# rownames(reference_vals) = unique(gpsdata$monitor)
# 
# gpsdata[ ,monitor := reference_vals[monitor,]]
# 
# gpsdata[, datetimeUTC := as.POSIXct(paste(`UTC DATE`, `UTC TIME`), tz="UTC")]
# 
# gpsdata[, datetimelocal := as.POSIXct(format(datetimeUTC, tz="America/Los_Angeles"),
#                                       tz="America/Los_Angeles")]
# 
# gpsdata <- gpsdata[,c("datetimelocal", "datetimeUTC", "monitor","VALID","LATITUDE",
#                       "LATITUDE","LONGITUDE","HEIGHT","SPEED","HEADING","DSTA","DAGE",
#                       "PDOP","HDOP","VDOP")]
# 
# gpsdata[monitor=="black",monitor:=NA]
# 
# gpsdata[, height := as.numeric(unlist(strsplit(HEIGHT,"M")))]
# 
# gpsdata[, speed := as.numeric(unlist(strsplit(SPEED,"km/h")))]
# 
# #merge with data
# 
# gpsdata[, datetimelocal := as.POSIXct(format(datetimelocal, "%Y-%m-%d %H:%M:00"))]
# gpsdata[, datetimeUTC := as.POSIXct(format(datetimeUTC, "%Y-%m-%d %H:%M:00"))]
# 
# gpsdata <- gpsdata[!is.na(monitor),]
# 
# gpsdata_minute <- gpsdata[, lapply(.SD, mean, na.rm=T), by =c("datetimelocal","monitor"),
#                           .SDcols = c("LATITUDE","LONGITUDE","PDOP","HDOP","VDOP","height","speed")]
# 
# setkey(gpsdata_minute, datetimelocal, monitor)
# setkey(merged_data, datetime, monitor)
# 
# merged_data <- gpsdata_minute[merged_data]
# 
# merged_data <- merged_data[!is.na(monitor),]

# merged_data <- data.table(read.csv("../Minute_data_TWINS5_mergedAPCD_GPS.csv"))
# merged_data[, datetimelocal := as.POSIXct(datetimelocal, tz=timezone)] 
# 
# merged_data[, day := format(datetimelocal, format= "%a, %b %d")]
# merged_data[,day := factor(day, levels=unique(day))]
# 
# 
# ###leaflet plotting
# 
# merged_data_list <- lapply (unique(merged_data$monitor), 
#                             FUN = function (x) {
#                               temp = merged_data[monitor%in%x]
#                               temp = merged_data[, lon:=as.numeric(LONGITUDE)]
#                               temp = merged_data[, lat:=as.numeric(LATITUDE)]
#                               temp
#                               })
# 
# 
# ### plot data (Minute averages)
# p1 <- ggplot(merged_data, 
#              aes(as.POSIXct(datetimelocal), pm25m,color="Personal")) + 
#   ylab( expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")) ) + xlab("Time") +
#   theme_light(12)+ ylim(0, quantile(merged_data$pm25m,.999)) +
#   facet_wrap(~monitor, ncol=1)
# p1 + geom_point(alpha =0.3, cex=0.5)  + 
#   scale_x_datetime(date_breaks ="2 day", date_labels = "%m/%d") +
#   geom_point(data=merged_data, aes(as.POSIXct(datetimelocal), 
#                                    outdoorPM2.5, color="Outdoor"), 
#              size=.5) + scale_colour_manual(name="Air Pollution",
#                                             values=c(Outdoor="red", Personal="blue"))+
#   guides(colour = guide_legend(override.aes = list(size=2)))+
#   theme(legend.position="bottom")
# 
# 
# #Identify missing data gaps
# 
# merged_data[,gaps := c(as.POSIXct(tail(datetimelocal, -1)) - 
#                      as.POSIXct(head(datetimelocal, -1)),-999 ), by="monitor"]
# 
# merged_data[difference>120 ,c("monitor", "datetimelocal","gaps")]
# 
# 
# 
# merged_data[, round(cor(outdoorPM2.5, pm25m),2), by=monitor]
# 
# #plot location
# 
# # merged_data[lon==0, lon:=NA ]
# # merged_data[lat==0, lat:=NA ]
# # merged_data[LATITUDE<35, lon:=NA]
# # merged_data[lat<35, lat:=NA]
# 
# minlon<-min(merged_data$LONGITUDE, na.rm=T)
# maxlon<-max(merged_data$LONGITUDE, na.rm=T)
# minlat<-min(merged_data$LATITUDE, na.rm=T)
# maxlat<-max(merged_data$LATITUDE, na.rm=T)
# 
# 
# monitor_names = unique(merged_data$monitor)
# 
# 
# 
# seattle_map <- get_map(location=c(-median(merged_data$LONGITUDE,na.rm=T), median(merged_data$LATITUDE,na.rm=T)), zoom=12)
# 
# orange_palette = colorRampPalette(c(1,10,20,35,50,70,100), space = "Lab")
# my_orange2 = orange_palette(20)
# 
#                        
# ggmap(seattle_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  color=monitor, size=pm25m), alpha=0.4,
#              data=merged_data[day%in%c("Fri, Aug 04","Tue, Aug 08")]) + theme_map(9) +
#   theme(legend.position="right") +
#   facet_wrap(~day, ncol=1)+
#   scale_size("PM Concentration (ug/m3)",
#              range = c(1, 6), breaks=c(0,10, 20,30,50,75,100))
# 
# seattle_map <- get_map("Seattle")
# ggmap(seattle_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  size=5, color=pm25m), alpha=0.05,
#               data=merged_data[monitor=="Twins6_Red"]) + theme_map(8) +
#   theme(legend.position="right") +
#   facet_wrap(~day) +
#   scale_size("PM Concentration (ug/m3)",
#              range = c(1, 8), breaks=c(0,10,20,35,50,70,100))
# 
# bell_map <- get_map("Bellingham, WA", zoom=9)
# ggmap(bell_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  size=pm25m), alpha=0.1,
#              color="blue",
#              data=merged_data[monitor%in%"Twins2_Purple" &
#                                 day%in%"Sat, Jul 29"]) + theme_map(8) +
#   theme(legend.position="right") +
#   facet_wrap(~day) +
#   scale_size("PM Concentration (ug/m3)",
#              range = c(1, 5), breaks=c(0,10,20,35,50,70,100))
# 
# bell_map <- get_map("Seattle, WA", zoom=9)
# ggmap(bell_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  size=pm25m), alpha=0.1,
#              color="blue",
#              data=merged_data[monitor%in%"Twins7_Purple" &
#                                 day%in%"Sat, Jul 29"]) + theme_map(8) +
#   theme(legend.position="right") +
#   facet_wrap(~day) +
#   scale_size("PM Concentration (ug/m3)",
#              range = c(1, 5), breaks=c(0,10,20,35,50,70,100))
# 
# 
# 
# bell_map <- get_map("Seattle, WA", zoom=10)
# ggmap(bell_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  size=pm25m), alpha=0.2,
#              color="blue",
#              data=merged_data[monitor%in%"Twins2_Purple" &
#                                 day%in%"Sun, Jul 30"]) + theme_map(8) +
#   theme(legend.position="right") +
#   facet_wrap(~day) +
#   scale_size("PM Concentration (ug/m3)",
#              range = c(1, 10), breaks=c(0,10,20,35,50,70,100))
# 
# 
# ggmap(bell_map) + 
#   geom_point(aes(-LONGITUDE, LATITUDE,  size=pm25m), alpha=0.5,
#              color="blue",
#              data=merged_data[monitor%in%"Twins2_Purple"]) + theme_map(12) +
#   facet_wrap(~day) + theme(legend.position="bottom")+
#   scale_size(range = c(1, 50), breaks=c(0,10,20,35,50,70,100))
# 
# 
# 
# 
# 

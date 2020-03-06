library(httr)
library(rgdal)
library(stringr)
library(jsonlite)
library(zoo)
library(xts)

SenFedServer <- 'https://esoil.io/SensorFederationWebAPI/SensorAPI'

convertJSONtoTS <- function(resp){
  
  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))
  
  for (i in 1:nrow(xin)) {
    
    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])
    
  }
  colnames(outDF) <- cnames
  
  ds <- na.omit(outDF)
  d <- as.POSIXct(str_trim(ds$DateTime) , format = "%Y-%m-%d %H:%M:%S")
  ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
  return(ts)
}



depths <- seq(0, 90, 10)

url <- paste0(SenFedServer,"/getSensorLocations?sensortype=Soil-Moisture" )
response <- GET(url)
stop_for_status(response) # stop if the response is an error
sensorLocs <- fromJSON(content(response, as="text"))
bs <- sensorLocs[sensorLocs$SensorGroup == 'EPARF',]

pts <- bs
coordinates(pts) <- ~Longitude+Latitude
plot(pts)




outdf <- data.frame(sid=character(), depth=numeric(), LL=numeric(), UL=numeric(), minDate=character(), maxDate=character())
for (i in 22:nrow(bs)) {
  
  print(i)
  #url <- paste0(SenFedServer, "/getSensorDataStreams?siteid=", 'op33583' ,"&sensortype=", 'Soil-Moisture', "&aggperiod=days&sensorid=785583")
  url <- paste0(SenFedServer, "/getSensorDataStreams?siteid=", bs$SiteID[i] ,"&sensortype=", 'Soil-Moisture', "&aggperiod=days&startdate=2010-01-01T00:00:00")
  
  response <- GET(url, timeout(2000))
  stop_for_status(response) # stop if the response is an error
  sensorData <- content(response, as="text", encoding='UTF-8')
  if(!str_detect( sensorData, 'error')){
  
  ts <- convertJSONtoTS(sensorData)
  
  tsdf <-  as.data.frame(ts)
  write.csv(tsdf, paste0('c:/temp/EPSM/',bs$SiteName[i], '_', bs$SiteID[i], '.csv'))
  mind <- start(ts)
  maxd <- end(ts)
  
  for (j in 1:ncol(ts)) {
    
    df <- ts[, j]
    minv <- min(df[,1])
    maxv <- max(df[,1])
    df <- data.frame(sid=bs$SiteID[i], depth=as.numeric(str_replace(names(ts)[j], 'Soil-Moisture_', '')), LL=minv, UL=maxv, minDate=mind, maxDate=maxd)
    outdf <- rbind(outdf, df)
  }
  
  }
  
}


fls <- list.files('c:/temp/EPSM/', full.names = T)
outdf <- data.frame(sid=character(), depth=numeric(), LL=numeric(), UL=numeric(), minDate=character(), maxDate=character())
for (i in 1:length(fls)) {
  
  print(i)
  
  fname <- fls[i]
  sid <- str_remove(basename(fname), '.csv')
    z <- read.csv.zoo(fls[i])
    x <- as.xts(z)
    mind <- start(x)
    maxd <- end(x)
    
    for (j in 1:ncol(x)) {
      
      
      df <- x[, j]
      minv <- min(df[,1])
      maxv <- max(df[,1])
      df <- data.frame(sid=sid, depth=as.numeric(str_replace(names(x)[j], 'Soil.Moisture_', '')), LL=minv, UL=maxv, minDate=mind, maxDate=maxd)
      outdf <- rbind(outdf, df)
    }
    
  }
  
write.csv(outdf, 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/EP/probeInfo.csv')


indf <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/EP/probeInfo.csv')
head(indf)
head(bs)
write.csv(bs, 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/EP/probeLocs.csv')

indf <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/EP/probeInfo.csv')





probeInfo <- read.csv(paste0(rootDir, '/probeInfo.csv'))
head(probeInfo)








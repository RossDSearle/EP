



#getBucket(SiteID='op26357', currentSW=NULL)

getBucket <- function(SiteID, currentSW, probeInfo){

  rec <- tail(currentSW, 1)
  recv <- t(rec)
  rownames(recv) <- NULL
  
  b <- probeInfo[probeInfo$SiteID==SiteID, ]
  b2<- b[order(b$depth),]
  
  x <- c(b2$LL,  rev(b2$UL))
  y <- c(b2$depth,  rev(b2$depth))
  
  # x <- c(15, 15, 16, 17, 18, 20, 20, 22, 40,  42, 42, 43, 45, 46, 47, 48)
  # y <- c(30, 40, 50, 60, 70, 80, 90, 100, 100, 90, 80, 70, 60, 50, 40, 30)
  
  xm <- c(b2$LL,  rev(recv[,1]))
  ym <- c(b2$depth,  rev(b2$depth))
  

  # xm <- c(15, 15, 16, 17, 18, 20, 20, 22, rev(recv[,1]))
  # ym <- c(30, 40, 50, 60, 70, 80, 90, 100, 100, 90, 80, 70, 60, 50, 40, 30)
  
  dfBucket <- data.frame(x,y)
  dfWater <- data.frame(xm,ym)
  res <- list()
  res$dfBucket <- dfBucket
  res$dfWater <- dfWater
  
  print(min(b2$LL))
  res$minx <- min(b2$LL)
  res$maxx <- max(b2$UL)
  #outDF <- c(dfBucket, dfWater)
  return(res)
}

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




getSMmap <- function(depth, day, bdy){
  
  outDf <- data.frame()
  
  for(i in 1:nrow(bs)){
    print(i)
    
    sid <- bs$SiteID[i]
    
    url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=', sid)
    response <- GET(url)
    stop_for_status(response) # stop if the response is an error
    sensors <- fromJSON(content(response, as="text"))
    
    sens1 <- sensors[grepl(paste0('_', depth, '_'), sensors$SensorID), ]
    sens2 <- sens1[grepl(paste0('_dielectric_constant'), sens1$SensorID), ]
    
    d1 <- as.Date(day)
    d2 <- d1-10
    d3 <- paste0(d2, 'T00:00:00')
    
    url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&sensorid=', sens2$SensorID, '&startdate=', d3)
    #url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensortype=Soil-Moisture&startdate=2020-01-10T00%3A00%3A00')
    
    response <- GET(url)
    stop_for_status(response) # stop if the response is an error
    stream <- content(response, as="text")
    
    xin <- fromJSON(stream)
    outDF <- data.frame(xin$DataStream[[1]]$t)
    cnames<- c('DateTime', rep('x', nrow(xin)))
    
    for (i in 1:nrow(xin)) {
      d <- xin[i,]
      dd <- d$DataStream
      outDF[, (i+1)] <- dd[[1]]$v
      
      if(is.null(d$UpperDepth[1])){
        suffix = paste0('_x', i)
      }else if(is.na(d$UpperDepth[1])){
        suffix = paste0('_x', i)
      }else if(d$UpperDepth == d$LowerDepth[1]){
        suffix = paste0('_', d$UpperDepth[1])
      }else{
        suffix =  paste0('_', d$UpperDepth[1], '_', d$LowerDepth[1])
      }
      cnames[i+1] <- c(paste0(d$DataType[1],suffix))
    }
    colnames(outDF) <- cnames
    
    ts <- outDF
    
    tail(ts)
    rec <- tail(ts, 1)
    index(rec)[1]
    df <- data.frame(SiteID=sid, dt= index(rec)[1], y=bs$Latitude[i], x=bs$Longitude[i], SM=rec[1,1])
    outDf <- rbind(outDf,df)
    
  }
  
  ext <- extent(bdy)
  r <- raster(ext, nrows=100, ncols=100)
  
  xy <- data.frame(x=outDf$x, y=outDf$y)
  tps <- Tps(xy, outDf$SM, lon.lat = T, lambda=0.01)
  p <- interpolate(p, tps)
  p <- mask(p, bdy)
  
  return(p)
  
}
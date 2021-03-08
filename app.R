library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dygraphs)
library(httr)
library(jsonlite)
library(xts)
library(stringr)
library(raster)
library(rgdal)
library(rhandsontable)
library(viridis)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(fields)
library(lubridate)
library(shinybusy)

#library(flexdashboard)

pingAPI <- function(url){
  
  r <- GET(url)
    resp <- GET(url, timeout(300))
    if(http_error(resp)){
      return(NULL)
    }
  response <- content(resp, "text", encoding = 'UTF-8')
  return(response)
}

getTotalAvailableSoilWater <- function(bucket){
  
  lvw_mm <- (bucket$VolWater - bucket$LL) / 100 * bucket$Thickness
  lawc_mm <- (bucket$DUL - bucket$LL) / 100 * bucket$Thickness
  l = list()
  l$AvailWater_mm = sum(lvw_mm)
  l$AWC_mm = sum(lawc_mm)
  return(l)
  
}

getProfileWater <- function(rawProbeVals, soilParams){
  print(soilParams)
  volVals <- data.frame(depth=numeric(nrow(soilParams)), Vol=numeric(nrow(soilParams)))
  volVals[,] <- NA
  for (i in 1:ncol(rawProbeVals)) {
    print(i)
    rec <- soilParams[i, ]
    coln <- colnames(rawProbeVals)[i]
    print(coln)
    depth <- as.numeric(str_split(coln, '_')[[1]][2])
    volVal <- getVolumetricValue(pParams=soilParams, depth=depth, probeVal=as.numeric(rawProbeVals[,i]) )
    volVals[i,1] <- depth
    volVals[i, 2] <- volVal
  }
  
  volVals <- na.omit(volVals)
  d1 <- volVals[order(volVals$depth),]
  return(na.omit(d1))
}



plotPAW <- function(dfBucket, title='PAW', yscale=10){
  
  sbx <- c(dfBucket$LL, rev(dfBucket$DUL))
  sby <- c(dfBucket$Depths, rev(dfBucket$Depths)) /yscale 
  svw <- c(dfBucket$LL,  rev(dfBucket$VolWater)) 
  
  plot( 0, type="n",  
        xlab='Volumteric Soil Moisture (mm)', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c(min(dfBucket$LL)-5, max(dfBucket$DUL) + 5), ylim = rev(range(c(0,110))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(svw,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
}

getBucket <- function(soilAWCValues){
  
  bp<- soilAWCValues
  LL <- bp$LL * 100
  DUL <- bp$DUL * 100
  Depths <-  bp$depth
  VolWater <- bp$Vol * 100
  Thickness= bp$Thickness
  outDF <- data.frame(LL,DUL, VolWater, Depths,Thickness)
  return(outDF)
  
}

getVolumetricValue <- function(pParams, depth, probeVal){
  
  print('here1')
  rec <- pParams[pParams$depth == depth, ]
  minp <- rec$minProbe
  maxp <- rec$maxProbe
  dul <- rec$modDUL
  ll <- rec$modLL
 
  mmperpv <-  (dul-ll)/ (maxp - minp)
  #dv <- dayVals$Soil.Moisture_1000
  vol <- ll + (mmperpv * (probeVal-minp))
  print(vol)
  print('here2')
  return(vol)
}





network = 'EPARF'


defWidth = '380px'
loaderTime = 5
numberofDaysSinceToday <- 10

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/EP'
  dataStoreDir <- '/mnt/data/BARS/SoilPropertyPredictions'
  source(paste0( rootDir, '/appUtils.R'))
  
}else{
  rootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/EP'
  dataStoreDir <- 'C:/Temp/boorowa_2019/data/processed'
  source(paste0( rootDir, '/appUtils.R'))
}

SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'
probeInfo <- read.csv(paste0(rootDir, '/probeInfo.csv'))
head(probeInfo)
#probeLocs <- read.csv(paste0(rootDir, '/probeLocs.csv'))
#head(probeLocs)

probeLocsURL <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=', network)
probeLocsJSON <- pingAPI(probeLocsURL)
AllProbeLocs <- fromJSON(probeLocsJSON)
#soilPolys <- sf::read_sf('C:/Projects/EP/Soil Data/EPSoils4.shp')

today <- paste0(Sys.Date(), 'T00:00:00')

today

sdLabels <- c('30 cm', '40 cm','50 cm','60 cm','70 cm','80 cm','90 cm','100 cm')
sdVals <- c('0', '1', '2','3','4','4','6','7')
soilDepthsDF <- data.frame(sdLabels, sdVals, stringsAsFactors = F)


today <- str_replace(str_remove(Sys.Date()-hours(10), ' UTC'), ' ', 'T')

probeSoilInfo <- read.csv('Data/AllProbeSoilParams.csv', stringsAsFactors = F)
print(head(probeSoilInfo))



probeSites <- unique(probeSoilInfo$sid)
print(probeSites)
probeLocs <- AllProbeLocs[AllProbeLocs$SiteID %in% probeSites, ]
probeNames <- probeLocs$SiteName



shiny::shinyApp(
  ui = f7Page(
    title = "Eyre Peninsular",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="wheat.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
              #tags$title("BCG AgDataShop"),
              #tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
              
              ),
    
    useShinyjs(),
    
    #add_busy_bar(color = "#FF0000", centered = FALSE, height = "18px"),
    #add_busy_spinner(spin = "fading-circle"),
    add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    #title = NULL,
    preloader = F,
    loading_duration = loaderTime,
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "About", side = "left", theme = "dark", effect = "cover",
                
                f7Link(label = "About probe network", src = "http://eparf.com.au/wp-content/uploads/2018/05/1i.-Soil-moisture-probe-network-WARE.pdf", external = TRUE),
                f7Link(label = "Detail data", src = "https://www.outpostcentral.com/remote/EPARFLogin.html", external = TRUE)
                )
      ),
 
##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
       # title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "EP Probe Network"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/landscapeEP.png", width = "100px", height = "40px", align='right'))),
        hairline = T,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),


##################################  UI - SOIL MOISTURE PROBE MAP  ##################################         
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "SM Probes",
          icon = f7Icon("layers_fill", old = TRUE),
          active = TRUE,
          
          
          f7Float( f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        #title = "Click on a probe location to display info below. Scroll down to view results.",
                        # pickerInput(
                        #   inputId = 'wgtSiteID',
                        #   label = "Select on a probe from the list below or click on map location. Scroll down to view results.", 
                        #   choices = probeSites,
                        #   inline = F,
                        #   options = list(mobile = T)
                        #   
                        # ),
                        f7Select(inputId = 'wgtSiteID', label = "Select on a probe from the list below or click on map location. Scroll down to view results.", choices = probeNames),
                        HTML('<BR>'),
                       # f7Select(inputId = 'wgtSiteID', label = "Select SM Probe", choices = probeNames),
                       # HTML('<BR>'),
                       #f7Picker('wgtSiteID', 'Site Id', choices = probeSites),
                        leafletOutput("SoilMoistureProbeMap", height = 400 )
                        
                      )
            )
          ), side = "left" ),
          
          f7Float( 
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              tags$div( style=paste0("width: ", defWidth),
                        f7Card(
                          title = '',
                          plotOutput("bucketPlot")
                        )
              )
            ), side = "left" ),
          
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "Current Soil Water Summary",
                        id = 'swgaugecard',
                        f7Gauge(
                          id = "swTotGauge",
                          type  = "semicircle",
                          value = 0,
                          borderColor = "#2196f3",
                          borderWidth = 40,
                          size = 300,
                          valueFontSize = 30,
                          valueTextColor = "#2196f3",
                          labelText = "Total Soil Water"
                        )
                        
                      ))), side = "left" ),
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = NULL,
                        
                        dygraphOutput("mositureChart1", width = "350", height = "300px")
                        
                      )
            )
          ), side = "left" )
        )
      ,


      
      f7Tab(
        tabName = "History",
        icon = f7Icon("alarm", old = F),
        active = FALSE,
        f7Float(  
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            
            tags$div( style=paste0("width: ", 300),
                      
                      f7Card(
                        title = paste0("Todays Weather (", format(Sys.Date(), format="%B %d %Y"), ')' ),
                        
                        htmlOutput("historyVideo")
                        
                        
                      )
            )
          )
        )
      ),
      
      
      
      
      
      
      
      
##################################  UI - WEATHER   ##################################          
f7Tab(
  tabName = "Weather",
  icon = f7Icon("cloud_heavyrain_fill", old = F),
  active = FALSE,
  f7Float(  
    f7Shadow(
      intensity = 10,
      hover = TRUE,
      
      tags$div( style=paste0("width: ", defWidth),
                
                f7Card(
                  title = paste0("Todays Weather (", format(Sys.Date(), format="%B %d %Y"), ')' ),
                  
                  verbatimTextOutput("todaysRainfall"),
                  verbatimTextOutput("todaysMaxRainfall"),
                  HTML('<BR>'),
                  verbatimTextOutput("todaysCurrentTemperature"),
                  verbatimTextOutput("todaysMinTemperature"),
                  verbatimTextOutput("todaysMaxTemperature"),
                  HTML('<BR>'),
                  verbatimTextOutput("todaysCurrentHumidity"),
                  verbatimTextOutput("todaysMinHumidity"),
                  verbatimTextOutput("todaysMaxHumidity"),
                  HTML('<BR>'),
                  verbatimTextOutput("todaysCurrentWindspeed"),
                  verbatimTextOutput("todaysMinWindspeed"),
                  verbatimTextOutput("todaysMaxWindspeed"),
                  HTML('<BR>'),
                  verbatimTextOutput("todaysCurrentWindDirection")
                  # verbatimTextOutput("todaysMinHumidity"),
                  # verbatimTextOutput("todaysMaxHumidity")
                ))), side = "left"), 
  
  f7Float(  
    f7Shadow(
      intensity = 10,
      hover = TRUE,
      
      tags$div( style=paste0("width: ", defWidth),           
                f7Card(
                  title = "Weather History",
                  prettyRadioButtons(
                    
                    inputId = "WeatherHistoryButtons",
                    label = "Variable:",
                    
                    c("Rainfall" = "Rainfall",
                      "Temperature" = "Temperature",
                      "Humidity" = "Humidity",
                      "Windspeed" = "Wind-Speed"),
                    inline = TRUE,
                    status = "success",
                    animation = "pulse",
                    bigger = T
                  ),
                  dygraphOutput("WeatherHistoryChart", height = "300px")
                )))), side = "left")),





    )
  ),


##################################  SERVER  ##################################   
  server = function(input, output, session) {
    
    session$allowReconnect(TRUE)
    
    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$currentSite <- NULL
    RV$sensorLocs <- probeLocs
    RV$currentSoil <- NULL
    RV$SoilMOistureSensors <- NULL
    RV$currentSoilMoisturePercent <- NULL
    RV$TodaysWeather <- NULL
    RV$HistoricalRainfall <- NULL
    
    
    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    output$historyVideo <- renderUI({
      
      req( input$wgtSiteID)
      sn <- input$wgtSiteID
      sitename <- str_replace_all(sn, ' ', '_')
      #t <- paste0('<iframe width="360" height="560" src="History/', 'movie' , '.m4v" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      
      t <- '<iframe width="460" height="500" src="https://www.youtube.com/embed/5d49tI022Bc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
      HTML(paste(t))
    })
    
    
    
    
    ########  Get sensor locations   ##############
    
    observe({
      
      req(RV$currentSite)
      
      isolate({
      today <- str_replace(str_remove(Sys.Date()-hours(10), ' UTC'), ' ', 'T')
      
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', RV$currentSite, '&sensortype=Rainfall&aggperiod=hours&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
      # RV$TodaysWeather=NULL
      # RV$TodaysWeather$Rainfall <- sum(ts)
      # RV$TodaysWeather$MaxRainfall <- max(ts)
      
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', RV$currentSite,'&sensortype=Temperature&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # print(stream)
      # ts <- convertJSONtoTS(stream)
      # RV$TodaysWeather$CurrentTemp <- tail(ts, 1)
      # RV$TodaysWeather$MinTemp <- min(ts)
      # RV$TodaysWeather$MaxTemp <- max(ts)
      
      
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', RV$currentSite,'&sensortype=Humidity&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
      # RV$TodaysWeather$CurrentHumidity <- tail(ts, 1)
      # RV$TodaysWeather$MinHumidity <- min(ts)
      # RV$TodaysWeather$MaxHumidity <- max(ts)
      # 
      # url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', RV$currentSite,'&sensortype=Wind-Speed&aggperiod=none&startdate=',today)
      # print(url)
      # response <- GET(url)
      # stream <- content(response, as="text", encoding	='UTF-8')
      # ts <- convertJSONtoTS(stream)
      # RV$TodaysWeather$CurrentWindSpeed <- tail(ts, 1)
      # RV$TodaysWeather$MaxWindSpeed <- max(ts, 1)
      # RV$TodaysWeather$MinWindSpeed <- min(ts, 1)
      
     
      })
      
      
    })
    
    output$todaysRainfall <- renderText({paste0('Rainfall : ', RV$TodaysWeather$Rainfall, ' mm') })
    output$todaysMaxRainfall <- renderText({paste0('Maximum Rainfall : ', RV$TodaysWeather$Rainfall, ' mm/hr') })
    
    output$todaysCurrentTemperature <- renderText({paste0('Current Temperature : ', RV$TodaysWeather$CurrentTemp,  intToUtf8(176), 'C') })
    output$todaysMaxTemperature <- renderText({ paste0('Maximum Temperature : ', RV$TodaysWeather$MaxTemp,  intToUtf8(176), 'C') })
    output$todaysMinTemperature <- renderText({ paste0('Minimum Temperature : ', RV$TodaysWeather$MinTemp,  intToUtf8(176), 'C') })
    
    output$todaysCurrentHumidity <- renderText({paste0('Current Humidity : ', RV$TodaysWeather$CurrentHumidity, '%') })
    output$todaysMaxHumidity <- renderText({ paste0('Maximum Humidity : ', RV$TodaysWeather$MaxHumidity, '%') })
    output$todaysMinHumidity <- renderText({ paste0('Minimum Humidity : ', RV$TodaysWeather$MinHumidity, '%') })
    
    output$todaysCurrentWindspeed <- renderText({ paste0('Current Wind Speed : ',  RV$TodaysWeather$CurrentWindSpeed , ' km/hr') })
    output$todaysMinWindspeed <- renderText({ paste0('Maximum Wind Speed : ',  RV$TodaysWeather$MaxWindSpeed , ' km/hr') })
    output$todaysMaxWindspeed <- renderText({ paste0('Minimum Wind Speed : ',  RV$TodaysWeather$MinWindSpeed , ' km/hr') })
    
    
    

    ##################################  SERVER - SOIL PROBE MAP   ##################################   
    
      #   Generate the soil bucket plot   ====
    
    output$bucketPlot <- renderPlot({
      req( RV$currentTS)
      
      sid <- RV$currentSite
      
      soilInfo <- probeSoilInfo[probeSoilInfo$sid==sid,]
      dtnow <- Sys.Date()
      
      dt <- min(dtnow, end(RV$currentTS))
      
      dayVals <-  RV$currentTS[dt]
      
      pw <- getProfileWater(rawProbeVals = dayVals, soilParams = soilInfo)
      soilAWCValues <- data.frame(sid=soilInfo$sid, depth=soilInfo$depth, LL=soilInfo$modLL, DUL=soilInfo$modDUL, Thickness=soilInfo$thickness)
      svals <- merge(soilAWCValues, pw, by='depth')
      dfBucket <- getBucket(soilAWCValues=svals)

      swt <- getTotalAvailableSoilWater(dfBucket)
      swp <- swt$AvailWater_mm/swt$AWC_mm * 100
      
      plotPAW(dfBucket = dfBucket, title=sid, yscale=10)
      
      sw = round(swp, digits=0)
      updateF7Gauge(session, id = 'swTotGauge', value = sw)
      RV$currentSoilMoisturePercent = sw
      
    })
    
    
    ################  Get data from Clicking on a sensor  #################
    observe({
      click<-input$SoilMoistureProbeMap_marker_click
      if(is.null(click))
        return()
      
      RV$currentTS <- NULL
      
      DataType <- 'Soil-Moisture'
      
      sid <- click$id
      print(sid)
      startDate <- '2021-01-01T00:00:00'
      endDate <- '2021-03-08T23:00:00'
      url <- paste0(SenFedServer, "/getSensorDataStreams?siteid=", sid,"&sensortype=", DataType, "&aggperiod=days&startdate=", startDate, "&enddate=", endDate)
      print(url)
      
      response <- GET(url)
     # stop_for_status(response) 
      sensorData <- content(response, as="text")
      
      ts <- convertJSONtoTS(sensorData)
      ts2 <- xts(coredata(ts), as.Date(index(ts)))
      RV$currentTS <- ts2
      RV$currentSite <- sid
    })
    
    observeEvent(input$SoilMoistureProbeMap_marker_click, { # update the map markers and view on location selectInput changes
      p <- input$SoilMoistureProbeMap_marker_click
      if(is.null(p))
        return()
      proxy <- leafletProxy("SoilMoistureProbeMap")
      if(p$id=="Selected"){
        proxy %>% removeMarker(layerId="Selected")
      } else {
        proxy %>% acm_defaults(p$lng, p$lat)
      }
    })
    
    
    ################## Render the Chart from a map drill  ##################
    output$mositureChart1 <- renderDygraph({
      
      if(!is.null(RV$currentTS)){
        
        isolate({
          
          maxVal <- max(RV$currentTS)
          
          dygraph(RV$currentTS ,  main = paste0('SoilMoisture'), ylab = 'Soil Moisture')%>%
            dyAxis("y", label = '',axisLabelWidth = 15) %>%
            dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
            dyLegend(show = "follow", hideOnMouseOut = T, labelsSeparateLines = T)  %>%
            dyRangeSelector()
          
        })
      }
    })  
    
    
    #   Soil Moisture Probe Map  ====
    
    output$SoilMoistureProbeMap <- renderLeaflet({
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = 135.89, lat = -34.1, zoom = 7) %>%
        
        # addControlGPS() %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c( "SW Probes"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    #  Soil Moisture Map proxy
    observe({

      sdf <- RV$sensorLocs
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
                '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
                '<li>Backend : ', sdf[i, "Backend"], '</li>',
                #'<li>Access : ', sdf[i, "Access"], '</li>',
                '<li>Site ID : ', sdf[i, "SiteID"], '</li>')
      })
      
      colCnt <- length(unique(sdf[,input$SensorLabel]))
      colCats <- unique(sdf[,input$SensorLabel])
      colField <- sdf[,input$SensorLabel]
      #factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
     # ppal <- colorFactor(viridis(7), soilPolys$LANSLU) # I'm assuming the variable ward contains the names of the communities.
      
      
      proxy <- leafletProxy("SoilMoistureProbeMap", data = RV$sensorLocs)
      proxy %>% clearMarkers()
      proxy %>% clearControls()
      proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude,
                                    label = lapply(labs, HTML),
                                    stroke = FALSE,
                                    fillOpacity = 1,
                                    color = 'blue',
                                    radius = 10,
                                    layerId=paste0(sdf$SiteID),
                                    group = "SW Probes" )
      # proxy %>% addPolygons(data=soilPolys, color = ppal, weight = 1, 
      #             opacity = 1.0, fillOpacity = 0.5,
      #             fillColor = ppal,
      #             smoothFactor = 0.1,
      #             group = "Soils",
      #             highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE )
      # )
      
    
        # proxy %>%  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
        #             opacity = 1.0, fillOpacity = 0.5,
        #             fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        #             highlightOptions = highlightOptions(color = "white", weight = 2,
        #                                                 bringToFront = TRUE))
      
    })
    
  }
)










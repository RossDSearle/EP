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

library(shinyjs)
library(shinyalert)
library(shinyBS)
library(fields)
library(lubridate)
library(shinybusy)

#library(flexdashboard)


defWidth = '380px'
loaderTime = 5
numberofDaysSinceToday <- 10

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/EP'
  dataStoreDir <- '/mnt/data/BARS/SoilPropertyPredictions'
  source(paste0( rootDir, '/appUtils.R'))
  
}else{
  rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/EP'
  dataStoreDir <- 'C:/Temp/boorowa_2019/data/processed'
  source(paste0( rootDir, '/appUtils.R'))
}

SenFedServer <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI'
probeInfo <- read.csv(paste0(rootDir, '/probeInfo.csv'))
head(probeInfo)
probeLocs <- read.csv(paste0(rootDir, '/probeLocs.csv'))
head(probeLocs)

today <- paste0(Sys.Date(), 'T00:00:00')

sdLabels <- c('30 cm', '40 cm','50 cm','60 cm','70 cm','80 cm','90 cm','100 cm')
sdVals <- c('0', '1', '2','3','4','4','6','7')
soilDepthsDF <- data.frame(sdLabels, sdVals, stringsAsFactors = F)


today <- str_replace(str_remove(Sys.Date()-hours(10), ' UTC'), ' ', 'T')




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
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "EP Probe Network"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/EPARFLogo.png", width = "40px", height = "40px", align='right'))),
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
                        title = "Click on a probe location to display info below. Scroll down to view results.",
                        
                       # f7Select(inputId = 'SMDepth', label = "Select Soil Moisture Depth (cm)", c(30, 40, 50,60,70,80,90,100)),
                       # HTML('<BR>'),
                        leafletOutput("SoilMoistureProbeMap", height = 400 )
                        
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
          ), side = "left" ),
          
          f7Float( 
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = 'Soil Water Bucket',
                        plotOutput("bucketPlot")
                      )
            )
          ), side = "left" )
        )
      )
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
    RV$m <- NULL
    RV$TodaysWeather <- NULL
    RV$HistoricalRainfall <- NULL
    
    
    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    ########  Get sensor locations   ##############
    
    observe({
     # RV$sensorLocs <- probeLocs
    })
    

    ##################################  SERVER - SOIL PROBE MAP   ##################################   
    
      #   Generate the soil bucket plot   ====
    
    output$bucketPlot <- renderPlot({
      req( RV$currentTS)
      
      
      res <- getBucket(RV$currentSite, RV$currentTS, probeInfo = probeInfo)
      
      
      sw <- round(RV$currentTS[1,1], digits = 1)
      updateF7Gauge(session, id = 'swTotGauge', value = sw)
      RV$m = sw
      
      plot( 0, type="n", main=paste( ''), 
            xlab='Volumteric Soil Mositure (%)', ylab='Soil Depth (cm)',
            yaxs = "i", xaxs = "i", xlim = c(res$minx, res$maxx),  ylim = rev(range(c(0,100))),
           # yaxs = "i", xaxs = "i", xlim = c(10, 50), ylim = rev(range(c(0,100))),
            cex.lab = 1.5
      )
      
      dfBucket <- res$dfBucket
      dfWater <- res$dfWater
      
      print(dfBucket)
      print(dfWater)
      
      polygon(dfBucket$x,dfBucket$y,
              col=c("navajowhite3"),
              border=c("navajowhite3"),
              lwd=1, lty=c("solid"))
      
      polygon(dfWater$xm, dfWater$ym,
              col=c("lightskyblue"),
              border=c("lightskyblue"),
              lwd=1, lty=c("solid"))
      
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
      startDate <- '2020-03-01T00:00:00'
      url <- paste0(SenFedServer, "/getSensorDataStreams?siteid=", sid,"&sensortype=", DataType, "&aggperiod=days&startdate=", startDate)
      print(url)
      
      response <- GET(url)
      stop_for_status(response) 
      sensorData <- content(response, as="text")
      ts <- convertJSONtoTS(sensorData)
      RV$currentTS <- ts
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
      factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
      
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
      
    
        # proxy %>%  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
        #             opacity = 1.0, fillOpacity = 0.5,
        #             fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        #             highlightOptions = highlightOptions(color = "white", weight = 2,
        #                                                 bringToFront = TRUE))
      
    })
    
  }
)










library(httr)
library(htmltidy)
library(XML)
library(stringr)
library(DBI)
library(RSQLite)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')

getEmptySitesDF <- function(){
  
  return(data.frame( SiteID = character(), SiteName = character(), SensorGroup = character(),
                          Backend = character(), Access = character(), Usr = character(), pwd = character(),
                          Latitude = numeric(), Longitude=numeric(), Owner = character(), Contact = character(), 
                          ProviderURL = character(), NetworkInfoWebsite = character(), Description = character(),
                          ServerName = character(),
                          stringsAsFactors = F )
         )
}

getEmptySensorDF <- function(){
  
  return(data.frame( SiteID = character(), Active = logical(), SensorID = character(),
                          sensorName = character(), StartDate = character(), EndDate = character(), DataType = character(),
                          UpperDepth = numeric(), LowerDepth = numeric(), Calibrated = logical(), Units = character(), 
                          TotalDays  = numeric(), IsActive = logical(), NumGaps = numeric(), TotalGapDays = numeric(), Gapiness = numeric(),
                          MinimumValue = numeric(), MaximumValue = numeric(), MeanValue = numeric(), StandardDeviation = numeric(), HarvestDate = character(),
                          stringsAsFactors = F )
        )
}


url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', 'EPARF', '&password=', 'eparf',
              '&dateFrom=1/Dec/2020%2000:00:00&dateTo=2/Dec/2019%2000:00:00')
response <- GET(url, encoding = 'UTF-8-BOM')
stream <- content(response, as="text", encoding	='UTF-8')
cat(stream, file='c:/temp/outpost.xml')

xmlObj=xmlParse(stream, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

siteNames <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)
siteIDs <- xpathSApply(doc ,"//opdata:sites/opdata:site/id", xmlValue, ns)
lat <- xpathSApply(doc ,"//opdata:sites/opdata:site/latitude", xmlValue, ns)
lon <- xpathSApply(doc ,"//opdata:sites/opdata:site/longitude", xmlValue, ns)

siteDF <- data.frame(SiteID = siteIDs, SiteName = siteNames, SensorGroup = 'EPARF',
Backend = 'Outpost2', Access = 'Public', Usr = 'EPARF', pwd = 'eparf',
Latitude = lat, Longitude=lon, Owner = 'EPARF', Contact = 'https://eparf.com.au/contact-us/', 
ProviderURL = 'https://www.outpostcentral.com', NetworkInfoWebsite = 'https://eparf.com.au/', Description = 'Public soil mositure probes on the Eyre Peninsular',
ServerName = 'https://www.outpostcentral.com',
stringsAsFactors = F )
write.csv(siteDF, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/EPARF/eparfSites.csv')

sensorDF <- getEmptySensorDF()

for(i in 1:length(sites)){
  print(i)
  streams <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteNames[i],"']/parent::opdata:site/opdata:inputs/opdata:input/name"), xmlValue, ns)
  streamID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteNames[i],"']/parent::opdata:site/opdata:inputs/opdata:input/id"), xmlValue, ns)
  
  if(is.null(loggerID)){
    loggerID <- "None"
  }
    
  df <- data.frame(SiteID = siteIDs[i], Active = TRUE, SensorID = streamID,
                    sensorName = streams, StartDate = NA, EndDate = NA, DataType = NA,
                    UpperDepth = NA, LowerDepth = NA, Calibrated = FALSE, Units = NA, 
                    TotalDays  = NA, IsActive = TRUE, NumGaps = NA, TotalGapDays = NA, Gapiness = NA,
                    MinimumValue = NA, MaximumValue = NA, MeanValue = NA, StandardDeviation = NA, HarvestDate = NA,
                    stringsAsFactors = F )  
  
  sensorDF <- rbind(sensorDF, df)
}


write.csv(sensorDF, 'c:/temp/eparf.csv')

#####    manual edits to get rid of non wanted records

d <- read.csv('c:/temp/eparf.csv', stringsAsFactors = F)
d <- read.csv('c:/temp/eparfEdits.csv', stringsAsFactors = F)

sNames <- unique(d$sensorName)
sNames



ids <- which(grepl('Rainfall', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Rainfall'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'mm'

ids <- which(grepl('Soil Moisture', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Soil-Moisture'


depths <- seq(10, 100, 10)

for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('Soil Moisture ', depths[i], 'cm'), d$sensorName, ignore.case = T))
  #d$DataType[ids] <- 'Soil-Moisture'
  #d$UpperDepth[ids] <- depths[i]
  #d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'Percent'
}

for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('Temperature ', depths[i], 'cm'), d$sensorName, ignore.case = T))
  #d$DataType[ids] <- 'Soil-Temperature'
  #d$UpperDepth[ids] <- depths[i]
  #d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'DegreesCelcius'
}

ids <- which(grepl('Wind Speed', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Wind-Speed'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'km/hr'


ids <- which(grepl('Barometric Pressure', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Atmospheric Pressure'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'hPa'

ids <- which(grepl('Relative Humidity', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Humidity'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'Percent'

ids <- which(grepl('Vapour pressure', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Vapour-Pressure'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'hPa'

ids <- which(grepl('Wind Direction', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Wind-Direction'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'Degrees'

ids <- which(grepl('Temperature °C', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Temperature'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'DegreesCelcius'

ids <- which(grepl('Atmospheric Pressure', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Atmospheric-Pressure'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'hPa'

ids <- which(grepl('Temp °C', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Temperature'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'DegreesCelcius'

ids <- which(grepl('Rel Humidity', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Humidity'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'Percent'

ids <- which(grepl('Wind Dir', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Wind-Direction'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'Degrees'

ids <- which(grepl('Pressure hPA', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Atmospheric-Pressure'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'hPa'

ids <- which(grepl('Rain mm', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Rainfall'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'mm'

ids <- which(grepl('Air Temperature', d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Temperature'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0
d$Units[ids] <- 'DegreesCelcius'

ids <- which(is.na(d$DataType))
d$Units[ids] <- NA


write.csv(d, 'c:/temp/eparfEdits.csv', row.names = F)


eparfSites <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/EPARF/eparfSites.csv', stringsAsFactors = F)
eparfSensors <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/EPARF/eparfSensors.csv', stringsAsFactors = F)

con <- dbConnect()
DBCon <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite', flags = SQLITE_RW)
dbWriteTable(DBCon, "sites", eparfSites, append=T)
dbWriteTable(DBCon, "sensors", eparfSensors, append=T)

dbDisconnect(DBCon)





##############################  Rubbish  #######################################


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
                  '&dateFrom=1/Dec/2017%2000:00:00&dateTo=', '3/Dec/2017%2000:00:00', '&outpostID=', 'op15376')
response <- GET(urlData, encoding = 'UTF-8-BOM')
stream <- content(response, as="text", encoding	='UTF-8')
cat(stream, file='c:/temp/outpost.xml')

xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)

xml_tree_view(xmlObj)


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', 'EPARF', '&password=', 'eparf', 
                  '&dateFrom=1/Dec/2017%2000:00:00&dateTo=1/Dec/2017%2001:00:00&format=csv')
response <- GET(urlData, encoding = 'UTF-8-BOM')
stream <- content(response, as="text", encoding	='UTF-8')
cat(stream, file='c:/temp/outpost.csv')
xmlObj=xmlParse(stream, useInternalNodes = TRUE)
xml_tree_view(xmlObj)

url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', 'EPARF', '&password=', 'eparf',
              '&dateFrom=1/Dec/2020%2000:00:00&dateTo=2/Dec/2019%2000:00:00')


url <- paste0(providerInfo$server, '/api/2.0/dataservice/mydata.aspx?userName=', usr, '&password=', pwd,
              '&outpostID=', opID, '&inputID=', sensorID, '&dateFrom=', dateFrom, '&dateTo=', dateTo)


url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
              '&outpostID=', 'OP42741',  '&dateFrom=1/Mar/2020%2000:00:00&dateTo=', '1/Mar/2020%2002:00:00')

url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
              '&dateFrom=1/Mar/2020%2000:00:00&dateTo=', '2/Mar/2020%2000:00:00')

url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
              '&inputID=', '710569', '&dateFrom=1/Mar/2020%2000:00:00&dateTo=', '5/Mar/2020%2002:00:00')

url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
              '&inputName=', 'Temp °C', '&dateFrom=1/Mar/2020%2000:00:00&dateTo=', '1/Mar/2020%2001:00:00')


response <- GET(url, encoding = 'UTF-8-BOM')
stream <- content(response, as="text", encoding	='UTF-8')
#stream
cat(stream, file='c:/temp/outpost.xml')


xmlObj=xmlParse(response, useInternalNodes = TRUE)


doc <- xmlRoot(xmlObj)

nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

# rawDates <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site[id='", '43657',"']/opdata:inputs/opdata:input/opdata:records/opdata:record/date"), xmlValue, ns)
# rawDates <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/id[text()='", '43657',"']/parent::opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/date"), xmlValue, ns)






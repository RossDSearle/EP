library(XML)

pingAPI <- function(url){
  
  r <- GET(url)
  resp <- GET(url, timeout(300))
  if(http_error(resp)){
    return(NULL)
  }
  response <- content(resp, "text", encoding = 'UTF-8')
  return(response)
}

network = 'EPARF'


http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=opSID_20610&sensortype=Soil-Moisture&startdate=2020-03-29T00%3A00%3A00&enddate=2020-03-29T15%3A00%3A00&aggperiod=days


probeLocsURL <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=', network)
probeLocsJSON <- pingAPI(probeLocsURL)
probeLocs <- fromJSON(probeLocsJSON)

for (i in 1:nrow(probeLocs)){
  print(i)
  p <- probeLocs$SiteID[i]
  a <-pingAPI(paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=',p , '&sensortype=Soil-Moisture&startdate=2020-03-29T00%3A00%3A00&enddate=2020-03-29T01%3A00%3A00&aggperiod=days'))
print(a)
  }



url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', 'EPARF', '&password=', 'eparf',
              '&dateFrom=1/Dec/2019%2000:00:00&dateTo=1/Dec/2019%2001:00:00')

#url <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', 'EPARF', '&password=', 'eparf')

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



lon <- xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input
", xmlValue, ns)










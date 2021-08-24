# ---
# title: "Data Mining Thredds"

# Conversion to R script
# Jeff Mitzelfelt
# 2021/07/19

# Original
# author: "Janelle Christensen"
# date: "8/4/2020"
# output: "html_document"
# ---

# ----------
# purpose: data mining the thredds website
# what will happen: download data from the thredds website
# historical daily, historical monthly, future daily (all models), future monthly (all models)
# adjust as needed
# ----------
library(plyr) 
library(data.table) 
library(here)
# be careful with this, it causes issues for dplyr::group_by
# I didn't try, but the internet says if you load it before tidyverse, that issue goes away
library(tidyverse)
library(beepr)
library(lubridate)
library(RCurl)
# library(directlabels)
# library(ggbeeswarm)
# library(gghalves)
# library(ggrepel)

# Current historical datasets hosted by Yellowstone Solutions THREDDS
historicData <- c("gridMET", "Daymet")
# Current future models hosted by Yellowstone Solutions THREDDS
futureModels <- c("inmcm4", "NorESM1-M", "MRI-CGCM3", "MIROC5", "MIROC-ESM-CHEM", "IPSL-CM5A-LR", "HadGEM2-CC365", "GFDL-ESM2G", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CCSM4", "BNU-ESM")
# Accepted RCP Values
rcpValues <- c("rcp45", "rcp85")

# Time values
startYear <- 1980
endHistoric <- 2019
startFuture <- 2020
endYear <- 2099

createSiteInfo <- function(
  centroid, past_data = "gridMET"){
  centroid["past_data"] <- past_data
  return(centroid)
}

genPath <- function(path){
  if (!file.exists(here::here(path))){
    here::here(dir.create(path))
  }
}

getDatasetType <- function(datasetName){
  if (toupper(datasetName) %in% toupper(historicData)){
    returnType <- "historical"
  }
  else if (toupper(datasetName) %in% toupper(futureModels)){
    returnType <- "future"
  }
  else{
    returnType <- ""
  }
  return(returnType)
}

buildFileStructure <- function(site, lat, lon){
  # create subfolders for data 

  path <- "sites"
  genPath(path)

  path <- paste(path, site, sep = "/")
  genPath(path)

  path <- paste(path, "raw_data", sep = "/")
  genPath(path)

  path <- paste(path, paste("lat",sprintf("%0.5f", lat),"lon",sprintf("%0.5f", lon),sep = "_"), sep = "/")
  genPath(path)

  for (i in c("historical_daily", "historical_monthly", "future_daily", "future_monthly")){
    thisPath <- paste(path, i, sep = "/")
    genPath(thisPath)
  }
}

buildFilePath <- function(
  siteInfo,
  dataset=list(
    model="gridMET",
    rcp=""
  ),
  monthly=F)
{
  site <- siteInfo$Name
  lat <- siteInfo$Lat
  lon <- siteInfo$Long
  currYear <- year(Sys.Date())
  timeFrame <- getDatasetType(dataset$model)
  if (timeFrame == 'historical'){
    rcpString <- ""
    endYear <- endHistoric
  }
  else if (timeFrame == "future"){
    rcpString <- str_interp("${dataset$rcp}_")
    startYear <- startFuture
  }
  else return(F)

  fileName <- str_interp("sites/${
    site
    }/raw_data/${
      paste(\"lat\",sprintf(\"%0.5f\", lat),
      \"lon\",sprintf(\"%0.5f\", lon), 
      sep = \"_\")
    }/${
      timeFrame
    }_${
      ifelse(monthly, \"monthly\", \"daily\")
    }/${
      site
    }_${
      dataset$model
    }_${
      ifelse(timeFrame == \"historical\", \"\", rcpString)
    }${
      startYear
    }_${
      endYear
    }.csv")

  return(fileName)
}

genThreddsUrl <- function(
  lat,
  lon,
  dataName,
  year,
  climvar,
  monthly=F,
  rcpLevel="rcp45",
  timeFrame='daily', # monthly or daily
  baseUrl = "http://www.yellowstone.solutions/thredds/ncss/daily_or_monthly/")
{
  
  historic <- toupper(dataName) %in% toupper(historicData)
  timePeriod <- ifelse(historic, "v2_historical/", "gcm/")

  fullClimvar <- ifelse(monthly, paste(climvar, "_monthly", sep=""), climvar)

  leap <- lubridate::leap_year(year) # Determine if year is a leap year
  startDate <- ifelse(
    monthly,
    str_interp("${year}-01-16T05%3A14%3A31.916Z"), # Monthly option
    str_interp("${year}-01-01T12%3A00%3A00Z")      # Daily option
  )
  endDate <- ifelse(
    monthly,
    str_interp("${year}-12-${ifelse((leap | tolower(dataName)=='daymet'), 16, 17)}T00%3A34%3A14.059Z"), # Monthly option
    str_interp("${year}-12-31T12%3A00%3A00Z")                           # Daily option
  )
  if (historic){
    if (toupper(dataName) == toupper(historicData[1])){
      # gridMET url
      subDir <- "gridmet_v_1_5_historical/"
      fileName <- paste(
        "V_1_5_",
        year,
        "_gridmet_historical_",
        fullClimvar,
        ".nc4",
        sep=""
      )
    }
    else {
      # Daymet url
      subDir <- ifelse(monthly, "monthly/", "daily/")
      fileName <- paste(
        "v2_",
        year,
        "_",
        fullClimvar,
        ".nc4",
        sep=""
      )
    }
  }
  else {
    # Check that data name is in future list and that RCP value is valid
    if (toupper(dataName) %in% toupper(futureModels) & toupper(rcpLevel) %in% toupper(rcpValues)){
      model <- futureModels[match(toupper("daymet"), toupper(futureModels))]
      # Future data URL
      subDir <- paste(
        rcpLevel,
        "/",
        dataName,
        "/",
        sep=""
      )
      fileName <- paste(
        "V_1_5_",
        year,
        "_",
        dataName,
        "_",
        rcpLevel,
        "_",
        fullClimvar,
        ".nc4",
        sep=""
      )
    }
    else{
      # Entries are invalid
      print("Invalid Entries")
      return (-1)
    }
  }

  location <- paste("&latitude=", lat, "&longitude=", lon, sep="")

  attrs <-str_interp("?var=${ifelse(monthly, tolower(climvar), climvar)}${location}&time_start=${startDate}&time_end=${endDate}&accept=csv_file")

  dataUrl <- paste(baseUrl, timePeriod, subDir, fileName, attrs, sep="")
  return(dataUrl)
}

readUrl <- function(url, attempt=1, maxAttempts=3){
  output <- NULL
  thisFrame <- environment()
  tryCatch(
    {
      text <- getURL(url)
      names <- read.table(text=text, sep=",", nrows=1)
      output <- read.table(text=text, sep=",", col.names=names, skip=1)
    },
    error=function(cond){
      cat("*")
      if (attempt <= maxAttempts){
        thisFrame$output <- readUrl(url, attempt=attempt+1, maxAttempts=maxAttempts)
      }
      else {
        message("Data not read properly")
        message(url)
        message(cond)
        stop()
      }
    }
  ) 
  return (output)
}

genData <- function(
  siteInfo,
  dataset="gridMET",
  startYear=1980,
  endYear=2019,
  monthly=F,
  rcpLevel="rcp45",
  climvarList=c("soil_water", "runoff", "rain", "accumswe", "PET", "Deficit", "AET")
){
  lat <- siteInfo$Lat
  lon <- siteInfo$Long

  colAccumulator <- NULL
  for (climvar in climvarList){
    print(climvar)
    periodAccumulator <- NULL
    for (yr in c(startYear:endYear)){
      cat(toString(yr), "")
      url <- genThreddsUrl(lat, lon, dataset, yr, climvar, rcpLevel=rcpLevel, monthly=monthly)
      if (url != -1){
        readData <- readUrl(url)
        periodAccumulator <- rbind(periodAccumulator, readData)
      }
    }
    print("Done")
    colAccumulator <- cbind(colAccumulator, periodAccumulator[,4])
  }
  
  # All data sets except Daymet need to be divided by 10 to have the correct values.
  if (tolower(dataset) != "daymet"){
    divide.by.10 <- function(x, na.rm=F){
      x/10
    }
    colAccumulator <- colAccumulator / 10
  }
  outNames <- c("date", "lat", "lon", paste(tolower(climvarList), ifelse(monthly, "monthly", "daily"), sep="_"))
  outData <- cbind(periodAccumulator[,1:3], colAccumulator)
  names(outData) <- outNames

  return(outData)
}

getData <- function(
  siteInfo,
  datasets=list(
    historic=list(
      model="gridMET",
      rcp=""
    ),
    wcModel=list(
      model="CanESM2",
      rcp="rcp85"
    ),
    wc2Model=list(
      model="HadGEM2-CC365",
      rcp="rcp85"
    ),
    bcModel=list(
      model="MRI-CGCM3",
      rcp="rcp45"
    )
  ),
  timePeriods=c("daily", "monthly"),
  climvarList=c("soil_water", "runoff", "rain", "accumswe", "PET", "Deficit", "AET")
)
{
  site <- siteInfo$Name
  lat <- siteInfo$Lat
  lon <- siteInfo$Long

  # Build File Structure
  buildFileStructure(site, lat, lon)

  # Get Data
  rcpLevel <- ''
  for (dataset in datasets){
    if (toupper(dataset$model) %in% toupper(historicData)){
      beginYear <- startYear
      doneYear <- endHistoric
      rcpLevel <- ''
    }
    else if (toupper(dataset$model) %in% toupper(futureModels)){
      beginYear <- startFuture
      doneYear <- endYear
      rcpLevel <- dataset$rcp
    }

    for(timePeriod in timePeriods){
      monthly <- (timePeriod == "monthly")

      fileName <- buildFilePath(siteInfo, dataset=dataset, monthly=monthly)

      # Check to see if siteData has already been downloaded
      if (! file.exists(here::here(fileName))){
        print(fileName)
        thisData <- genData(siteInfo, dataset=dataset$model, monthly=monthly, startYear=beginYear, endYear=doneYear, rcpLevel=rcpLevel, climvarList=climvarList)
        write_csv(thisData, here::here(fileName))
      }
    }
  }
  # return(fileName)
}


# getData <- function(
#   siteInfo, past_data = "gridMET"){
#   thisSiteInfo <- createSiteInfo(siteInfo, past_data)
#   buildFileStructure(thisSiteInfo$Name, thisSiteInfo$Lat, thisSiteInfo$Long)
#   # getHistoricalDailyData(thisSiteInfo, start = 1980, end = 1989)
#   getHistoricalMonthlyData(thisSiteInfo, start = 1980, end = 1989)
# }
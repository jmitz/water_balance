source("water_balance_data.R")
source("testPlots.R")

historicData <- c("gridMET", "Daymet")

# Current future models hosted by Yellowstone Solutions THREDDS
futureModels <- c("inmcm4", "NorESM1-M", "MRI-CGCM3", "MIROC5", "MIROC-ESM-CHEM", "IPSL-CM5A-LR", "HadGEM2-CC365", "GFDL-ESM2G", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CCSM4", "BNU-ESM")

datasets <- list(
  historic=list(
    model="gridMET",
    rcp=""
  ),
  bcModel=list(
    model="MRI-CGCM3",
    rcp="rcp45"
  ),
  wcModel=list(
    model="CanESM2",
    rcp="rcp85"
  )
)

climvarList <- c('soil water')

years=list(
  startYear= 1980,
  endHist  = 2019,
  startFut = 2020,
  endYear  = 2099
)


dayLoad <- function(
  siteInfo,
  dataset,
  parm = "Deficit"
){
  parmName <- str_interp("${tolower(parm)}_daily")
  vars <- c("date", parmName)

  tempData <- read_csv(here::here(buildFilePath(siteInfo, dataset=dataset)))
  
  returnData <- select(tempData, all_of(vars)) %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(
        date,
        label = TRUE,
        abbr = TRUE),
      day = lubridate::day(date),
      doy = yday(date)
    ) 
  
  return (returnData)
}


loadPlotData <- function(
  siteInfo,
  datasets,
  parm="Deficit"
){
  returnList <- list()
  dataNames <- names(datasets)
  for (name in dataNames){
    dataset <- datasets[[name]]
    if (file.exists(buildFilePath(siteInfo, dataset))){
      returnList <- append(returnList, list(thisData=dayLoad(siteInfo, dataset, parm = parm)))
      names(returnList)[names(returnList)=="thisData"] <- name
    }
  }
  return(returnList)
}


annual <- function(pastDay, endHistoric=years$endHistoric){
  
  annualPast <- pastDay %>% 
  group_by(year) %>% 
  filter(year <= endHistoric) %>% 
  summarize(
    #annual_soil_water = mean(soil_water_daily, na.rm = TRUE),
    #annual_runoff = sum(runoff_daily, na.rm = TRUE), 
    #annual_rain = sum(rain_daily, na.rm = TRUE),
    #annual_accumswe = max(accumswe_daily, na.rm = TRUE),
    #annual_pet = sum(pet_daily, na.rm = TRUE),
    annual_deficit = sum(deficit_daily, na.rm = TRUE) 
    #annual_agdd = max(agdd_daily, na.rm = TRUE), 
    #put this back in when agdd is working
    #annual_aet = sum(aet_daily, na.rm = TRUE)
    )

    return(annualPast)
}


renamePlotData <- function(inData, parm){
  
  parmName <- str_interp("${tolower(parm)}_daily")
  
  returnData <- inData
  names(returnData)[names(returnData) == parmName] <- "dataToPlot"
  return(returnData)
}


genPlotData <- function(plotData, parm="Deficit"){

  pastAnnual <- renamePlotData(plotData$historic, parm) %>% 
    group_by(year) %>%
    filter(year <= years$endHist) %>%
    summarise(annual_value = sum(dataToPlot, na.rm=TRUE)
  )
  
  bcAnnual <- renamePlotData(plotData$bcModel, parm) %>%
    group_by(year) %>%
    filter(year >= years$startFut) %>%
    summarise(bc_annual_value = sum(dataToPlot, na.rm=TRUE)
    )
    
  wcAnnual <- renamePlotData(plotData$wcModel, parm) %>%
    group_by(year) %>%
    filter(year >= years$startFut) %>%
    summarise(wc_annual_value = sum(dataToPlot, na.rm=TRUE)
    )
  
  returnData <- full_join(pastAnnual, full_join(bcAnnual, wcAnnual))
  
  return(returnData)
}


genPlot <- function(
  siteInfo,
  datasets,
  parm="Deficit",
  save_images=FALSE
){
  plotData <- loadPlotData(siteInfo, datasets, parm = parm)
  dataToPlot <- genPlotData(plotData, parm = parm)
  returnPlot <- plot_single_annual(siteInfo, dataToPlot, datasets, parm = parm, save_images = save_images)
  return(returnPlot)
}


plotEmAll <- function(
  sitelist,
  datasets,
  parm='Deficit',
  save_images=FALSE
){
  siteNames <- names(site)
  for (rowNum in 1:nrow(sitelist)){
    site <- sitelist[rowNum,]
    buildFileStructure(site$Name, site$Lat, site$Long, dataType = 'figures')
    print(genPlot(site, datasets, parm = parm, save_images=save_images))
  }
}


genTestData <- function(plotData, parm="Deficit"){
  
  parmdaily <- str_interp("${tolower(parm)}_daily")
  
  pastAnnual <- plotData$historic
  
  names(pastAnnual)[names(pastAnnual) == parmdaily] <- "someName"
  return(pastAnnual)
}


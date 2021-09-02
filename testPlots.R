plot_annual = function(siteInfo, inData, datasets, dry_year=1988, wet_year=2014, parm="Deficit", save_images=FALSE) {

  point_low <- inData %>%
    filter(year == dry_year)
  
  point_high<- inData %>%
    filter(year == wet_year)
  
  line_low <- point_low$annual_value
  
  line_high <- point_high$annual_value
  
  # ---------------
  # create y limit for graph
  # ---------------
  
  max_y <- max(select(inData, -year), na.rm = TRUE) * 1.15
  
  # ---------------
  # graph
  # ---------------
  
  annual_graph_bc <- ggplot() +
    geom_line(data = inData,
              aes(x = year, 
                  y = annual_value, 
                  color = "black"),
              size = .75) +
    geom_line(data = inData, 
              aes(x = year, 
                  y = bc_annual_value, 
                  color = "blue"),
              size = .75) +
    geom_hline(data = point_low, # draws a line at a dry year
               yintercept = line_low, 
               linetype = "dashed",
               color = "#DA621E") +
    geom_hline(data = point_high, # draws a line at a wet year
               yintercept = line_high, 
               linetype = "dashed",
               color = "#1287A8") +
    geom_point(data = point_low, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#DA621E",
               alpha =  0.25,
               size = 4) +
    geom_point(data = point_high, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#1287A8",
               alpha =  0.25,
               size = 4) +
    labs(title = paste(siteInfo$Name, "\nAnnual", parm, "\n", datasets$bcModel$model, toupper(datasets$bcModel$rcp), sep = " "),
         y = ifelse(parm == "agdd_daily", "Growing Degree Days (C)",
                    "Water (mm)")) +
    geom_text(data = point_low, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Dry year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#DA621E") +
    geom_text(data = point_high, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Wet year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#1287A8") +
    scale_color_manual(name = "", values = c("gray60","#A3B86C"), labels = (c(paste(datasets$historic$model), paste(datasets$bcModel$model, toupper(datasets$bcModel$rcp))))) + # colors is a named vector, see top of function
    ylim(NA, max_y) +
    xlim(1960.5, NA)
  
  if(save_images == TRUE){
    ggsave(here::here("sites",
                      siteInfo$Name, #site subfolder
                      "figures", #subfolder
                      paste("lat",siteInfo$Lat,"lon",siteInfo$Long,sep = "_"),
                      paste(tolower(parm),
                            "annual_graph_bc",
                            datasets$bcModel$model,
                            toupper(datasets$bcModel$rcp),
                            ".png",
                            sep = "_")
                      )) #file name
  }
  
  annual_graph_wc <- ggplot() +
    geom_line(data = inData,
              aes(x = year, 
                  y = annual_value, 
                  color = "black"),
              size = .75) +
    geom_line(data = inData,
              aes(x = year, 
                  y = wc_annual_value, 
                  color = "blue"),
              size = .75) +
    geom_hline(data = point_low, 
               yintercept = line_low, 
               linetype = "dashed",
               color = "#DA621E") +
    geom_hline(data = point_high, # draws a line at a wet year
               yintercept = line_high, 
               linetype = "dashed",
               color = "#1287A8") +
    geom_point(data = point_low, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#DA621E",
               alpha =  0.25,
               size = 4) +
    geom_point(data = point_high, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#1287A8",
               alpha =  0.25,
               size = 4) +
    labs(title = paste(siteInfo$Name, "\nAnnual ", parm, "\n", datasets$wcModel$model, " ", toupper(datasets$wcModel$rcp), sep = ""),
         y = ifelse(parm == "ADGG", "Growing Degree Days (C)",
                    "Water (mm)")) +
    geom_text(data = point_low, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Dry year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#DA621E") +
    geom_text(data = point_high, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Wet year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#1287A8") +
    scale_color_manual(name = "", values = c("gray60", "#EBC944"), labels = (c(paste(datasets$historic$model), paste(datasets$wcModel$model, toupper(datasets$wcModel$rcp))))) + # colors is a named vector, see top of function
    ylim(NA, max_y) +
    xlim(1960.5, NA)
  
  if(save_images == TRUE){
    ggsave(here::here("sites",
                      siteInfo$Name, #site subfolder
                      "figures", #subfolder
                      paste("lat",siteInfo$Lat,"lon",siteInfo$Long, sep = "_"),
                      paste(tolower(parm),
                            "annual_graph_wc",
                            datasets$wcModel$model,
                            toupper(datasets$wcModel$rcp),
                            ".png",
                            sep = "_")
                      )) #filename
  }
  
  print(annual_graph_bc)
  print(annual_graph_wc)
}


plot_single_annual = function(siteInfo, inData, datasets, dry_year=1988, wet_year=2014, parm="Deficit", save_images=FALSE) {

  point_low <- inData %>%
    filter(year == dry_year)
  
  point_high<- inData %>%
    filter(year == wet_year)

  point_2016<- inData %>%
    filter(year == 2016)
  
  line_low <- point_low$annual_value
  
  line_high <- point_high$annual_value

  line_2016 <- point_2016$annual_value
  
  # ---------------
  # create y limit for graph
  # ---------------
  
  max_y <- max(select(inData, -year), na.rm = TRUE) * 1.15
  
  # ---------------
  # graph
  # ---------------
  
  annual_graph <- ggplot() +
    geom_line(data = inData,
                  aes(x = year, 
                  y = annual_value,
                  color = "gray60"),
              size = .75) +
    geom_line(data = inData,
              aes(x = year, 
                  y = bc_annual_value,
                  color = "#A3B86C"),
              size = .75) +
    geom_line(data = inData,
              aes(x = year, 
                  y = wc_annual_value, 
                  color = "#EBC944"),
              size = .75) +
    geom_hline(data = point_low, # draws a line at a dry year
               yintercept = line_low, 
               linetype = "dashed",
               color = "#DA621E") +
    geom_hline(data = point_2016, # draws a line at a dry year
               yintercept = line_2016, 
               linetype = "dashed",
               color = "#DA621E") +
    geom_hline(data = point_high, # draws a line at a wet year
               yintercept = line_high, 
               linetype = "dashed",
               color = "#1287A8") +
    geom_point(data = point_low, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#DA621E",
               alpha =  0.25,
               size = 4) +
    geom_point(data = point_2016, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#DA621E",
               alpha =  0.25,
               size = 4) +
    geom_point(data = point_high, # highlights a dry year point
               aes(x = year, y = annual_value),
               color = "#1287A8",
               alpha =  0.25,
               size = 4) +
    labs(title = paste(siteInfo$Name, "\nAnnual", parm, sep = " "),
         y = ifelse(parm == "agdd_daily", "Growing Degree Days (C)",
                    "Water (mm)")) +
    geom_text(data = point_low, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Dry year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#DA621E") +
    geom_text(data = point_2016, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = year, 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#DA621E") +
    geom_text(data = point_high, # labels the line
              aes(x = 1967, 
                  y = annual_value,
                  label = paste("Wet year:", year), 
                  vjust = -0.35), # -0.x moves the text up above the line
              color = "#1287A8") +
    scale_color_manual(name = "", values = c("gray60"="gray60","#A3B86C"="#A3B86C", "#EBC944"="#EBC944"), labels = (c(paste(datasets$historic$model), paste(datasets$bcModel$model, toupper(datasets$bcModel$rcp)), paste(datasets$wcModel$model, toupper(datasets$wcModel$rcp))))) +
    ylim(NA, max_y) +
    xlim(1960.5, NA)
  
  if(save_images == TRUE){

    filepath <- paste(paste("sites",
                      siteInfo$Name, #site subfolder
                      "figures", #subfolder
                      paste("lat",sprintf("%0.5f", siteInfo$Lat),"lon",sprintf("%0.5f", siteInfo$Long),sep = "_"),
                      paste(tolower(parm),
                            "annual_graph_bc",
                            datasets$bcModel$model,
                            toupper(datasets$bcModel$rcp),
                            datasets$wcModel$model,
                            toupper(datasets$wcModel$rcp),
                            sep = "_"),
                            sep = "/"),
                            ".png",
                            sep = "")
    ggsave(here::here(filepath))
  }
    
  return(annual_graph)
  
}

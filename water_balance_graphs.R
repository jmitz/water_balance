
# Need annual_values

annual_values <- bc_annual_values %>% 
  full_join(wc_annual_values) 

# Need bc_annual_values & wc_annual_values

bc_annual_values <- annual_past %>%
  full_join(bc_annual_future) %>% 
  pivot_longer(`annual_soil_water`:`annual_aet_bc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "annual_value") %>% 
  na.omit(annual_values) %>% # new column name to store values
  mutate(decade = case_when(
    .$year %in% c(1980:1989) ~ "1980s",
    .$year %in% c(1990:1999) ~ "1990s",
    .$year %in% c(2000:2009) ~ "2000s",
    .$year %in% c(2010:2019) ~ "2010s",
    .$year %in% c(2020:2029) ~ "2020s",
    .$year %in% c(2030:2039) ~ "2030s",
    .$year %in% c(2040:2049) ~ "2040s",
    .$year %in% c(2050:2059) ~ "2050s",
    .$year %in% c(2060:2069) ~ "2060s",
    .$year %in% c(2070:2079) ~ "2070s",
    .$year %in% c(2080:2089) ~ "2080s",
    TRUE ~ "2090s"
  ))  %>%  mutate(variable = case_when(
    .$variable %in% c("annual_runoff", "annual_runoff_bc") ~ "Runoff",
    #.Svariable calls the variable, then it can be renamed
    .$variable %in% c("annual_agdd", "annual_agdd_bc") ~ "AGDD",
    .$variable %in% c("annual_soil_water", "annual_soil_water_bc") ~ "Soil Water",
    .$variable %in% c("annual_rain", "annual_rain_bc") ~ "Rain",
    .$variable %in% c("annual_accumswe", "annual_accumswe_bc") ~ "Accumulated SWE",
    .$variable %in% c("annual_pet", "annual_pet_bc") ~ "PET",
    .$variable %in% c("annual_deficit", "annual_deficit_bc") ~ "Deficit",
    TRUE ~ "AET" # last ifelse is just labeled as TRUE
  )) %>% 
  mutate(averages = case_when(
    .$year %in% c(1980:2019) ~ "annual_avg",
    TRUE ~ "annual_avg_bc"
  )) %>% 
  mutate(decades = case_when( #don't need individual decades for this data
    .$year %in% c(1980:2019) ~ "1980-2019",
    .$year %in% c(2020:2059) ~ "2020-2059",
    TRUE ~ "2060-2099")) #decades by 39 yr chunks



# ------------
# worst case
# ------------

wc_annual_values <- annual_past %>% 
  full_join(wc_annual_future) %>%
  pivot_longer(`annual_soil_water`:`annual_aet_wc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "annual_value") %>% 
  na.omit(annual_values) %>% # new column name to store values
  mutate(decade = case_when(
    .$year %in% c(1980:1989) ~ "1980s",
    .$year %in% c(1990:1999) ~ "1990s",
    .$year %in% c(2000:2009) ~ "2000s",
    .$year %in% c(2010:2019) ~ "2010s",
    .$year %in% c(2020:2029) ~ "2020s",
    .$year %in% c(2030:2039) ~ "2030s",
    .$year %in% c(2040:2049) ~ "2040s",
    .$year %in% c(2050:2059) ~ "2050s",
    .$year %in% c(2060:2069) ~ "2060s",
    .$year %in% c(2070:2079) ~ "2070s",
    .$year %in% c(2080:2089) ~ "2080s",
    TRUE ~ "2090s" #decade individually
  )) %>% 
  mutate(variable = case_when( #make names more readable
    .$variable %in% c("annual_runoff", "annual_runoff_wc") ~ "Runoff",
    #.$variable calls the variable, then it can be renamed
    .$variable %in% c("annual_agdd", "annual_agdd_wc") ~ "AGDD",
    .$variable %in% c("annual_soil_water", "annual_soil_water_wc") ~ "Soil Water",
    .$variable %in% c("annual_rain", "annual_rain_wc") ~ "Rain",
    .$variable %in% c("annual_accumswe", "annual_accumswe_wc") ~ "Accumulated SWE",
    .$variable %in% c("annual_pet", "annual_pet_wc") ~ "PET",
    .$variable %in% c("annual_deficit", "annual_deficit_wc") ~ "Deficit",
    TRUE ~ "AET" # last ifelse is just labeled as TRUE
  ))  %>% 
  mutate(averages = case_when(
    .$year %in% c(1980:2019) ~ "annual_avg",
    TRUE ~ "annual_avg_wc"
  )) %>% 
  mutate(decades = case_when( #don't need individual decades for this data
    .$year %in% c(1980:2019) ~ "1980-2019",
    .$year %in% c(2020:2059) ~ "2020-2059",
    TRUE ~ "2060-2099")) #decades by 39 yr chunks

# Need annual_past for

annual_past <- past_day %>% 
  group_by(year) %>% 
  filter(!year == 2020) %>% 
  summarize(annual_soil_water = mean(soil_water_daily, na.rm = TRUE),
            annual_runoff = sum(runoff_daily, na.rm = TRUE), 
            annual_rain = sum(rain_daily, na.rm = TRUE),
            annual_accumswe = max(accumswe_daily, na.rm = TRUE),
            annual_pet = sum(pet_daily, na.rm = TRUE),
            annual_deficit = sum(deficit_daily, na.rm = TRUE), 
            #annual_agdd = max(agdd_daily, na.rm = TRUE), 
            #put this back in when agdd is working
            annual_aet = sum(aet_daily, na.rm = TRUE))

# Need past_day

past_day <- past_day_1 %>% 
  select(-lat, -lon, -date)  %>%
  select(which(!colSums(., na.rm = TRUE) < 2)) %>% # this removes variables that sum to 2 (i.e. accumswe in a hot climate)
  mutate(date = date_pd,
         year = lubridate::year(date),
         month = lubridate::month(date,
                                  label = TRUE,
                                  abbr = TRUE),
         day = lubridate::day(date),
         doy = yday(date)) 

# Need past_day_1

past_day_1 <- read_csv(here::here("sites",
                                      site,
                                  "raw_data",
                                  paste("lat",lat,"lon",lon, sep = "_"),
                                  "historical_daily",
                                  paste(site,"_daily_",past_data,".csv", sep = "")),
                           na = c("-3276.7")) 

# need site, lat, lon, past_data to generate file path to CSV file.

plot_annual = function(.y) {
   
   point_low <- annual_values %>%
     filter(variable %in% c(.y)) %>% 
     filter(year == dry_year)
   
   point_high<- annual_values %>%
     filter(variable %in% c(.y)) %>% 
     filter(year == wet_year)
   
   line_low <- point_low$annual_value
   
   line_high <- point_high$annual_value
   
   # ---------------
   # create y limit for graph
   # ---------------
   
    max_y_1 <- annual_values %>%
      filter(variable %in% c(.y))
    
    max_y_2 <- max(max_y_1[,3], na.rm = TRUE)
    
    max_y <- max_y_2*1.15
    
    # ---------------
    # graph
    # ---------------
  
  annual_graph_bc <- ggplot() +
    geom_line(data = annual_values %>% 
                filter(averages %in% c("annual_avg", "annual_avg_bc")) %>% 
                filter(variable == .y), # filter data for historical and bc lines
              aes(x = year, 
                  y = annual_value, 
                  color = averages),
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
  labs(title = paste("Annual", .y, "\n", model_bc, " ",model_bc_rcp_name, sep = ""),
       y = ifelse(.y == "agdd_daily", "Growing Degree Days (C)",
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
    scale_color_manual(name = "", values = c("gray60","#A3B86C"), labels = (c(paste(past_data), paste(model_bc, model_bc_rcp_name)))) + # colors is a named vector, see top of function
 ylim(NA, max_y) +
      xlim(1960.5, NA)
  
    if(save_images == TRUE){
  ggsave(here::here("sites",
                    site, #site subfolder
                    "figures", #subfolder
                    paste("lat",lat,"lon",lon,sep = "_"),
                    paste(.y, "annual_graph_bc.png", sep = "_"))) #file name
    }
  
  annual_graph_wc <- ggplot() +
    geom_line(data = annual_values %>% 
                filter(averages %in% c("annual_avg", "annual_avg_wc")) %>%
                filter(variable == .y), # filter data for historical and wc lines
              aes(x = year, 
                  y = annual_value, 
                  color = averages),
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
  labs(title = paste("Annual ", .y, "\n", model_wc, " ", model_wc_rcp_name, sep = ""),
       y = ifelse(.y == "ADGG", "Growing Degree Days (C)",
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
    scale_color_manual(name = "", values = c("gray60", "#EBC944"), labels = (c(paste(past_data), paste(model_wc, model_wc_rcp_name)))) + # colors is a named vector, see top of function
  ylim(NA, max_y) +
    xlim(1960.5, NA)
  
  if(save_images == TRUE){
  ggsave(here::here("sites",
                    site, #site subfolder
                    "figures", #subfolder
                    paste("lat",lat,"lon",lon, sep = "_"),
                    paste(.y, "annual_graph_wc.png", sep = "_"))) #filename
  }
  
  # return(annual_graph_bc)
  # return(annual_graph_wc) with these two it only prints the 4.5 graph
  
  print(annual_graph_bc)
  print(annual_graph_wc)
  
}

################################################################################
#   Compiling of a specific week of observations


# in this file I loop through a specific week of obsevrations (1-4) 
# and save all the datasets below each other which have the columns:
#   uid (ACT001W), timestamp, value (30.5) and variable IBH_TEMP


rm(list=ls())


# for handling file paths and different operating systems
source("functions.R")

# libraries
library(readr);library(tidyr);library(dplyr);library(readxl)
library(lubridate);library(stringr);library(ggplot2);library(gridExtra); library(grid)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_2"


# LOAD DATA
#---- 

# load redcap from CCH

# REDCap for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_data.csv") |>
  dplyr::mutate(starttime = ymd_hms(starttime),
                  endtime   = ymd_hms(endtime),
                  redcap_event_name = substr(redcap_event_name, 13,18)) |>
  filter(redcap_event_name == week_indicator)|>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O"))) |>
  filter(str_starts(uid, "ACT"))

# REDCap for exclusion of pvls
redcap_pvl = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_pvl.csv") |>
  dplyr::mutate(redcap_event_name = substr(redcap_event_name, 13,18))|>
  filter(redcap_event_name == week_indicator) |>
  filter(str_starts(uid, "ACT"))


#---- 


# PREAMBLE FOR LOOPING
#---- 
# reate "emtpy" data.frame to be filled
data_full <- data.frame(uid      = "ACT",
                        datetime = redcap$starttime[1],
                        Value = 120,
                        Variable = "IBX")

# substrings to call every iButton and noise file
indicators <- data.frame(place = c("IBH", "IBH", "IBT", "IBW", "IBW", ""),
                         variable = c("HUM", "TEMP", "TEMP", "HUM", "TEMP", "NS"))
#---- 



# FOR LOOP COMPILING
#----


# loop through unique uids
for (uid in unique(redcap$uid)) {
  print(uid)

# week indicator!!! -------------------------------------------------------

  
   # extract all the files for every uid
    files_all <- list.files(paste0("~/SynologyDrive/Participants/", uid, "/week2/"), full.names = TRUE) 
    
  # only continue if the folder is not emty
  if (length(files_all) > 0) {
    
    
    # loop through all dataset indicators
    for (placevar in 1:nrow(indicators)) {
      
      place = indicators[placevar,1]
      variable = indicators[placevar,2]
      
      
      # extract file name for indicators
      file <- files_all[grepl(place, files_all) & grepl(variable, files_all)]
      
      # only continue of the chosen excel file is not empty
      if (length(file) > 0) {
        
        # remove temp/hidden files         
        file <- file[!grepl("^~\\$", basename(file))]  
        
        # ensure we only pick the **first valid file** (if multiple exist)
        file <- file[1]
        
        # Read the data
        if (file.exists(file)) {  # Double-check file exists
          
          
          
          
          
          # distinguish between noise and other files in assigning cols
          if(variable != "NS"){
            
            # Read the Excel file
            data <- read_excel(file)
            
            # find the row number where "Date" and "Time" are located
            header_row <- which(data[, 1] == "Date" & data[, 2] == "Time" )
            
            # keep emtly data files out
            if(length(header_row) != 0){
              data <- read_excel(file, skip = header_row) |>
                dplyr::mutate(datetime = ymd_hms(paste(Date, Time)),
                              Value = as.numeric(Value),
                              uid = uid,
                              Variable = paste0(place,"_",variable)) |>
                select(uid, datetime, Value, Variable)
              
              # exclude data before/after the pvl's
              start_time <- redcap$starttime[redcap$uid == uid]
              end_time <- redcap$endtime[redcap$uid == uid]
              
              data <- data |>
                filter(datetime >= start_time & datetime <= end_time)
              
              # assign to the right column based on datetime
              data_full <- rbind(data_full, data) 
            }
            
            
            
          } else {
            
            data <- read.delim(file, skip = 2, header = TRUE)  
            
            colnames(data) <- c("datetime", "Value")
            
            data <- data[,1:2] |>
              dplyr::mutate(uid = uid,
                            Variable = paste0(place,"_",variable)) |>
              select(uid, datetime, Value, Variable)
            
            # exclude data before/after the pvl's
            start_time <- redcap$starttime[redcap$uid == uid]
            end_time <- redcap$endtime[redcap$uid == uid]
            
            data <- data |>
              filter(datetime >= start_time & datetime <= end_time)
            
            # assign to the right column based on datetime
            data_full <- rbind(data_full, data) 
          }
        }
      } 
    }
  }
}
#----

# SAVE THE DATA 
#----

data_full <- data_full |> 
  filter(uid != "ACT")

# save the minute data
# write the data to csv 
write_csv(data_full, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/", week_indicator, "_minute_data_unclean.csv"))
write_csv(data_full, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Participants/", week_indicator, "_IB_RAW_data_unclean.csv"))


# create hourly averages
data_hourly <- data_full %>%
  mutate(hour = floor_date(ymd_hms(datetime), "hour")) %>%  # Round datetime to the nearest hour
  group_by(uid, hour, Variable) %>%               # Group by UID, hour, and Variable
  summarise(Value_avg = mean(Value, na.rm = TRUE), .groups = "drop")  # Calculate hourly mean


# unique data.frames with hourly averages
data_H <- data_hourly |>
  filter(Variable == "IBH_HUM" | Variable == "IBH_TEMP") |>
  pivot_wider(names_from = Variable, values_from = Value_avg) |>
  mutate(id_time = paste0(uid, hour))

data_T <- data_hourly |>
  filter(Variable == "IBT_TEMP") |>
  mutate(id_time = paste0(uid, hour))

data_W <- data_hourly |>
  filter(Variable == "IBW_HUM" | Variable == "IBW_TEMP")|>
  pivot_wider(names_from = Variable, values_from = Value_avg)|>
  mutate(id_time = paste0(uid, hour))

# data_N <- data_hourly |>
#   filter(Variable == "_NS")|>
#   mutate(id_time = paste0(uid, hour))


data_N <- data_full |>
  filter(Variable == "_NS")|>
  mutate(hour = floor_date(ymd_hms(datetime), "hour")) %>%
  group_by(uid, hour) |>
  summarise(NS = 10 * log10(mean(10^(Value / 10), na.rm = TRUE)),
                        .groups = "drop") |>
  mutate(id_time = paste0(uid, hour))

# combine hourly datasets.
# create time data based on redcap start and end time for later merging
datetime_series <- data.frame(datetime = ymd_hms("2099-01-01 09:00:00"),
                              uid = "XXX")
participants = unique(redcap$uid)

for(uid in unique(redcap$uid)){
  print(uid)
  
  redcap_subset = redcap[redcap$uid == uid,]
  
  df_timeseries = data.frame(datetime = seq(from = floor_date(min(ymd_hms(redcap_subset$starttime), na.rm = TRUE), "hour"), to = floor_date(max(ymd_hms(redcap_subset$endtime), na.rm = TRUE), "hour"), by = "hour"))
  
  df_timeseries$uid = rep(uid, nrow(df_timeseries))
  
  datetime_series = rbind(datetime_series, df_timeseries)
}

datetime_series <- datetime_series |>
  filter(uid != "XXX") |>
  mutate(id_time = paste0(uid, datetime))

# data_combined <- data_H %>%
#   full_join(data_W %>% select(id_time, IBW_HUM, IBW_TEMP), by = "id_time") %>%
#   full_join(data_T %>% select(id_time, IBT_TEMP = Value_avg), by = "id_time") %>%
#   full_join(data_N %>% select(id_time, NS = Value_avg), by = "id_time")
data_combined <- datetime_series |>
  full_join(data_H |> select(id_time, IBH_HUM, IBH_TEMP), by = "id_time") |>
  full_join(data_W |> select(id_time, IBW_HUM, IBW_TEMP), by = "id_time") |>
  full_join(data_T |> select(id_time, IBT_TEMP = Value_avg), by = "id_time") |>
  full_join(data_N |> select(id_time, NS = NS), by = "id_time") |>
  filter(!is.na(uid)) |>
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) |>
  mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "CET") - 3600)


# write the data to csv 
write_csv(data_combined, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/", week_indicator, "_hourly_data_unclean.csv"))
write_csv(data_combined, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Participants/", week_indicator, "_IB_hourly_data_unclean.csv"))



#----








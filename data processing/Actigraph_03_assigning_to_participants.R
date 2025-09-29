###############################################################################
### Actigraph variables relocation
################################################################################

# the purpose of this file

# In this document, I load all the individual actigraph measurement files (like
# steps, temperature, and the heart rate variables) and copy files into the 
# individual folders of the participants under data-raw

# clear environements
rm(list = ls())

# libraries
library(dplyr); library(ggplot2);library(ggnewscale);library(viridis);library(lubridate);library(readr)

# week indicator
week_indicator = "week_2"
week_indicator2 = "week2"

# load redcap from CCH for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_data.csv") |>
  mutate(starttime = ymd_hms(starttime),
                  endtime   = ymd_hms(endtime),
                  redcap_event_name = substr(redcap_event_name, 13,18)) |>
  filter(redcap_event_name == week_indicator)|>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O", "ACT048L", "ACT051G", "ACT060E"))) |>
  filter(str_starts(uid, "ACT"))

# vector of all uids
uids <- unique(redcap$uid)

# loop over all participants
for(uid in uids){
  
  print(uid)
  uidx = uid
  
  # load Heart Rate, Cardiac Rythym, Heart Rate Var and Interbeatinterval from HR folder
  hr_files = list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR/")
  
  hr_files <- hr_files[grepl(paste0(uid, "_", week_indicator2), hr_files)]
  
  # grepl the individual files for every variable and participant
  filename_HR <- hr_files[grepl("HeartRate.csv", hr_files)]
  filename_CR <- hr_files[grepl("Cardiac", hr_files)]
  filename_HRV <- hr_files[grepl("HeartRateV", hr_files)]
  filename_IBI <- hr_files[grepl("Inter", hr_files)]
  
  # copy the files if they exist into the new location
  #HR
  if(length(filename_HR) != 0){
    output_file <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                          week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_HR_RAW.csv")
    
    if (!file.exists(output_file)) {
      HR <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR/", filename_HR)) |> 
        mutate(uid = uidx)
      write_csv(HR, output_file)
    }
  }
  
  # CR
  if(length(filename_CR) != 0){  
    output_file_CR <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                             week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_CR_RAW.csv")
    if (!file.exists(output_file_CR)) {
      CR <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR/", filename_CR)) |> 
        mutate(uid = uidx)
      write_csv(CR, output_file_CR)
    }
  }
  
  # HRV
  if(length(filename_HRV) != 0){
    output_file_HRV <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                              week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_HRV_RAW.csv")
    if (!file.exists(output_file_HRV)) {
      HRV <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR/", filename_HRV)) |> 
        mutate(uid = uidx)
      write_csv(HRV, output_file_HRV)
    }
  }
  
  # IBI
  if(length(filename_IBI) != 0){
    output_file_IBI <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                              week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_IBI_RAW.csv")
    if (!file.exists(output_file_IBI)) {
      IBI <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR/", filename_IBI)) |> 
        mutate(uid = uidx)
      write_csv(IBI, output_file_IBI)
    }
  }

  # Temperature
  temp_files = list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/")
  temp_files <- temp_files[grepl(paste0(week_indicator2, ".*Temp"), temp_files)]
  
  # load temperature from csv folder if the file exists
  filename_Temp <- temp_files[grepl(paste0(uid, ".*", week_indicator2), temp_files)][1]
  
  # create timeseries of temperature file from the metadata above the numeric data
  if (!is.na(filename_Temp)) {
    output_file_Temp <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                               week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_Temp_RAW.csv")
    
    if (!file.exists(output_file_Temp)) {
      Temp <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 9)
      # Temp <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 11)
      
      # construct datetime series for TEMP
      date_TEMP <- colnames(read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 3))
      time_TEMP <- colnames(read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 2))
      # date_TEMP <- colnames(read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 5))
      # time_TEMP <- colnames(read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/", filename_Temp), skip = 4))
      
      # extract time and date
      time_str <- sub("Start Time ", "", time_TEMP)
      date_str <- sub("Start Date ", "", date_TEMP)
      start_datetime <- as.POSIXct(paste(date_str, time_str), format = "%d.%m.%Y %H:%M:%S")
      
      if(length(date_TEMP) != 0){
      # create minute sequence
      Temp$datetime <- seq(from = start_datetime, by = "60 sec", length.out = nrow(Temp))
      
      # assign uid
      Temp <- Temp |> mutate(uid = uidx)
      
      # write new temperature file
      write_csv(Temp, output_file_Temp)
      } else (print("Temp datetime empty"))
    }
  }
  
  # step counts file
  step_files <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/steps/")
  
  # load the steps file from the steps folder
  filename_Steps <- step_files[grepl(paste0(uid, ".*", week_indicator2), step_files)]
  
  if (length(filename_Steps) != 0) {
    output_file_Steps <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/",
                                week_indicator, "/", uid, "/", uid, "_", week_indicator2, "_actigraph_Steps_RAW.csv")
    
    if (!file.exists(output_file_Steps)) {
      STEPS <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/steps/", filename_Steps), skip = 10) |> 
        mutate(uid = uidx)
      
      write_csv(STEPS, output_file_Steps)
    }
  }
  
gc()

}





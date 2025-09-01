################################################################################
### Actigraph sleep compiling
################################################################################

# the purpose of this file

# in this document I loop through the output of GGIR for every participant and
# save the sleep data in one combined dataset

# empty environment
rm(list = ls())

# libraries
library(lubridate);library(readr);library(tidyverse)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_1"

# load redcap from CCH for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv") |> 
  filter(str_detect(redcap_event_name, week_indicator)) |>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O"))) |>
  filter(str_starts(uid, "ACT")) 

# vector of all uids
uids <- unique(redcap$uid)

# file path to participants
filepath_part <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/", week_indicator,"/")

# empty dataframe for rbind in loop
df_sleep <- data.frame()

# dummy participant for script dev
# redcap <- redcap |>
#   filter(str_starts(uid, "ACT044"))
# redcap <- redcap |>
#   filter(str_starts(uid, "ACT00"))
# uids <- unique(redcap$uid)


# loop over all participants
for(uidx in uids){
  
  print(uidx)
  
  # list the files of the folder in which the sleep summary is located
  files_uid <- list.files(paste0(filepath_part, uidx, "/RAW_processed/output_", uidx, "/results/"))
  
  # get the file
  file_sleep <- files_uid[grepl("4_night", files_uid)]
  
  if (length(file_sleep) != 0) {
    
    # load weartime validation and cut by observation period
    sleep <- read_csv(paste0(filepath_part, uidx, "/RAW_processed/output_", uidx, "/results/", file_sleep)) |>
      mutate(uid = uidx)
    
      
    df_sleep <- rbind(df_sleep, sleep)
    }
}


# save the cleaned but RAW resolution data
write_csv(df_sleep, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_sleep_RAW_data.csv"))


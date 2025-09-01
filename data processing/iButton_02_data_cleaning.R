###############################################################################
# DATA CLEANING SCRIPT BASED ON vignettes/Data_Cleaning_Protocol_revised.Rmd


rm(list=ls())


# for handling file paths and different operating systems
source("functions.R")

# libraries
library(readr);library(tidyr);library(dplyr);library(readxl);library(zoo)
library(lubridate);library(stringr);library(ggplot2);library(gridExtra); library(grid)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_2"


# LOAD and SPLIT DATA
#----
# iButton and Noise data
data <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Participants/", week_indicator, "_IB_RAW_data_unclean.csv"))
  
# REDCap for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv") |> 
  select(uid, redcap_event_name, pvl_start, pvl_end, starts_with("pvl_ib")) |>
  drop_na(pvl_start) |>
  filter(str_detect(redcap_event_name, week_indicator)) |>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O"))) |>
  filter(str_starts(uid, "ACT"))


# house data
data_H <- data |>
  filter(Variable == "IBH_HUM" | Variable == "IBH_TEMP")|>
  pivot_wider(names_from = Variable, values_from = Value)


# worn data
data_W <- data |>
  filter(Variable == "IBW_HUM" | Variable == "IBW_TEMP")|>
  pivot_wider(names_from = Variable, values_from = Value)|>
  mutate(
    IBW_HUM_MSD = rollapply(IBW_HUM, width = 3, FUN = sd, align = "left", fill = NA)
  ) 

# taped data
data_T <- data |>
  filter(Variable == "IBT_TEMP")|>
  pivot_wider(names_from = Variable, values_from = Value) 

# noise data
data_N <- data |>
  mutate(Variable = if_else(Variable == "_NS", "NS", Variable)) |>
  filter(Variable == "NS")|>
  pivot_wider(names_from = Variable, values_from = Value)|>
  mutate(
    NS_MA = rollmean(NS, k = 8, fill = NA, align = "left"))


#----


# CLEANING
#----
# 1. PVL-VISITS
# First, data has to be excluded that was taken outside the observation window
# and during personal visit log times if the devices were changed.
# The data was cut to the observation window in the data compiling but the checking 
# whether the device was changed will be done here.

# loop through uids
for(uids in unique(redcap$uid)){
  print(uids)
  redcap_subset <- redcap[redcap$uid == uids,]
  
  # loop through pvl visits
  for(i in 1:nrow(redcap_subset)) {
    
    
    startvalue <- redcap_subset$pvl_start[i]
    endvalue <-  redcap_subset$pvl_end[i]
    
    # if house Ibutton was changed
    if(redcap_subset$pvl_ibuthouse[i] == 1 ){
      
      # set values to NA
      data_H <- data_H |>
        mutate(IBH_HUM = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBH_HUM),
               IBH_TEMP = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBH_TEMP))
    }
    
    # if worn Ibutton was changed
    if(redcap_subset$pvl_ibutworn[i] == 1 ){
      
      # set values to NA
      data_W <- data_W |>
        mutate(IBW_HUM = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBW_HUM),
               IBW_TEMP = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBW_TEMP),
               IBW_HUM_MSD = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBW_HUM_MSD))
    }
    
    # if taped Ibutton was changed
    if(redcap_subset$pvl_ibuttaped[i] == 1 ){
      
      # set values to NA
      data_T <- data_T |>
        mutate(IBT_TEMP = if_else(uid %in% uids & datetime > startvalue & datetime < endvalue, NA, IBT_TEMP))
    }
  }
}


# 2. Physically possible
# Every Variable (temperature, RH, noise) has its physical limits that the following:
#   
#   1. Temperature: < -273 °C
#   2. RH: < 0 % and > 100 %
#   3. Noise: < 0 dB

# House
data_H <- data_H |>
  mutate(IBH_TEMP = if_else(IBH_TEMP < -273, NA, IBH_TEMP),
         IBH_HUM = if_else(IBH_HUM < 0 | IBH_HUM > 100, NA, IBH_HUM))

# Worn
data_W <- data_W |>
  mutate(IBW_TEMP = if_else(IBW_TEMP < -273, NA, IBW_TEMP),
         IBW_HUM = if_else(IBW_HUM < 0 | IBW_HUM > 100, NA, IBW_HUM))

# Taped
data_T <- data_T |>
  mutate(IBT_TEMP = if_else(IBT_TEMP < -273, NA, IBT_TEMP))

# Noise
data_N <- data_N |>
  mutate(NS = if_else(NS < 0, NA, NS))



# 3. Physically plausible
# The plausible range is to some degree subjective, depends on the observation surroundings and changes not only depending on the variable, but also what the variable describes (temperature taped and house). Therefore now we need to start with device specific variable value ranges. 
# 
# 1. House: Temperature < 0 °C and > 55 °C, RH: no additional filtering
# 2. Worn: Temperature < 10 °C and > 45 °C, RH: no additional filtering
# 3. Taped: Temperature below the 10th percentile (no upper filtering because taped temperature almost always is greater than house temperature) (25th percentile was too high)
# 4. Noise: no additional filtering

# House
data_H <- data_H |>
  mutate(IBH_TEMP = if_else(IBH_TEMP < 0 | IBH_TEMP > 55, NA, IBH_TEMP))

# Worn
data_W <- data_W |>
  mutate(IBW_TEMP = if_else(IBW_TEMP < 15 | IBW_TEMP > 45, NA, IBW_TEMP))

# Taped
thrsh = quantile(data_T$IBT_TEMP, .10, na.rm = T)
data_T <- data_T |>
  mutate(IBT_TEMP = if_else(IBT_TEMP < thrsh, NA, IBT_TEMP))


# 4. Variability 
# We do want to filter out worn measurements that resemble the variance of the house measurements 
# and indicate the the device was not worn. We use the moving standard deviation of 3 left aligned 
# humidity values. As an additional measure to prevent filtering out reasonable values, we filter 
# only measurements if the standard deviation has been too low for 4 consecutive measurements. 

# this treshold was adjusted for more severe filtering because we introduce some of the sporadic non filtered data back
# into the cleaned data through the hourly averaging
thrsh = 1

# Worn
data_W <- data_W |>
  mutate(IBW_HUM_thrsh = if_else(IBW_HUM_MSD < thrsh, 1, 0),
         IBW_HUM_MA_thrsh = rollmean(IBW_HUM_thrsh, k = 2, fill = NA, align = "left"),
         IBW_TEMP = if_else(IBW_HUM_MA_thrsh == 1, NA, IBW_TEMP),
         IBW_HUM = if_else(IBW_HUM_MA_thrsh == 1, NA, IBW_HUM))


# Save the data on CCH
# write the data to csv 
write_csv(data_H, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_IBH_RAW_data_clean.csv"))
write_csv(data_W, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_IBW_RAW_data_clean.csv"))
write_csv(data_T, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_IBT_RAW_data_clean.csv"))
write_csv(data_N, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_RAW_data_clean.csv"))



# CBIND AND SAVE DATA
#----

# create time data based on redcap start and end time for later merging
datetime_series <- data.frame(datetime = ymd_hms("2099-01-01 09:00:00"),
                              uid = "XXX")
participants = unique(redcap$uid)

for(uid in unique(redcap$uid)){
  print(uid)
  
  redcap_subset = redcap[redcap$uid == uid,]
  
  df_timeseries = data.frame(datetime = seq(from = floor_date(min(ymd_hms(redcap_subset$pvl_end), na.rm = TRUE), "hour"), to = floor_date(max(ymd_hms(redcap_subset$pvl_start), na.rm = TRUE), "hour"), by = "hour"))
  
  df_timeseries$uid = rep(uid, nrow(df_timeseries))
  
  datetime_series = rbind(datetime_series, df_timeseries)
}
  

datetime_series <- datetime_series |>
  filter(uid != "XXX") |>
  mutate(uid_time = paste0(uid, datetime))

# datetime_series <- data_W |>
#   mutate(datetime_hourly = floor_date(ymd_hms(datetime), "hour"),
#          uid_time = paste0(uid, datetime_hourly)) |>
#   select(uid, uid_time, datetime_hourly) |>
#   distinct(uid, datetime_hourly, uid_time)


# create hourly averages and then cbind all variables
data_H_hourly <- data_H |>
  mutate(datetime_hourly = floor_date(ymd_hms(datetime), "hour")) |>
  group_by(uid, datetime_hourly) |>
  summarise(IBH_HUM = mean(IBH_HUM, na.rm = TRUE),
            IBH_TEMP = mean(IBH_TEMP, na.rm = TRUE),
            .groups = "drop")  |>
  mutate(uid_time = paste0(uid, datetime_hourly)) 

data_W_hourly <- data_W |>
  mutate(datetime_hourly = floor_date(ymd_hms(datetime), "hour")) |>
  group_by(uid, datetime_hourly) |>
  summarise(IBW_HUM = mean(IBW_HUM, na.rm = TRUE),
            IBW_TEMP = mean(IBW_TEMP, na.rm = TRUE),
            .groups = "drop") |>
  mutate(uid_time = paste0(uid, datetime_hourly))

data_T_hourly <- data_T |>
  mutate(datetime_hourly = floor_date(ymd_hms(datetime), "hour")) |>
  group_by(uid, datetime_hourly) |>
  summarise(IBT_TEMP = mean(IBT_TEMP, na.rm = TRUE),
            .groups = "drop") |>
  mutate(uid_time = paste0(uid, datetime_hourly))

# data_N_hourly <- data_N |>
#   mutate(datetime_hourly = floor_date(ymd_hms(datetime), "hour")) |>
#   group_by(uid, datetime_hourly) |>
#   summarise(NS = 10 * log10(mean(10^(NS / 10), na.rm = TRUE)),
#             .groups = "drop") |>
#   mutate(uid_time = paste0(uid, datetime_hourly))


data_combined <- datetime_series |>
  full_join(data_H_hourly |> select(uid_time, IBH_HUM, IBH_TEMP), by = "uid_time") |>
  full_join(data_W_hourly |> select(uid_time, IBW_HUM, IBW_TEMP), by = "uid_time") |>
  full_join(data_T_hourly |> select(uid_time, IBT_TEMP), by = "uid_time") |>
  # full_join(data_N_hourly |> select(uid_time, NS), by = "uid_time") |>
  filter(!is.na(uid)) |>
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) |>
  mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "CET") - 3600)
  

# Save the hourly combined aggregated data on CCH
  # write the data to csv 
write_csv(data_combined, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_IB_hourly_data_clean.csv"))


#----





################################################################################
### WEEKLY SCREENING OF INDIVIDUAL LEVEL DATA
################################################################################

# the purpose of this app

# screen the raw data on synology to see whether it looks clean and has no 
# major issues or measurement errors

# clear environment
rm(list=ls())

# libraries
library(shiny);library(readr);library(tidyr);library(dplyr);library(readxl); library(grid)
library(lubridate);library(stringr);library(ggplot2);library(pdftools);library(gridExtra);

# # week indicator
week_indicator = "week_4"

# load cleaned recap data locally
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_data.csv") |>
    dplyr::mutate(starttime = ymd_hms(starttime),
                  endtime   = ymd_hms(endtime),
                  redcap_event_name = substr(redcap_event_name, 13,18)) |>
  filter(redcap_event_name == week_indicator)
  
# function for file pathing
source("functions.R")

# load user interface and server
source("app_weekly_screening_reports/ui_weekly_screening.R")
source("app_weekly_screening_reports/server_weekly_screening.R")


# run the app
shinyApp(ui = ui, server = server)


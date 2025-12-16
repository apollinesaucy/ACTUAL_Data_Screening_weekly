################################################################################
### GGIR processing
################################################################################

# the purpose of this file

# in this file I prepare the GGIR run by copying the RAW file into its own folder
# and then generate accelerometern and sleep output using GGIR.

# empty environment
rm(list=ls())

# libraries
library(dplyr); library(ggplot2);library(ggnewscale);library(viridis);library(stringr);library(lubridate)
library(readr); library(GGIR)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_3"

# load redcap from CCH for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_data.csv") |>
  dplyr::mutate(starttime = ymd_hms(starttime),
                endtime   = ymd_hms(endtime),
                redcap_event_name = substr(redcap_event_name, 13,18)) |>
  filter(redcap_event_name == week_indicator)|>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O", "ACT048L", "ACT051G", "ACT060E"))) |>
  filter(str_starts(uid, "ACT"))

uids <- unique(redcap$uid)


# STEP 1
# create the subfolders using the uids from redcap
for (uid in uids) {
  print(uid)
  folderpath <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/", week_indicator, "/", uid)
  print("Subfolder for uid already exists!")
  
  if (!dir.exists(folderpath)) {
    dir.create(folderpath, recursive = TRUE)
  }
}



# CAREFUL - WEEK INDICATOR in list.files()!!

# Step 2
# copy the .RAW files from the csv folder only of week_1 to the corresponding folder
files <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv/",
                    pattern = "ACT.*week3.*RAW.*\\.csv$", 
                    full.names = TRUE)

files <- files[c(1:48, 50:72)]

for (uid in uids) {
  print(uid)
  # select the file for the current uid
  selected_file <- files[grepl(uid, basename(files))]
  
  # skip if no matching file is found
  if (length(selected_file) == 0) next
  
  # define output location
  output_loc <- file.path("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants",
                          week_indicator, uid, basename(selected_file))
  
  # only copy if file doesn't already exist
  if (!file.exists(output_loc)) {
    file.copy(from = selected_file, to = output_loc)
    print("file copied successfully!")
  } else {print("file already in folder!")}
}



# Step 3 
# Run GGIR for every RAW file in every folder
for (uid in uids) { 
  
  print(uid)
  
  # data directory for GGIR
  datadir = paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/", week_indicator, "/", uid, "/") 
  
  # folder path for output folder
  folderpath <- paste0(datadir, "RAW_processed")
  
  # Skip if RAW_processed folder exists and is not empty
  if (dir.exists(folderpath) && length(list.files(folderpath)) > 0) {
    message(paste("RAW_processed already populated for", uid, "- skipping."))
    next
  }
  
  # create outputfolder if it doesnt exist
  if (!dir.exists(folderpath)) dir.create(folderpath)
  
  # define output directory for GGIR
  outputdir = paste0(folderpath, "/")
  
  # check if RAW file exists (assuming file name is RAW.csv)
  rawfile <- list.files(datadir, pattern = "\\)RAW.csv$", full.names = TRUE)
  if (length(rawfile) == 0) {
    message(paste("No RAW file found for", uid, "- skipping."))
    next  # skip to next iteration
  }
  
  # Run GGIR
  tryCatch({
    GGIR(
      mode = c(1, 2, 3, 4, 5),
      datadir = datadir,
      outputdir = outputdir,
      dataformat = "csv",
      csv.format = "actilife",
      csv.acc.col.acc = 2:4,
      csv.header = TRUE,
      csv.time.col = 1,
      csv.IDformat = 3,
      csv.col.names = TRUE,
      do.cal = TRUE,
      do.enmo = TRUE,
      strategy = 1,
      do.part3.sleep.analysis = TRUE,
      epochvalues2csv = TRUE,
      epochvalues2csv_minutes = 60,
      save_ms5rawlevels = TRUE,
      save_ms5raw_format = "csv",
      part5_agg2_60seconds = TRUE
    )
  }, error = function(e) {
    message(paste("Error processing", uid, ":", e$message))
  })
  
  gc()
}
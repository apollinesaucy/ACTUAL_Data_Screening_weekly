################################################################################
### HR VARIABLES CACULAITON
################################################################################

# the purpose of this file

# process all the RAW.csv files from the csv folder to all 4 heart rate variables
# and save them in the HR folder

# empty environment
rm(list = ls())

# ---- USER OPTIONS ----
# Set which outputs you want to generate
generate_hr  <- TRUE   # Heart Rate
generate_hrv <- TRUE   # Heart Rate Variability
generate_cr  <- TRUE  # Cardiac Rhythm
generate_ibi <- TRUE   # Inter-Beat Interval
# -----------------------

# Define paths
hr_exe <- "data processing/Actigraph_XX_HR_caclulation.exe"
input_dir <- "/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv"
output_dir <- "/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR"

# Get all RAW.csv files
raw_files <- list.files(input_dir, pattern = "RAW\\.csv$", full.names = TRUE)

# Loop over each file and process it
for (raw_csv in raw_files) {
  # browser()
  base_name <- sub("(\\))RAW\\.csv$", "\\1", basename(raw_csv))
  print(base_name)
  
  # Construct all possible output paths
  ppg_csv <- file.path(input_dir, paste0(base_name, "ppg25Hz.csv"))
  hr_csv  <- file.path(output_dir, paste0(base_name, "_HeartRate.csv"))
  hrv_csv <- file.path(output_dir, paste0(base_name, "_HeartRateVar.csv"))
  cr_csv  <- file.path(output_dir, paste0(base_name, "_CardiacRhythm.csv"))
  ibi_csv <- file.path(output_dir, paste0(base_name, "_InterBeatInterval.csv"))
  
  # Check if any selected output is missing
  needs_processing <- (
    (generate_hr  && !file.exists(hr_csv))  ||
      (generate_hrv && !file.exists(hrv_csv)) ||
      (generate_cr  && !file.exists(cr_csv))  ||
      (generate_ibi && !file.exists(ibi_csv))
  )
  
  if (needs_processing) {
    # browser()
    # Construct command arguments dynamically
    args <- c("-a", shQuote(raw_csv), "-p", shQuote(ppg_csv), "-z", "CET")
    
    if (generate_hr)  args <- c(args, "-e", shQuote(hr_csv))
    if (generate_hrv) args <- c(args, "-u", shQuote(hrv_csv))
    if (generate_cr)  args <- c(args, "-b", shQuote(cr_csv))
    if (generate_ibi) args <- c(args, "-i", shQuote(ibi_csv))
    
    # Run the command
    system2(hr_exe, args = args, wait = TRUE)
    cat("Processed:", raw_csv, "\n")
  } else {
    cat("Skipped (selected outputs exist):", raw_csv, "\n")
  }
}


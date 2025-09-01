library(fs)
library(stringr)

# Define source directory (where all participant folders are)
source_dir <- "~/SynologyDrive/Participants"

# Define destination directory (where all files will be copied)
destination_dir <- "/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph"

# Find all .agd and .agsd files in subdirectories
files <- dir_ls(source_dir, recurse = TRUE, type = "file", glob = "*.agd")
files <- c(files, dir_ls(source_dir, recurse = TRUE, type = "file", glob = "*.agsd"))


# Process each file
for (file in files) {
  # browser()
  # Extract parent folder names
  path_parts <- str_split(file, "/", simplify = TRUE)
  n <- length(path_parts)
  if (n >= 3) {  # Ensure we have at least two parent folders
    parent1 <- path_parts[n-2]  # Participant folder
    parent2 <- path_parts[n-1]  # Monitoring week folder
    file_name <- path_parts[n]  # Original file name
    # Construct new filename
    new_name <- paste0(parent1, "_", parent2, "_", file_name)
    # Define new file path
    new_path <- file.path(destination_dir, new_name)
    # Check if the file already exists before copying
    if (!file_exists(new_path)) {
      file_copy(file, new_path)
      cat("Copied:", new_name, "\n")
    } else {
      cat("Skipped (already exists):", new_name, "\n")
    }
  }
}

cat("Files copied and renamed successfully!\n")

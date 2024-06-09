corrupt_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist.")
  }

  if (file.info(file_path)$isdir) {
    stop("The provided path is a directory, not a file.")
  }

  file_size <- file.info(file_path)$size
  random_data <- as.raw(sample(0:255, file_size, replace = TRUE))

  writeBin(random_data, file_path)
  message("File corrupted successfully.")
}

corrupt_file("data/survey_num_27_10.csv")
corrupt_file("data/survey_text_27_10.csv")

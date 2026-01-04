package_list <- c("pastecs", "openxlsx", "tidyr", "tools", "haven", "plyr", "dplyr", "summarytools", "purrr", "car", "psych", "ggplot2", "lmtest") #nolint
new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])] # nolint
if (length(new_packages)) install.packages(package_list)

invisible(lapply(package_list, library, character.only = TRUE))

data_path <- "data/data.csv" # nolint
file_type <- tools::file_ext(data_path)

if (file_type == "csv") {
  data <- read.csv(data_path, header=TRUE)
} else if (file_type %in% c("xlsx", "xls")) {
  data <- read.xlsx2(data_path, 1)
} else if (file_type == "sav") {
  data <- read_sav(data_path)
}
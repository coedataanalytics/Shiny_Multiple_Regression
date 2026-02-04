if (!require(pacman)) {
  install.packages("pacman")
}

usethis::use_git_ignore(c(".Rhistory",".RData",".Rproj.user",".Renviron",".env",
                          "data-raw/","outputs/","logs/", "processed_data/"))
usethis::use_readme_rmd()


#load library
install.packages("tidyverse")
install.packages("haven")
install.packages("readxl")
library(tidyverse)
library(haven)
library(readxl)

#data management
#loading data
my_csv_data <- read.csv("data/filename.csv")
my_xls_data <- read_excel("data/filename.xls")
my_xlsx_data <- read_excel("data/filename.xlsx")
spss_data <- read_sav("data/filename.sav")

#viewing 50 rows of data
head(my_xls_data, n = 50:100)
view(my_xls_data, )
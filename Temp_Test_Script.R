if (!require(pacman)) {
  install.packages("pacman")
}

usethis::use_git_ignore(c(".Rhistory",".RData",".Rproj.user",".Renviron",".env",
                          "data-raw/","outputs/","logs/", "processed_data/"))
usethis::use_readme_rmd()

# testing this stuff

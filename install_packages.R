pkgs <- c(
  "shiny", "bslib", "bsicons",
  "leaflet", "leaflet.extras",
  "dplyr", "readr", "lubridate",
  "ggplot2", "DT",
  "httr2", "here"
)

install.packages(pkgs[!pkgs %in% installed.packages()[, "Package"]],
                 repos = "https://cloud.r-project.org")

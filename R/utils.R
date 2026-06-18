library(dplyr)
library(readr)
library(lubridate)

SNAPSHOT_FILE <- here::here("data", "snapshots.csv")

# Known military ICAO prefixes (hex ranges) and callsign patterns
MILITARY_CALLSIGN_RE <- "^(RCH|REACH|PAT|VENUS|CONDA|SPAR|IRON|BLADE|FURY|GHOST|HAWK|EAGLE|VIPER|HALO|REAPER|BISON|TUSK|JAKE|WOLF|OTTER|CYLON|BRRT)"

load_snapshots <- function(hours_back = 24) {
  if (!file.exists(SNAPSHOT_FILE)) return(tibble())
  df <- read_csv(SNAPSHOT_FILE, show_col_types = FALSE,
                 col_types = cols(fetched_at = col_character()))
  df |>
    mutate(fetched_at = ymd_hms(fetched_at)) |>
    filter(fetched_at >= Sys.time() - hours(hours_back))
}

flag_military <- function(df) {
  df |> mutate(
    is_military = grepl(MILITARY_CALLSIGN_RE, callsign, ignore.case = TRUE) |
      grepl("^AE", icao24, ignore.case = TRUE)  # US military ICAO block
  )
}

latest_snapshot <- function(df) {
  if (nrow(df) == 0) return(df)
  latest_time <- max(df$fetched_at, na.rm = TRUE)
  df |> filter(fetched_at == latest_time)
}

activity_by_hour <- function(df) {
  df |>
    mutate(hour = hour(fetched_at)) |>
    group_by(hour) |>
    summarise(n_aircraft = n_distinct(icao24), .groups = "drop")
}

top_callsigns <- function(df, n = 20) {
  df |>
    filter(nchar(callsign) > 0, !is.na(callsign)) |>
    count(callsign, origin_country, sort = TRUE) |>
    slice_head(n = n)
}

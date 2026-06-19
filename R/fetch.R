# Fetch aircraft data from OpenSky Network and append to local snapshot store

library(httr2)
library(dplyr)
library(readr)

SNAPSHOT_FILE <- file.path("data", "snapshots.csv")

# West Point area bounding box (roughly 50-mile radius)
# lamin, lomin, lamax, lomax
BBOX <- list(
  lamin = 40.5, lomin = -74.5,
  lamax = 41.5, lomax = -73.5
)

fetch_aircraft <- function(bbox = BBOX) {
  resp <- request("https://opensky-network.org/api/states/all") |>
    req_url_query(
      lamin = bbox$lamin, lomin = bbox$lomin,
      lamax = bbox$lamax, lomax = bbox$lomax
    ) |>
    req_timeout(30) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    message("OpenSky fetch failed: ", resp_status(resp))
    return(NULL)
  }

  body <- resp_body_json(resp)
  if (is.null(body$states) || length(body$states) == 0) {
    message("No aircraft in bounding box at ", Sys.time())
    return(NULL)
  }

  cols <- c(
    "icao24", "callsign", "origin_country", "time_position",
    "last_contact", "longitude", "latitude", "baro_altitude",
    "on_ground", "velocity", "true_track", "vertical_rate",
    "sensors", "geo_altitude", "squawk", "spi", "position_source"
  )

  df <- lapply(body$states, \(s) {
    s[sapply(s, is.null)] <- NA
    as.data.frame(setNames(s, cols[seq_along(s)]), stringsAsFactors = FALSE)
  }) |> bind_rows()

  df |>
    select(icao24, callsign, origin_country, longitude, latitude,
           baro_altitude, on_ground, velocity, true_track, squawk) |>
    mutate(
      callsign    = trimws(iconv(callsign, to = "UTF-8", sub = "")),
      fetched_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      baro_altitude = as.numeric(baro_altitude),
      longitude   = as.numeric(longitude),
      latitude    = as.numeric(latitude),
      velocity    = as.numeric(velocity),
      true_track  = as.numeric(true_track),
      on_ground   = as.logical(on_ground)
    )
}

append_snapshot <- function() {
  df <- fetch_aircraft()
  if (is.null(df)) return(invisible(NULL))

  if (!file.exists(SNAPSHOT_FILE)) {
    write_csv(df, SNAPSHOT_FILE)
  } else {
    write_csv(df, SNAPSHOT_FILE, append = TRUE, col_names = FALSE)
  }
  message("Appended ", nrow(df), " records at ", Sys.time())
  invisible(df)
}

if (!interactive()) append_snapshot()

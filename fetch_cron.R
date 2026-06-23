# Run this script on a cron schedule to collect snapshots.
# Installed cron entry (every 3 hours):
#   0 */3 * * * /usr/bin/Rscript /home/math-pi-2/SkyView/fetch_cron.R >> /home/math-pi-2/SkyView/logs/fetch.log 2>&1

args     <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("--file=", args, value = TRUE)
if (length(file_arg) > 0)
  setwd(dirname(normalizePath(sub("--file=", "", file_arg))))

source("R/fetch.R")

# Run this script on a cron schedule to collect snapshots.
# On the Pi, add to crontab:
#   */5 * * * * Rscript /home/pi/SkyView/fetch_cron.R >> /home/pi/SkyView/logs/fetch.log 2>&1

setwd(dirname(sys.frame(1)$ofile))  # set wd to script location
source("R/fetch.R")

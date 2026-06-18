#!/bin/bash
# Run on the Pi to set up SkyView after cloning.
# Usage: bash deploy_pi.sh

set -e
APP_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_DIR="$APP_DIR/logs"
mkdir -p "$LOG_DIR"

echo "=== Installing R packages ==="
Rscript "$APP_DIR/install_packages.R"

echo "=== Setting up cron job (every 5 minutes) ==="
CRON_LINE="*/5 * * * * Rscript $APP_DIR/fetch_cron.R >> $LOG_DIR/fetch.log 2>&1"
( crontab -l 2>/dev/null | grep -v "fetch_cron.R" ; echo "$CRON_LINE" ) | crontab -
echo "Cron added: $CRON_LINE"

echo "=== Starting Shiny app on port 3838 ==="
nohup Rscript -e "shiny::runApp('$APP_DIR', host='0.0.0.0', port=3838)" \
  >> "$LOG_DIR/shiny.log" 2>&1 &
echo "Shiny PID: $!"
echo "Access at http://$(hostname -I | awk '{print $1}'):3838  or via Tailscale IP"

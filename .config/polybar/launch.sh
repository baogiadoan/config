#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload bartop1 &
    MONITOR=$m polybar --reload barbot1 &
  done
else
  polybar --reload bartop1 &
  polybar --reload barbot1 &
fi
# Launch Polybar, using default config location ~/.config/polybar/config
#polybar bartop1 &
#polybar barbot1 &


echo "Polybar launched..."

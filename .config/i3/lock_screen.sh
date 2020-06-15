#betterlockscreen -u $(ls ~/Pictures/wallpapers/lockscreen/* | sort -R | head -n5) -r $(xrandr | grep Screen | awk '{print $8 "x" $10}') &
betterlockscreen -r 3600x1080
betterlockscreen -l dim

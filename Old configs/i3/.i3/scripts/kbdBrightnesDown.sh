file=/sys/class/leds/asus::kbd_backlight/brightness; val=`cat $file`; if [ $val -gt 0 ]; then echo `expr $val - 1` | sudo tee $file; fi

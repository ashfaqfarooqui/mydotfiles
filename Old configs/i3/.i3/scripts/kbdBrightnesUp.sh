file=/sys/class/leds/asus::kbd_backlight/brightness; val=`cat $file`; if [ $val -lt 3 ]; then echo `expr $val + 1` | sudo tee $file; fi

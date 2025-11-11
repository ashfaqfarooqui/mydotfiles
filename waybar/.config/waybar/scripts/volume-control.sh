#!/usr/bin/env bash

# Define functions
print_error() {
	cat <<"EOF"
Usage: ./volumecontrol.sh -[device] <actions>
...valid devices are...
    i   -- input device
    o   -- output device
    p   -- player application
...valid actions are...
    i   -- increase volume [+2]
    d   -- decrease volume [-2]
    m   -- mute [x]
EOF
	exit 1
}

send_notification() {
	# Get volume from wpctl (format: "Volume: 0.50" or "Volume: 0.50 [MUTED]")
	vol_raw=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $2}')
	vol=$(echo "$vol_raw * 100" | bc | cut -d'.' -f1)
	notify-send -r 91190 "Volume: ${vol}%"
}

notify_mute() {
	# Check if muted (wpctl outputs "Volume: X.XX [MUTED]" when muted)
	mute_status=$(wpctl get-volume @DEFAULT_AUDIO_SINK@)
	if echo "$mute_status" | grep -q "MUTED"; then
		notify-send -r 91190 "Muted"
	else
		notify-send -r 91190 "Unmuted"
	fi
}

action_volume() {
	case "${1}" in
	i)
		# Get current volume as decimal (0.0 to 1.5 typically)
		current_vol_raw=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $2}')
		current_vol=$(echo "$current_vol_raw * 100" | bc | cut -d'.' -f1)

		# Increase by 2% if below 100
		if [ "$current_vol" -lt 100 ]; then
			new_vol=$((current_vol + 2))
			if [ "$new_vol" -gt 100 ]; then
				new_vol=100
			fi
			# Convert to decimal for wpctl (e.g., 50% = 0.50)
			new_vol_decimal=$(echo "scale=2; $new_vol / 100" | bc)
			wpctl set-volume @DEFAULT_AUDIO_SINK@ "$new_vol_decimal"
		fi
		;;
	d)
		# Get current volume
		current_vol_raw=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $2}')
		current_vol=$(echo "$current_vol_raw * 100" | bc | cut -d'.' -f1)

		# Decrease by 2%, ensuring it doesn't drop below 0%
		new_vol=$((current_vol - 2))
		if [ "$new_vol" -lt 0 ]; then
			new_vol=0
		fi
		# Convert to decimal for wpctl
		new_vol_decimal=$(echo "scale=2; $new_vol / 100" | bc)
		wpctl set-volume @DEFAULT_AUDIO_SINK@ "$new_vol_decimal"
		;;
	esac
}

select_output() {
	if [ "$*" ]; then
		desc="$*"
		# Get device ID by description
		device_id=$(wpctl status | sed -n '/Sinks:/,/Sources:/p' | grep -F "$desc" | awk '{print $2}' | tr -d '.*│')

		if [ -n "$device_id" ] && wpctl set-default "$device_id"; then
			notify-send -r 91190 "Activated: $desc"
		else
			notify-send -r 91190 "Error activating $desc"
		fi
	else
		# List all output devices
		wpctl status | sed -n '/Sinks:/,/Sources:/p' | grep "│" | sed 's/.*│ //' | sed 's/ \[vol.*//' | sort
	fi
}

# Evaluate device option
while getopts iops: DeviceOpt; do
	case "${DeviceOpt}" in
	i)
		# Input device (source)
		nsink=$(wpctl status | sed -n '/Sources:/,/Sink endpoints:/p' | grep "│" | head -1)
		[ -z "${nsink}" ] && echo "ERROR: Input device not found..." && exit 0
		srce="--default-source"
		device_type="source"
		;;
	o)
		# Output device (sink)
		nsink=$(wpctl status | sed -n '/Sinks:/,/Sources:/p' | grep "│" | head -1)
		[ -z "${nsink}" ] && echo "ERROR: Output device not found..." && exit 0
		srce=""
		device_type="sink"
		;;
	p)
		# Player
		nsink=$(playerctl --list-all | grep -w "${OPTARG}")
		[ -z "${nsink}" ] && echo "ERROR: Player ${OPTARG} not active..." && exit 0
		srce="${nsink}"
		device_type="player"
		;;
	s)
		# Select an output device
		select_output "$@"
		exit
		;;
	*) print_error ;;
	esac
done

# Set default variables
shift $((OPTIND - 1))

# Determine which device to use based on device_type
case "${device_type}" in
source)
	DEVICE="@DEFAULT_AUDIO_SOURCE@"
	;;
*)
	DEVICE="@DEFAULT_AUDIO_SINK@"
	;;
esac

# Execute action
case "${1}" in
i) action_volume i ;;
d) action_volume d ;;
m) wpctl set-mute "$DEVICE" toggle && notify_mute && exit 0 ;;
*) print_error ;;
esac

send_notification

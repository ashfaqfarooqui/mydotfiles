#!/usr/bin/env bash

# Detect the active network interface
IFACE=$(ip route get 1.1.1.1 2>/dev/null | awk '{for(i=1;i<=NF;i++) if($i=="dev") print $(i+1)}' | head -1)

if [[ -z "$IFACE" ]]; then
    echo "{\"text\": \"↑-.--MB\\n↓-.--MB\"}"
    exit 0
fi

PREV_FILE="/tmp/waybar-netspeed-${IFACE}"
CURR_TIME=$(date +%s%N)

# Read current RX/TX bytes from /proc/net/dev
read -r CURR_RX CURR_TX <<< "$(awk -v iface="${IFACE}:" '$1==iface {print $2, $10}' /proc/net/dev)"

if [[ -f "$PREV_FILE" ]]; then
    read -r PREV_RX PREV_TX PREV_TIME < "$PREV_FILE"

    ELAPSED_NS=$(( CURR_TIME - PREV_TIME ))

    if [[ $ELAPSED_NS -gt 0 && $CURR_RX -ge $PREV_RX && $CURR_TX -ge $PREV_TX ]]; then
        OUTPUT=$(awk -v crx="$CURR_RX" -v ctx="$CURR_TX" \
                     -v prx="$PREV_RX" -v ptx="$PREV_TX" \
                     -v elapsed_ns="$ELAPSED_NS" '
        BEGIN {
            elapsed = elapsed_ns / 1e9
            rx_rate = (crx - prx) / elapsed
            tx_rate = (ctx - ptx) / elapsed
            printf "{\"text\": \"↑%.2fMB\\n↓%.2fMB\"}", tx_rate/1048576, rx_rate/1048576
        }')
        echo "$OUTPUT"
    else
        echo "{\"text\": \"↑-.--MB\\n↓-.--MB\"}"
    fi
else
    echo "{\"text\": \"↑-.--MB\\n↓-.--MB\"}"
fi

echo "${CURR_RX} ${CURR_TX} ${CURR_TIME}" > "$PREV_FILE"

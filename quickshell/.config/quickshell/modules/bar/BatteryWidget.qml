import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "battery" module.
Text {
    readonly property int pct: Math.round(Battery.percent)
    readonly property var icons: ["󰁼", "󰁽", "󰁾", "󰁿", "󰂀", "󰂁", "󰂂"]

    text: {
        const idx = Math.min(icons.length - 1, Math.floor(pct / (100 / icons.length)));
        if (pct >= 100) return "󱃌 " + pct + "%";
        if (Battery.charging) return "󱘖 " + pct + "%";
        if (pct <= 15) return "󱃍 " + pct + "%";
        if (pct <= 30) return "󰁻 " + pct + "%";
        return icons[idx] + " " + pct + "%";
    }
    color: pct <= 15 && !Battery.charging ? Theme.red : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            Battery.charging ? "Charging: " + pct + "%" : "Discharging: " + pct + "%"
        ) : TooltipBus.hide()
    }
}

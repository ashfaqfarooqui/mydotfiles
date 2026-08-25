pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs.services

// Replaces the old wttrbar shell-out with a direct Open-Meteo call (free,
// no API key — see https://open-meteo.com/en/docs, `current`/`daily`
// params) so the bar widget and a proper detail panel (ported down from
// basecamp/omarchy's shell/plugins/panels/weather, minus its location
// picker/geocoding — this repo's location is fixed, same as the old
// wttrbar --location arg) share one parsed model instead of a pre-rendered
// waybar-style string.
Singleton {
    id: root

    // Borås, Sweden — same fixed location the old `wttrbar --location
    // boras,Sweden` call used.
    readonly property real latitude: 57.7211
    readonly property real longitude: 12.9401
    readonly property string locationName: "Borås"

    property string text: "..."
    property string tooltip: ""
    property real currentTemp: NaN
    property real feelsLike: NaN
    property real humidity: NaN
    property real windSpeed: NaN
    property int weatherCode: 0
    property bool isDay: true
    property var forecast: [] // [{date, maxTemp, minTemp, weatherCode}, ...]
    property bool loaded: false

    function iconForCode(code, night) {
        switch (code) {
        case 0: return night ? "\u{F0594}" : "\u{F0599}";
        case 1: case 2: return night ? "\u{F0F31}" : "\u{F0595}";
        case 3: return "\u{F0590}";
        case 45: case 48: return "\u{F0591}";
        case 51: case 53: case 55: case 56: case 57: return "\u{F0597}";
        case 61: case 63: case 65: case 66: case 67: case 80: case 81: case 82: return "\u{F0596}";
        case 71: case 73: case 75: case 77: case 85: case 86: return "\u{F0598}";
        case 95: return "\u{F0593}";
        case 96: case 99: return "\u{F067E}";
        default: return "\u{F0599}";
        }
    }

    function descriptionForCode(code) {
        switch (code) {
        case 0: return "Clear sky";
        case 1: return "Mainly clear";
        case 2: return "Partly cloudy";
        case 3: return "Overcast";
        case 45: case 48: return "Fog";
        case 51: case 53: case 55: return "Drizzle";
        case 56: case 57: return "Freezing drizzle";
        case 61: case 63: case 65: return "Rain";
        case 66: case 67: return "Freezing rain";
        case 71: case 73: case 75: case 77: return "Snow";
        case 80: case 81: case 82: return "Rain showers";
        case 85: case 86: return "Snow showers";
        case 95: return "Thunderstorm";
        case 96: case 99: return "Thunderstorm with hail";
        default: return "";
        }
    }

    function dayLabel(dateString) {
        const d = new Date(dateString + "T12:00:00");
        if (isNaN(d.getTime())) return "";
        return d.toLocaleDateString(Qt.locale(), "ddd");
    }

    function refresh() {
        if (poll.running) return;
        poll.running = true;
    }

    Timer {
        interval: 600000 // 10 minutes — current conditions don't move fast enough to justify wttrbar's old 60s poll.
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }

    Process {
        id: poll
        command: ["curl", "-fsS", "--max-time", "8",
            "https://api.open-meteo.com/v1/forecast" +
            "?latitude=" + root.latitude +
            "&longitude=" + root.longitude +
            "&current=temperature_2m,apparent_temperature,relative_humidity_2m,wind_speed_10m,weather_code,is_day" +
            "&daily=temperature_2m_max,temperature_2m_min,weather_code" +
            "&timezone=auto&forecast_days=4"]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    const data = JSON.parse(this.text);
                    const current = data.current || {};
                    root.currentTemp = current.temperature_2m;
                    root.feelsLike = current.apparent_temperature;
                    root.humidity = current.relative_humidity_2m;
                    root.windSpeed = current.wind_speed_10m;
                    root.weatherCode = current.weather_code ?? 0;
                    root.isDay = (current.is_day ?? 1) === 1;

                    const daily = data.daily || {};
                    const days = [];
                    const todayStr = String(current.time || "").slice(0, 10);
                    for (let i = 0; i < (daily.time?.length ?? 0) && days.length < 3; i++) {
                        if (daily.time[i] === todayStr) continue;
                        days.push({
                            date: daily.time[i],
                            maxTemp: daily.temperature_2m_max?.[i],
                            minTemp: daily.temperature_2m_min?.[i],
                            weatherCode: daily.weather_code?.[i] ?? 0
                        });
                    }
                    root.forecast = days;

                    const roundedTemp = Math.round(root.currentTemp);
                    root.text = isNaN(roundedTemp) ? "…" : String(roundedTemp);
                    root.tooltip = root.locationName + " · " + root.descriptionForCode(root.weatherCode) +
                        "\nFeels like " + Math.round(root.feelsLike) + "°  ·  Humidity " + Math.round(root.humidity) + "%" +
                        "\nWind " + Math.round(root.windSpeed) + " km/h";
                    root.loaded = true;
                } catch (e) {}
            }
        }
    }

    // Panel visibility, same singleton-flag pattern as Network.qml/Bluetooth.qml.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            Network.hidePanel();
            Bluetooth.hidePanel();
            Battery.hidePanel();
            Brightness.hidePanel();
            Audio.hidePanel();
            Calendar.hidePanel();
            SystemStats.hidePanel();
            AgentsUsage.hidePanel();
            Tailscale.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}

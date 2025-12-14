# VectorWatch 🦟📡

VectorWatch is an **R Shiny** malaria surveillance dashboard that visualizes **county prevalence**, detects **LISA hotspot clusters**, and shows **trend + 3D profiles** from weekly/monthly data extracts.

Live app: https://markchweya.shinyapps.io/VectorWatch/

---

## Demo logins
- `user / user123` (viewer)
- `admin / admin123` (admin)

> Note: these credentials are hardcoded for demo purposes.

---

## What it does
- **Secure login** with roles (viewer/admin)
- **Dataset folder picker** (local device) + manual path input
- **Coverage switching**: `high`, `medium`, `low`
- **Auto-detects extract frequency**:
  - Weekly files (W01..W52) OR monthly files (01..12)
- **Interactive tabs**
  - **Map**: County prevalence heat map (Leaflet)
  - **Hotzones**: LISA clusters (High-High, Low-Low, High-Low, Low-High)
  - **Trends**: Positivity over time (Plotly)
  - **3D**: County profile scatter (Age vs Hb vs Prevalence)
- **Export**
  - Download hotspots/cluster results as CSV

---

## Dataset requirements (important)
VectorWatch expects a base dataset folder containing these subfolders:

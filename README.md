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
- **Auto-detects extract frequency**
  - Weekly files (W01..W52) OR monthly files (01..12)
- **Interactive tabs**
  - **Map**: County prevalence heat map (Leaflet)
  - **Hotzones**: LISA clusters (High-High, Low-Low, High-Low, Low-High)
  - **Trends**: Positivity over time (Plotly)
  - **3D**: County profile scatter (Mean Age vs Mean Hb vs Prevalence)
- **Export**
  - Download hotspots/cluster results as CSV

---

## Dataset requirements (important)

### Folder structure
VectorWatch expects a base dataset folder containing these subfolders:
```
<DATASET_FOLDER>/
  high/
  medium/
  low/
```

### File naming conventions
Inside each coverage folder, files must match one of these patterns:

**Weekly**
```
vectorwatch_malaria_2024_W01_<coverage>.csv
vectorwatch_malaria_2024_W02_<coverage>.csv
...
vectorwatch_malaria_2024_W52_<coverage>.csv
```

**Monthly**
```
vectorwatch_malaria_2024_01_<coverage>.csv
vectorwatch_malaria_2024_02_<coverage>.csv
...
vectorwatch_malaria_2024_12_<coverage>.csv
```

Where `<coverage>` is one of: `high`, `medium`, `low`.

---

## Expected columns (CSV schema)
Your CSVs should include (or be mappable to) the following fields:

**Required**
- `county`
- `date` (convertible to `Date`)
- `parasite_detected` (1 = positive, 0 = negative)  
  - The app also tries to map common alternatives like `positive`, `pos`, `result`, `malaria_positive`.

**Optional (enables extra insights)**
- `age` (years)
- `hemoglobin_g_dl` (or `hb`, `hgb`, `hemoglobin`)
- `wbc_cells_ul` (or `wbc`, `wbc_ul`, `white_blood_cells`)

The app standardizes column names and attempts to auto-map common variations.

---

## Spatial layer & hotspot analytics
- Kenya county boundaries are fetched via: `geodata::gadm("KEN", level = 1)`
- Data is joined to polygons using a normalized county key (handles punctuation/spacing differences).
- Hotzones are computed with:
  - Global Moran’s I (spatial autocorrelation)
  - Local Moran’s I (LISA) to label clusters:
    - **High-High**, **Low-Low**, **High-Low**, **Low-High**, **Not significant**

---

## How it calculates key metrics
- **Tests** = total rows in selected files
- **Cases** = sum of `parasite_detected == 1`
- **Prevalence (%)** = `100 * cases / tests`
- County summaries also include:
  - mean age, mean Hb, mean WBC (if available)
- Filters:
  - Minimum tests per county (`min_n`)
  - Cluster category filter (All or specific LISA cluster types)

---

## Tech stack
Core packages used:
- **shiny**, **bslib**, **htmltools**, **shinyjs**
- **dplyr**, **janitor**, **stringr**, **lubridate**
- **leaflet**, **plotly**
- **sf**, **spdep**, **geodata**
- **digest** (password hashing)
- **shinyFiles** (folder picker)

---

## Run locally

### 1) Install dependencies
```r
install.packages(c(
  "shiny","bslib","htmltools","dplyr","janitor","stringr","lubridate",
  "digest","leaflet","plotly","shinyFiles","shinyjs",
  "sf","spdep","geodata"
), dependencies = TRUE)
```

### 2) Run the app
From the folder containing your app file:
```r
shiny::runApp()
```

> Tip: If you run into `sf` / spatial library install issues, install system dependencies for your OS first (GDAL/GEOS/PROJ), then reinstall `sf`.

---

## Security note (important)
This project includes demo credentials for convenience. If you publish the source code:
- Remove hardcoded users/passwords
- Use environment variables or an external authentication provider
- Never commit real credentials to GitHub

---

## Deployment
Deployed on **shinyapps.io**:
https://markchweya.shinyapps.io/VectorWatch/

---

## Author
Mark Chweya

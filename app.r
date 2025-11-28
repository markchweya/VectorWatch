# app.R — VectorWatch v2 (fixed ESDA + glass UI + collapsible sidebar + folder picker)
# Login:
#   user  / user123
#   admin / admin123
#
# You DO NOT paste 52 CSV paths.
# You point the app to ONE base folder that contains:
#   high/   medium/   low/
# And files inside like:
#   vectorwatch_malaria_2024_W10_high.csv   OR   vectorwatch_malaria_2024_03_high.csv
#
# This v2 fixes:
# - "Error: non-positive number of entities" (was caused by county name mismatch / empty map data)
# - Map not coloring / LISA not showing
# - 3D plot failing
# - White chart backgrounds (now transparent, glass glow)
# - Sidebar can collapse (desktop) / become slide-over (mobile)
# - Folder picker (browse) so user can select the dataset folder

install_if_missing <- function(pkgs){
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(miss)) install.packages(miss, dependencies = TRUE)
}

pkgs <- c("shiny","bslib","htmltools","dplyr","janitor","stringr",
          "sf","spdep","leaflet","plotly","digest","lubridate",
          "geodata","terra","shinyFiles")
install_if_missing(pkgs)

library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(janitor)
library(stringr)
library(sf)
library(spdep)
library(leaflet)
library(plotly)
library(digest)
library(lubridate)
library(shinyFiles)

favicon_data_uri <- function(emoji = "🦟"){
  svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>",
                "<text y='.9em' font-size='90'>", emoji, "</text></svg>")
  paste0("data:image/svg+xml,", URLencode(svg, reserved = TRUE))
}

hash_pw <- function(x) digest(x, algo = "sha256")
ACCOUNTS <- data.frame(
  user = c("user","admin"),
  pass_hash = c(hash_pw("user123"), hash_pw("admin123")),
  role = c("viewer","admin"),
  stringsAsFactors = FALSE
)

default_data_dir <- function(){
  dl <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  p_week <- file.path(dl, "vectorwatch_weekly_synthetic")
  p_mon  <- file.path(dl, "vectorwatch_synthetic")
  if (dir.exists(p_week)) return(normalizePath(p_week, winslash = "/", mustWork = FALSE))
  normalizePath(p_mon, winslash = "/", mustWork = FALSE)
}

norm_county <- function(x){
  x <- toupper(trimws(as.character(x)))
  x <- str_replace_all(x, "&", "AND")
  x <- str_replace_all(x, "[-’'`]", " ")
  x <- str_replace_all(x, "[^A-Z ]", " ")
  str_squish(x)
}

get_kenya_adm1 <- function(){
  tryCatch({
    adm <- geodata::gadm("KEN", level = 1, path = tempdir())
    kenya_sf <- sf::st_as_sf(adm) |> sf::st_make_valid()
    name_col <- intersect(names(kenya_sf), c("NAME_1","shapeName","name"))[1]
    kenya_sf |>
      rename(county = all_of(name_col)) |>
      mutate(
        county = str_squish(as.character(county)),
        county_key = norm_county(county)
      )
  }, error = function(e) NULL)
}

detect_mode <- function(data_dir, coverage){
  folder <- file.path(data_dir, coverage)
  if(!dir.exists(folder)) return(list(mode="none", hint="Coverage folder missing."))

  f <- list.files(folder, full.names = FALSE)

  weekly_pat  <- "^vectorwatch_malaria_2024_W\\d{2}_.+\\.csv$"
  monthly_pat <- "^vectorwatch_malaria_2024_\\d{2}_.+\\.csv$"

  has_weekly  <- any(grepl(weekly_pat, f, ignore.case = TRUE))
  has_monthly <- any(grepl(monthly_pat, f, ignore.case = TRUE)) && !has_weekly

  if(has_weekly) return(list(mode="weekly", hint="Detected weekly extracts (W01..W52)."))
  if(has_monthly) return(list(mode="monthly", hint="Detected monthly extracts (01..12)."))

  list(mode="none", hint="No matching VectorWatch CSVs found in this coverage folder.")
}

read_data <- function(data_dir, coverage, mode, time_vals){
  folder <- file.path(data_dir, coverage)

  files <- if (mode == "weekly"){
    file.path(folder, sprintf("vectorwatch_malaria_2024_W%02d_%s.csv", time_vals, coverage))
  } else {
    file.path(folder, sprintf("vectorwatch_malaria_2024_%02d_%s.csv", time_vals, coverage))
  }

  files <- files[file.exists(files)]
  if(length(files) == 0) return(NULL)

  df <- do.call(rbind, lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE)))
  df <- df |>
    clean_names() |>
    mutate(
      date = as.Date(date),
      county_key = norm_county(county),
      iso_week = ifelse("iso_week" %in% names(.),
                        as.integer(iso_week),
                        as.integer(lubridate::isoweek(date)))
    )

  df
}

make_county_summary <- function(df){
  df |>
    group_by(county_key) |>
    summarise(
      n_tests = n(),
      malaria_cases = sum(parasite_detected == 1, na.rm = TRUE),
      prevalence = 100 * malaria_cases / n_tests,
      mean_age = mean(age, na.rm = TRUE),
      mean_hb  = mean(hemoglobin_g_dl, na.rm = TRUE),
      mean_wbc = mean(wbc_cells_ul, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(prevalence))
}

esda_on_polygons <- function(kenya_joined){
  km <- kenya_joined |> filter(!is.na(prevalence))

  if(nrow(km) < 3){
    return(list(ok = FALSE, msg = "Not enough counties left for ESDA. Lower 'Minimum tests', or select more weeks/months.", km = km))
  }

  nb <- spdep::poly2nb(km, queen = TRUE)
  if(length(nb) == 0){
    return(list(ok = FALSE, msg = "Neighbour list is empty (polygons issue).", km = km))
  }

  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  mor <- tryCatch(spdep::moran.test(km$prevalence, listw = lw, zero.policy = TRUE),
                  error = function(e) NULL)

  lisa <- tryCatch(spdep::localmoran(km$prevalence, lw, zero.policy = TRUE),
                   error = function(e) NULL)

  if(is.null(mor) || is.null(lisa)){
    return(list(ok = FALSE, msg = "ESDA failed to compute (try selecting more weeks/months).", km = km))
  }

  km$Ii   <- lisa[, 1]
  km$p_Ii <- lisa[, 5]

  z <- as.numeric(scale(km$prevalence))
  lag_z <- spdep::lag.listw(lw, z, zero.policy = TRUE)

  km$cluster <- "Not significant"
  sig <- km$p_Ii <= 0.05
  km$cluster[sig & z > 0 & lag_z > 0] <- "High-High"
  km$cluster[sig & z < 0 & lag_z < 0] <- "Low-Low"
  km$cluster[sig & z > 0 & lag_z < 0] <- "High-Low"
  km$cluster[sig & z < 0 & lag_z > 0] <- "Low-High"

  list(ok = TRUE, km = km, moran = mor)
}

theme_vw <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = font_google("Plus Jakarta Sans"),
  heading_font = font_google("Sora")
)

ui <- fluidPage(
  theme = theme_vw,
  tags$head(
    tags$link(rel = "icon", href = favicon_data_uri("🦟")),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root{
        --bg0:#050A14; --bg1:#070D1D;
        --glass: rgba(255,255,255,.06);
        --stroke: rgba(255,255,255,.10);
        --muted: rgba(255,255,255,.66);
        --text: rgba(255,255,255,.92);
        --teal: #4AF2D6; --blue: #4C78FF; --violet:#B56CFF; --amber:#FFC857;
        --shadow: 0 14px 40px rgba(0,0,0,.35);
        --r: 20px;
      }

      html, body { height: 100%; }
      body{
        overflow: hidden;
        background:
          radial-gradient(1200px 650px at 12% 8%, rgba(76,120,255,.18), transparent 55%),
          radial-gradient(900px 520px at 86% 22%, rgba(74,242,214,.14), transparent 55%),
          radial-gradient(1000px 540px at 60% 100%, rgba(181,108,255,.10), transparent 65%),
          linear-gradient(180deg, var(--bg0), var(--bg1));
      }

      .vw-wrap{ height: 100vh; display: flex; flex-direction: column; }
      .vw-topbar{
        height: 64px; display: flex; align-items: center; gap: .9rem; padding: 0 1rem;
        border-bottom: 1px solid var(--stroke);
        backdrop-filter: blur(14px);
        background: rgba(5,10,20,.62);
      }
      .vw-dot{
        width: 10px; height: 10px; border-radius: 999px;
        background: linear-gradient(135deg, var(--teal), var(--blue));
        box-shadow: 0 0 18px rgba(74,242,214,.35);
      }
      .vw-brand h1{ font-size: 1.08rem; margin: 0; line-height: 1.1; font-weight: 800; }
      .vw-brand .tag{ margin: 0; font-size: .9rem; color: var(--muted); }

      .vw-shell{ flex: 1; display: flex; min-height: 0; }
      .vw-sidebar{
        width: 330px; padding: 1rem; border-right: 1px solid var(--stroke);
        background: rgba(255,255,255,.03); backdrop-filter: blur(14px);
        transition: transform .28s ease, width .28s ease, opacity .28s ease;
      }
      .vw-main{ flex: 1; min-width: 0; padding: 1rem; overflow: hidden; }

      .vw-collapsed .vw-sidebar{ width: 0 !important; padding: 0 !important; opacity: 0; transform: translateX(-10px); border-right:none; }

      @media (max-width: 992px){
        body{ overflow: auto; }
        .vw-shell{ flex-direction: column; }
        .vw-sidebar{ width: 100%; border-right:none; border-bottom:1px solid var(--stroke); }
        .vw-main{ overflow: visible; }
      }

      .vw-card{
        border: 1px solid var(--stroke);
        background: var(--glass);
        border-radius: var(--r);
        box-shadow: var(--shadow);
      }
      .vw-pad{ padding: 1rem; }

      .vw-glow{ position: relative; overflow: hidden; }
      .vw-glow:before{
        content:''; position:absolute; inset:-1px; border-radius: var(--r); padding: 1px;
        background: linear-gradient(135deg, rgba(74,242,214,.55), rgba(76,120,255,.55), rgba(181,108,255,.35));
        -webkit-mask: linear-gradient(#000 0 0) content-box, linear-gradient(#000 0 0);
        -webkit-mask-composite: xor; mask-composite: exclude;
        opacity: .55; pointer-events:none;
      }
      .vw-glow:hover:before{ opacity: .85; }

      .vw-appear{ animation: vwIn .35s ease-out both; }
      @keyframes vwIn { from { opacity:0; transform: translateY(6px);} to { opacity:1; transform: translateY(0);} }
      .tab-pane{ animation: vwIn .28s ease-out both; }

      .form-control, .selectize-input, .selectize-dropdown, .btn{ border-radius: 14px !important; }
      .form-control, .selectize-input{
        background: rgba(255,255,255,.06) !important;
        border: 1px solid rgba(255,255,255,.12) !important;
        color: var(--text) !important;
      }
      .selectize-dropdown{ background: rgba(10,14,26,.95) !important; border: 1px solid rgba(255,255,255,.12) !important; }
      .selectize-dropdown .active{ background: rgba(74,242,214,.18) !important; }
      label{ color: rgba(255,255,255,.72) !important; }

      .nav-pills .nav-link{ border: 1px solid rgba(255,255,255,.10) !important; background: rgba(255,255,255,.04) !important; margin-right: .4rem; }
      .nav-pills .nav-link.active{ background: rgba(74,242,214,.14) !important; border-color: rgba(74,242,214,.30) !important; }

      .leaflet-control{ background: rgba(10,14,26,.72) !important; backdrop-filter: blur(12px); border: 1px solid rgba(255,255,255,.12) !important; color: rgba(255,255,255,.88) !important; border-radius: 14px !important; box-shadow: 0 12px 34px rgba(0,0,0,.35); }
      .leaflet-control-attribution{ background: transparent !important; color: rgba(255,255,255,.45) !important; }

      .vw-kpi .k{ color: var(--muted); font-size: .9rem; }
      .vw-kpi .v{ font-size: 1.6rem; font-weight: 800; }
      .vw-kpi .sub{ color: rgba(255,255,255,.55); font-size:.88rem; }

      .vw-out{ border-radius: var(--r); overflow: hidden; }

      .vw-ghost{ background: rgba(255,255,255,.06) !important; border: 1px solid rgba(255,255,255,.14) !important; color: rgba(255,255,255,.88) !important; }
      .vw-danger{ color: #ff6b6b; font-weight: 700; }

      .vw-scroll{ overflow: auto; max-height: calc(100vh - 64px - 2rem); }
      .vw-scroll::-webkit-scrollbar{ width: 10px; height:10px;}
      .vw-scroll::-webkit-scrollbar-thumb{ background: rgba(255,255,255,.12); border-radius: 99px;}
      .vw-scroll::-webkit-scrollbar-track{ background: transparent;}
    ")),
    tags$script(HTML("
      document.addEventListener('click', (e) => {
        if(e.target && e.target.id === 'vwToggleSidebar'){
          const root = document.getElementById('vwRoot');
          if(root) root.classList.toggle('vw-collapsed');
        }
      });
    "))
  ),

  div(id="vwRoot", class="vw-wrap",
    div(class="vw-topbar",
      div(class="vw-dot"),
      div(class="vw-brand",
        tags$h1("VectorWatch"),
        tags$p(class="tag","Malaria surveillance • Hotspots • Early signal • Targeted action")
      ),
      div(style="margin-left:auto; display:flex; gap:.5rem; align-items:center;",
        actionButton("toggleSidebarBtn","☰", class="vw-ghost", onclick="document.getElementById('vwToggleSidebar').click();"),
        tags$button(id="vwToggleSidebar", type="button", style="display:none;", ""),
        uiOutput("whoami_ui"),
        actionButton("logout", "Logout", class="vw-ghost")
      )
    ),

    div(class="vw-shell",
      div(class="vw-sidebar vw-scroll", uiOutput("sidebar_ui")),
      div(class="vw-main vw-scroll", uiOutput("main_ui"))
    )
  )
)

server <- function(input, output, session){

  rv <- reactiveValues(
    authed = FALSE,
    user = NULL,
    role = NULL,
    data_dir = default_data_dir(),
    kenya_sf = NULL
  )

  volumes <- shinyFiles::getVolumes()()
  shinyDirChoose(input, "dir", roots = volumes, session = session)

  observe({ rv$kenya_sf <- get_kenya_adm1() })

  tries <- reactiveVal(0)

  output$whoami_ui <- renderUI({
    if (!isTRUE(rv$authed)) return(span(style="opacity:.7;color:rgba(255,255,255,.75);", "Not signed in"))
    span(style="opacity:.9;color:rgba(255,255,255,.85);",
         paste0("Signed in: ", rv$user, " • ", rv$role))
  })

  login_ui <- function(){
    div(class="vw-card vw-pad vw-glow vw-appear", style="max-width: 560px; margin: 4vh auto;",
      tags$h3(style="margin:.1rem 0 .2rem;font-weight:900;", "Secure Access 🦟"),
      tags$p(style="margin:0 0 1rem;color:rgba(255,255,255,.70);",
             "Sign in to access hotspot intelligence and operational planning."),
      textInput("login_user", "Username", placeholder = "user"),
      passwordInput("login_pass", "Password", placeholder = "••••••••"),
      div(style="display:flex; gap:.6rem; align-items:center;",
          actionButton("login_btn", "Login", class="btn btn-primary"),
          span(style="color:rgba(255,255,255,.55); font-size:.92rem;", "Demo: user/user123 • admin/admin123")
      ),
      uiOutput("login_msg")
    )
  }

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pass %||% ""

    if (tries() >= 5) {
      output$login_msg <- renderUI(div(style="margin-top:.8rem;color:#FFC857;",
                                      "Too many attempts. Refresh to try again."))
      return()
    }

    row <- ACCOUNTS |> filter(user == u)
    ok <- nrow(row) == 1 && identical(row$pass_hash[[1]], hash_pw(p))

    if (ok) {
      rv$authed <- TRUE
      rv$user <- u
      rv$role <- row$role[[1]]
      tries(0)
      output$login_msg <- renderUI(NULL)
    } else {
      tries(tries() + 1)
      output$login_msg <- renderUI(div(style="margin-top:.8rem;", span(class="vw-danger", paste0("Login failed (", tries(), "/5)"))))
    }
  })

  observeEvent(input$logout, {
    rv$authed <- FALSE
    rv$user <- NULL
    rv$role <- NULL
  })

  observeEvent(input$dir, {
    path <- shinyFiles::parseDirPath(volumes, input$dir)
    if(length(path) && dir.exists(path)) rv$data_dir <- normalizePath(path, winslash="/", mustWork=FALSE)
  })

  output$sidebar_ui <- renderUI({
    if (!isTRUE(rv$authed)) return(NULL)

    tagList(
      div(class="vw-card vw-pad vw-glow vw-appear",
        tags$h4(style="margin:0 0 .8rem; font-weight:900;", "Controls"),
        textInput("data_dir", "Dataset location", value = rv$data_dir),
        shinyDirButton("dir", "Pick folder on this device", "Choose folder"),
        tags$div(style="margin-top:.5rem; color:rgba(255,255,255,.65); font-size:.92rem;",
                 "Pick the folder that contains high/, medium/, low/."),
        tags$hr(style="border-color: rgba(255,255,255,.10);"),

        selectInput("coverage", "Coverage", choices = c("high","medium","low"), selected = "high"),
        uiOutput("mode_hint"),
        uiOutput("time_selector"),

        tags$hr(style="border-color: rgba(255,255,255,.10);"),
        sliderInput("min_n", "Minimum tests per county", min = 100, max = 10000, value = 500, step = 100),
        selectInput("cluster_filter", "Show clusters", choices = c("All","High-High","Low-Low","High-Low","Low-High","Not significant"), selected = "All"),

        tags$hr(style="border-color: rgba(255,255,255,.10);"),
        downloadButton("dl_hotspots", "Download hotspots (CSV)", class="vw-ghost")
      )
    )
  })

  observeEvent(input$data_dir, {
    rv$data_dir <- input$data_dir
  }, ignoreInit = TRUE)

  output$main_ui <- renderUI({
    if (!isTRUE(rv$authed)) return(login_ui())

    tabsetPanel(
      type = "pills",

      tabPanel("Command Center",
        fluidRow(
          column(3, div(class="vw-card vw-pad vw-glow vw-kpi vw-appear", uiOutput("kpi_tests"))),
          column(3, div(class="vw-card vw-pad vw-glow vw-kpi vw-appear", uiOutput("kpi_pos"))),
          column(3, div(class="vw-card vw-pad vw-glow vw-kpi vw-appear", uiOutput("kpi_hotspots"))),
          column(3, div(class="vw-card vw-pad vw-glow vw-kpi vw-appear", uiOutput("kpi_moran")))
        ),
        br(),
        fluidRow(
          column(7, div(class="vw-card vw-pad vw-glow vw-appear vw-out", tags$h4("Prevalence map"), leafletOutput("map", height = 440))),
          column(5, div(class="vw-card vw-pad vw-glow vw-appear", tags$h4("Priority queue"), uiOutput("data_status"), tableOutput("priority_table")))
        )
      ),

      tabPanel("Hotzones (LISA)",
        fluidRow(
          column(7, div(class="vw-card vw-pad vw-glow vw-appear vw-out", tags$h4("LISA cluster map"), leafletOutput("map_lisa", height = 500))),
          column(5, div(class="vw-card vw-pad vw-glow vw-appear", tags$h4("Hotspot list"), tableOutput("hotspot_table")))
        )
      ),

      tabPanel("Trends",
        fluidRow(
          column(4, div(class="vw-card vw-pad vw-glow vw-appear",
                        tags$h4("Weekly trend"),
                        selectInput("trend_county", "County", choices = character(0)))),
          column(8, div(class="vw-card vw-pad vw-glow vw-appear vw-out",
                        plotlyOutput("trend_plot", height = 450)))
        )
      ),

      tabPanel("3D Insights",
        div(class="vw-card vw-pad vw-glow vw-appear vw-out",
          tags$h4("3D county profile"),
          tags$p(style="margin-top:-.2rem;color:rgba(255,255,255,.65);",
                 "Rotate/zoom. X = mean age, Y = mean Hb, Z = prevalence (%)."),
          plotlyOutput("plot3d", height = 560)
        )
      )
    )
  })

  mode_info <- reactive({
    req(rv$authed)
    req(input$coverage)
    detect_mode(rv$data_dir, input$coverage)
  })

  output$mode_hint <- renderUI({
    mi <- mode_info()
    div(style="margin-top:.35rem; font-size:.92rem; color:rgba(255,255,255,.70);",
        paste0("Auto-detect: ", mi$hint))
  })

  output$time_selector <- renderUI({
    mi <- mode_info()
    if(mi$mode == "weekly"){
      selectizeInput("time_vals", "ISO weeks (2024)", choices = 1:52,
                     selected = c(10,11,12,13,14,15), multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    } else {
      selectizeInput("time_vals", "Months (2024)", choices = 1:12,
                     selected = c(3,4,5), multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    }
  })

  df_raw <- reactive({
    req(rv$authed)
    mi <- mode_info()
    req(input$time_vals)

    validate(need(mi$mode != "none",
      paste0("No VectorWatch CSVs detected.\n\n",
             "Folder must contain: high/, medium/, low/\n",
             "Example weekly: vectorwatch_malaria_2024_W10_high.csv\n",
             "Example monthly: vectorwatch_malaria_2024_03_high.csv")
    ))

    df <- read_data(rv$data_dir, input$coverage, mi$mode, as.integer(input$time_vals))
    validate(need(!is.null(df), "No matching files for your selection. Try different weeks/months or confirm folder."))
    df
  })

  county_summary <- reactive({
    make_county_summary(df_raw()) |> filter(n_tests >= input$min_n)
  })

  observeEvent(df_raw(), {
    d <- df_raw() |> distinct(county) |> arrange(county)
    updateSelectInput(session, "trend_county", choices = d$county, selected = d$county[1])
  }, ignoreInit = TRUE)

  kenya_joined <- reactive({
    req(rv$kenya_sf)
    rv$kenya_sf |> left_join(county_summary(), by = "county_key")
  })

  esda <- reactive({
    req(rv$kenya_sf)
    esda_on_polygons(kenya_joined())
  })

  output$kpi_tests <- renderUI({
    div(class="vw-kpi", div(class="k","Records loaded"),
        div(class="v", format(nrow(df_raw()), big.mark=",")),
        div(class="sub", paste0("Coverage: ", input$coverage)))
  })

  output$kpi_pos <- renderUI({
    r <- mean(df_raw()$parasite_detected == 1, na.rm = TRUE)
    div(class="vw-kpi", div(class="k","Positivity rate"),
        div(class="v", paste0(round(100*r,1), "%")),
        div(class="sub", paste0("Selected: ", paste(input$time_vals, collapse=", "))))
  })

  output$kpi_hotspots <- renderUI({
    e <- esda()
    if(!isTRUE(e$ok)) return(div(class="vw-kpi", div(class="k","High–High hotspots"), div(class="v","—"), div(class="sub", e$msg)))
    hh <- sum(e$km$cluster == "High-High", na.rm = TRUE)
    div(class="vw-kpi", div(class="k","High–High hotspots"), div(class="v", hh), div(class="sub","p ≤ 0.05"))
  })

  output$kpi_moran <- renderUI({
    e <- esda()
    if(!isTRUE(e$ok)) return(div(class="vw-kpi", div(class="k","Global Moran’s I"), div(class="v","—"), div(class="sub", e$msg)))
    mor <- e$moran
    I <- unname(mor$estimate[["Moran I statistic"]])
    p <- mor$p.value
    div(class="vw-kpi", div(class="k","Global Moran’s I (p)"), div(class="v", paste0(round(I,3))),
        div(class="sub", paste0("p=", signif(p,3))))
  })

  output$data_status <- renderUI({
    mi <- mode_info()
    div(style="margin:.5rem 0; padding:.6rem .75rem; border-radius: 16px;
              border:1px solid rgba(74,242,214,.22); background: rgba(74,242,214,.08);
              color: rgba(255,255,255,.85);",
        strong("Using: "), rv$data_dir, br(),
        span(style="opacity:.85;", paste0("Coverage: ", input$coverage, " • Mode: ", mi$mode, " • Selected: ", paste(input$time_vals, collapse=', '))))
  })

  output$priority_table <- renderTable({
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))
    km <- e$km |> st_drop_geometry()

    if (input$cluster_filter != "All") km <- km |> filter(cluster == input$cluster_filter)

    km |>
      mutate(priority = case_when(
        cluster == "High-High" ~ 1L,
        cluster == "High-Low"  ~ 2L,
        cluster == "Low-High"  ~ 3L,
        cluster == "Low-Low"   ~ 4L,
        TRUE                   ~ 9L
      )) |>
      arrange(priority, desc(prevalence)) |>
      transmute(county, cluster, n_tests, prevalence = round(prevalence,2), p = signif(p_Ii,3)) |>
      head(10)
  }, striped = TRUE)

  output$hotspot_table <- renderTable({
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))
    e$km |>
      st_drop_geometry() |>
      filter(p_Ii <= 0.05, cluster != "Not significant") |>
      arrange(factor(cluster, levels=c("High-High","High-Low","Low-High","Low-Low")), desc(prevalence)) |>
      transmute(county, cluster, n_tests, prevalence = round(prevalence,2), Ii = round(Ii,3), p = signif(p_Ii,3)) |>
      head(25)
  }, striped = TRUE)

  output$map <- renderLeaflet({
    req(rv$kenya_sf)
    km <- kenya_joined()
    pal <- colorNumeric("viridis", domain = km$prevalence, na.color = "transparent")

    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color = "rgba(255,255,255,.16)", weight = 1, opacity = 1,
        fillColor = ~pal(prevalence), fillOpacity = 0.78,
        label = ~paste0("<b>", county, "</b><br/>Tests: ", ifelse(is.na(n_tests),"—",n_tests),
                        "<br/>Prevalence: ", ifelse(is.na(prevalence),"—",round(prevalence,1)),"%") |> lapply(HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#4AF2D6", fillOpacity = 0.92, bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = pal, values = ~prevalence, title = "Prevalence (%)", opacity = 1)
  })

  output$map_lisa <- renderLeaflet({
    req(rv$kenya_sf)
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))
    km <- e$km

    lisa_pal <- colorFactor(
      palette = c(
        "High-High" = "#FF4D6D",
        "Low-Low" = "#4AF2D6",
        "High-Low" = "#FFC857",
        "Low-High" = "#4C78FF",
        "Not significant" = "rgba(255,255,255,.08)"
      ),
      domain = c("High-High","Low-Low","High-Low","Low-High","Not significant")
    )

    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color = "rgba(255,255,255,.16)", weight = 1, opacity = 1,
        fillColor = ~lisa_pal(cluster), fillOpacity = 0.80,
        label = ~paste0("<b>", county, "</b><br/>Cluster: ", cluster,
                        "<br/>Prev: ", round(prevalence,1), "%<br/>p: ", signif(p_Ii,3)) |> lapply(HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#4AF2D6", fillOpacity = 0.92, bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = lisa_pal, values = ~cluster, title = "LISA cluster", opacity = 1)
  })

  output$trend_plot <- renderPlotly({
    req(input$trend_county)

    wk <- df_raw() |>
      filter(county == input$trend_county) |>
      group_by(iso_week) |>
      summarise(
        tests = n(),
        cases = sum(parasite_detected == 1, na.rm = TRUE),
        positivity = 100 * cases / tests,
        .groups = "drop"
      ) |>
      arrange(iso_week)

    plot_ly(wk, x = ~iso_week, y = ~positivity, type = "scatter", mode = "lines+markers",
            hovertemplate = "Week: %{x}<br>Positivity: %{y:.1f}%<extra></extra>",
            line = list(width = 3)) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(color = "rgba(255,255,255,.88)"),
        xaxis = list(title = "ISO week", gridcolor = "rgba(255,255,255,.08)"),
        yaxis = list(title = "Positivity (%)", gridcolor = "rgba(255,255,255,.08)"),
        margin = list(l=55, r=15, b=45, t=15)
      )
  })

  output$plot3d <- renderPlotly({
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))

    km <- e$km |>
      st_drop_geometry() |>
      filter(!is.na(mean_age), !is.na(mean_hb), !is.na(prevalence))

    validate(need(nrow(km) > 3, "Not enough counties for 3D plot. Lower the filter or select more weeks/months."))

    plot_ly(
      data = km,
      x = ~mean_age, y = ~mean_hb, z = ~prevalence,
      type = "scatter3d", mode = "markers",
      color = ~cluster,
      marker = list(size = 4, opacity = 0.9),
      text = ~paste0("<b>", county, "</b><br>Prev: ", round(prevalence,1), "%<br>Tests: ", n_tests, "<br>Cluster: ", cluster),
      hoverinfo = "text"
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "rgba(255,255,255,.88)"),
        scene = list(
          bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Mean age", gridcolor = "rgba(255,255,255,.08)"),
          yaxis = list(title = "Mean Hb (g/dL)", gridcolor = "rgba(255,255,255,.08)"),
          zaxis = list(title = "Prevalence (%)", gridcolor = "rgba(255,255,255,.08)")
        ),
        margin = list(l=0, r=0, b=0, t=0)
      )
  })

  output$dl_hotspots <- downloadHandler(
    filename = function(){ paste0("vectorwatch_hotspots_", Sys.Date(), ".csv") },
    content = function(file){
      e <- esda()
      if(!isTRUE(e$ok)){
        writeLines("ESDA not available for current selection. Select more weeks/months or lower the filter.", file)
        return()
      }
      km <- e$km |> st_drop_geometry()
      out <- km |> select(county, cluster, n_tests, malaria_cases, prevalence, Ii, p_Ii) |> arrange(desc(prevalence))
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

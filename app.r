# app.R — VectorWatch (Smart loader: weekly OR monthly extracts)
# Login:
#   user  / user123
#   admin / admin123
#
# What this app does:
# - You point it to a folder that contains:
#     high/ , medium/ , low/
# - It AUTO-detects whether the files are WEEKLY (W01..W52) or MONTHLY (01..12)
# - So you don't need to paste every CSV path (just ONE base folder path).

install_if_missing <- function(pkgs){
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(miss)) install.packages(miss, dependencies = TRUE)
}

pkgs <- c("shiny","bslib","htmltools","dplyr","janitor","stringr",
          "sf","spdep","leaflet","plotly","digest","lubridate",
          "geodata","terra")
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
  # Try weekly first, then monthly
  p1 <- file.path(dl, "vectorwatch_weekly_synthetic")
  p2 <- file.path(dl, "vectorwatch_synthetic")
  if (dir.exists(p1)) return(normalizePath(p1, winslash="/", mustWork=FALSE))
  normalizePath(p2, winslash="/", mustWork=FALSE)
}

get_kenya_adm1 <- function(){
  tryCatch({
    adm <- geodata::gadm("KEN", level = 1, path = tempdir())
    kenya_sf <- sf::st_as_sf(adm) |> sf::st_make_valid()
    name_col <- intersect(names(kenya_sf), c("NAME_1","shapeName","name"))[1]
    kenya_sf |>
      rename(county = all_of(name_col)) |>
      mutate(county = str_squish(as.character(county)))
  }, error = function(e) NULL)
}

detect_mode <- function(data_dir, coverage){
  folder <- file.path(data_dir, coverage)
  if(!dir.exists(folder)) return(list(mode="none", hint="Coverage folder missing."))

  f <- list.files(folder, full.names = FALSE)

  weekly_pat <- "^vectorwatch_malaria_2024_W\\d{2}_.+\\.csv$"
  monthly_pat <- "^vectorwatch_malaria_2024_\\d{2}_.+\\.csv$"

  has_weekly <- any(grepl(weekly_pat, f, ignore.case = TRUE))
  has_monthly <- any(grepl(monthly_pat, f, ignore.case = TRUE)) && !has_weekly

  if(has_weekly) return(list(mode="weekly", hint="Detected weekly extracts (W01..W52)."))
  if(has_monthly) return(list(mode="monthly", hint="Detected monthly extracts (01..12)."))

  list(mode="none", hint="No matching VectorWatch CSVs found in this coverage folder.")
}

read_data <- function(data_dir, coverage, mode, time_vals){
  folder <- file.path(data_dir, coverage)

  if (mode == "weekly") {
    files <- file.path(folder, sprintf("vectorwatch_malaria_2024_W%02d_%s.csv", time_vals, coverage))
  } else {
    files <- file.path(folder, sprintf("vectorwatch_malaria_2024_%02d_%s.csv", time_vals, coverage))
  }
  files <- files[file.exists(files)]
  if(length(files) == 0) return(NULL)

  df <- do.call(rbind, lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE)))
  df <- df |> clean_names() |> mutate(date = as.Date(date))

  # If monthly files: add iso_week for trend charts
  if(!"iso_week" %in% names(df)){
    df <- df |> mutate(iso_week = isoweek(date))
  }
  df
}

make_county_summary <- function(df){
  df |>
    group_by(county) |>
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
  nb <- spdep::poly2nb(km, queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  mor <- spdep::moran.test(km$prevalence, listw = lw, zero.policy = TRUE)

  lisa <- spdep::localmoran(km$prevalence, lw, zero.policy = TRUE)
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

  list(km = km, moran = mor)
}

theme_vw <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = font_google("Inter"),
  heading_font = font_google("Space Grotesk")
)

ui <- fluidPage(
  theme = theme_vw,
  tags$head(
    tags$link(rel = "icon", href = favicon_data_uri("🦟")),
    tags$style(HTML("
      :root{
        --bg0:#070B14; --bg1:#0B1220;
        --card: rgba(255,255,255,.06);
        --stroke: rgba(255,255,255,.10);
        --muted: rgba(255,255,255,.67);
        --teal: #38E6D6; --blue: #2B6CFF; --amber:#FFC857;
      }
      body{
        background:
          radial-gradient(1200px 600px at 20% 10%, rgba(43,108,255,.18), transparent 55%),
          radial-gradient(900px 500px at 80% 20%, rgba(56,230,214,.14), transparent 55%),
          linear-gradient(180deg, var(--bg0), var(--bg1));
      }
      .vw-topbar{
        position: sticky; top: 0; z-index: 20;
        backdrop-filter: blur(14px);
        background: rgba(7,11,20,.65);
        border-bottom: 1px solid var(--stroke);
      }
      .vw-brand{ display:flex; gap:.65rem; align-items:center; padding: .9rem 1.1rem; }
      .vw-dot{
        width: 10px; height: 10px; border-radius: 999px;
        background: linear-gradient(135deg, var(--teal), var(--blue));
        box-shadow: 0 0 14px rgba(56,230,214,.35);
      }
      .vw-title{ font-weight: 800; letter-spacing: .2px; }
      .vw-tag{ color: var(--muted); font-size: .92rem; margin-top: .12rem; }
      .vw-fadein{ animation: vwFadeIn .35s ease-out both; }
      @keyframes vwFadeIn { from { opacity:0; transform: translateY(6px);} to { opacity:1; transform: translateY(0);} }
      .vw-card{
        border: 1px solid var(--stroke);
        background: var(--card);
        border-radius: 18px;
        padding: 1rem 1rem;
        box-shadow: 0 10px 30px rgba(0,0,0,.22);
      }
      .vw-card.glow{ position: relative; }
      .vw-card.glow:before{
        content:'';
        position:absolute; inset:-1px;
        border-radius: 18px;
        padding: 1px;
        background: linear-gradient(135deg, rgba(56,230,214,.55), rgba(43,108,255,.55), rgba(255,200,87,.35));
        -webkit-mask: linear-gradient(#000 0 0) content-box, linear-gradient(#000 0 0);
        -webkit-mask-composite: xor;
        mask-composite: exclude;
        opacity: .7; pointer-events:none;
      }
      .vw-metric{ display:flex; flex-direction:column; gap:.25rem; }
      .vw-metric .k{ color: var(--muted); font-size: .9rem;}
      .vw-metric .v{ font-size: 1.55rem; font-weight: 800; letter-spacing: .2px;}
      .tilt{ transform-style: preserve-3d; transition: transform .15s ease-out; }
      .tilt:hover{ transform: perspective(900px) rotateX(2deg) rotateY(-2deg) translateY(-1px); }
      .btn-primary{
        background: linear-gradient(135deg, var(--teal), var(--blue)) !important;
        border: none !important;
      }
    "))
  ),
  div(class="vw-topbar",
      div(class="vw-brand",
          div(class="vw-dot"),
          div(
            div(class="vw-title", "VectorWatch"),
            div(class="vw-tag", "Malaria surveillance • Hotspots • Early signal • Targeted action")
          ),
          div(style="margin-left:auto;display:flex;gap:.5rem;align-items:center;padding-right:1rem;",
              uiOutput("whoami_ui"),
              actionButton("logout", "Logout", class="btn btn-outline-light btn-sm")
          )
      )
  ),
  uiOutput("main_ui")
)

server <- function(input, output, session){

  rv <- reactiveValues(
    authed = FALSE,
    user = NULL,
    role = NULL,
    data_dir = default_data_dir(),
    kenya_sf = NULL
  )

  observe({ rv$kenya_sf <- get_kenya_adm1() })

  output$whoami_ui <- renderUI({
    if (!isTRUE(rv$authed)) return(span(style="opacity:.75;", "Not signed in"))
    span(style="opacity:.85;font-size:.95rem;",
         paste0("Signed in: ", rv$user, "  •  ", rv$role))
  })

  login_ui <- function(){
    fluidRow(
      column(12,
        div(class="vw-card glow tilt vw-fadein", style="max-width: 560px; margin: 5vh auto 0 auto;",
          div(style="display:flex;align-items:center;gap:.6rem;margin-bottom:.4rem;",
              span(style="font-size: 1.35rem;", "🦟"),
              span(style="font-size: 1.25rem; font-weight: 800;", "Secure Access")
          ),
          div(style="color: rgba(255,255,255,.70); margin-bottom: 1rem;",
              "Sign in to access malaria hotspot intelligence dashboards."
          ),
          textInput("login_user", "Username", placeholder = "user"),
          passwordInput("login_pass", "Password", placeholder = "••••••••"),
          div(style="display:flex;gap:.6rem;align-items:center;",
              actionButton("login_btn", "Login", class="btn-primary"),
              span(style="color: rgba(255,255,255,.55); font-size:.92rem;",
                   "Demo accounts: user/user123, admin/admin123")
          ),
          uiOutput("login_msg")
        )
      )
    )
  }

  app_ui <- function(){
    fluidRow(
      column(3,
        div(class="vw-card tilt vw-fadein",
          h5("Dataset location"),
          textInput("data_dir", NULL, value = rv$data_dir),
          uiOutput("mode_hint"),
          tags$hr(style="border-color: rgba(255,255,255,.10);"),

          h5("Coverage"),
          selectInput("coverage", NULL, choices = c("high","medium","low"), selected = "high"),
          tags$hr(style="border-color: rgba(255,255,255,.10);"),

          uiOutput("time_selector"),
          tags$hr(style="border-color: rgba(255,255,255,.10);"),

          h5("Filters"),
          sliderInput("min_n", "Minimum tests per county", min = 100, max = 10000, value = 500, step = 100),
          selectInput("cluster_filter", "Show clusters", choices = c("All","High-High","Low-Low","High-Low","Low-High","Not significant"), selected = "All"),
          tags$hr(style="border-color: rgba(255,255,255,.10);"),

          downloadButton("dl_hotspots", "Download hotspot table (CSV)", class="btn btn-outline-light")
        )
      ),

      column(9,
        div(class="vw-card glow vw-fadein",
          fluidRow(
            column(3, div(class="vw-card tilt", uiOutput("kpi_tests"))),
            column(3, div(class="vw-card tilt", uiOutput("kpi_pos"))),
            column(3, div(class="vw-card tilt", uiOutput("kpi_hotspots"))),
            column(3, div(class="vw-card tilt", uiOutput("kpi_moran")))
          ),
          br(),
          fluidRow(
            column(7, div(class="vw-card tilt", h5("Prevalence Map"), leafletOutput("map", height = 420))),
            column(5, div(class="vw-card tilt", h5("Priority Queue"), uiOutput("data_status"), tableOutput("priority_table")))
          ),
          br(),
          fluidRow(
            column(6, div(class="vw-card tilt", h5("LISA Cluster Map"), leafletOutput("map_lisa", height = 420))),
            column(6, div(class="vw-card tilt", h5("Weekly trend"), selectInput("trend_county", "County", choices = character(0)), plotlyOutput("trend_plot", height = 360)))
          ),
          br(),
          div(class="vw-card tilt", h5("3D County Profile"), plotlyOutput("plot3d", height = 520))
        )
      )
    )
  }

  output$main_ui <- renderUI({
    if (!isTRUE(rv$authed)) login_ui() else app_ui()
  })

  tries <- reactiveVal(0)

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pass %||% ""

    if (tries() >= 5) {
      output$login_msg <- renderUI(div(style="margin-top:.8rem;color:#FFC857;",
                                      "Too many attempts. Refresh the page to try again."))
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
      output$login_msg <- renderUI(div(style="margin-top:.8rem;color:#ff6b6b;",
                                      paste0("Login failed. Attempts: ", tries(), "/5")))
    }
  })

  observeEvent(input$logout, {
    rv$authed <- FALSE
    rv$user <- NULL
    rv$role <- NULL
  })

  observeEvent(input$data_dir, { rv$data_dir <- input$data_dir }, ignoreInit = TRUE)

  mode_info <- reactive({
    req(rv$authed)
    req(input$coverage)
    detect_mode(rv$data_dir, input$coverage)
  })

  output$mode_hint <- renderUI({
    mi <- mode_info()
    div(style="color: rgba(255,255,255,.70); font-size:.92rem; margin-top:.4rem;",
        paste0("Auto-detect: ", mi$hint))
  })

  output$time_selector <- renderUI({
    mi <- mode_info()
    if(mi$mode == "weekly"){
      tagList(
        h5("ISO weeks (2024)"),
        selectizeInput("time_vals", NULL,
          choices = 1:52, selected = c(10,11,12,13,14,15), multiple = TRUE)
      )
    } else {
      tagList(
        h5("Months (2024)"),
        selectizeInput("time_vals", NULL,
          choices = 1:12, selected = c(3,4,5), multiple = TRUE)
      )
    }
  })

  df_raw <- reactive({
    req(rv$authed)
    mi <- mode_info()
    req(input$time_vals)

    validate(need(mi$mode != "none",
      paste0("No files detected. Make sure your folder contains high/medium/low with VectorWatch CSVs.\n\n",
             "Example weekly filename: vectorwatch_malaria_2024_W10_high.csv\n",
             "Example monthly filename: vectorwatch_malaria_2024_03_high.csv")
    ))

    df <- read_data(rv$data_dir, input$coverage, mi$mode, as.integer(input$time_vals))
    validate(need(!is.null(df),
      "No matching files for your selected time range. Try selecting a different week/month, or check your folder."
    ))
    df
  })

  county_summary <- reactive({
    df_raw() |> make_county_summary() |> filter(n_tests >= input$min_n)
  })

  output$data_status <- renderUI({
    mi <- mode_info()
    div(style="margin:.4rem 0 0; padding:.55rem .7rem; border-radius: 14px;
              border:1px solid rgba(56,230,214,.25); background: rgba(56,230,214,.08);
              color: rgba(255,255,255,.82);",
        strong("Using: "), rv$data_dir,
        br(),
        span(style="opacity:.85;",
             paste0("Coverage: ", input$coverage, " • Mode: ", mi$mode, " • Selected: ", paste(input$time_vals, collapse=", ")))
    )
  })

  observeEvent(county_summary(), {
    updateSelectInput(session, "trend_county",
                      choices = county_summary()$county,
                      selected = county_summary()$county[1])
  }, ignoreInit = TRUE)

  output$kpi_tests <- renderUI({
    df <- df_raw()
    div(class="vw-metric", div(class="k","Records loaded"),
        div(class="v", format(nrow(df), big.mark=",")))
  })

  output$kpi_pos <- renderUI({
    df <- df_raw()
    rate <- mean(df$parasite_detected == 1, na.rm = TRUE)
    div(class="vw-metric", div(class="k","Positivity rate"),
        div(class="v", paste0(round(100*rate,1), "%")))
  })

  kenya_joined <- reactive({
    req(rv$kenya_sf)
    rv$kenya_sf |> left_join(county_summary(), by="county")
  })

  esda <- reactive({
    req(rv$kenya_sf)
    esda_on_polygons(kenya_joined())
  })

  output$kpi_hotspots <- renderUI({
    hh <- sum(esda()$km$cluster == "High-High", na.rm = TRUE)
    div(class="vw-metric", div(class="k","High–High hotspots"), div(class="v", hh))
  })

  output$kpi_moran <- renderUI({
    mor <- esda()$moran
    I  <- unname(mor$estimate[["Moran I statistic"]])
    p  <- mor$p.value
    div(class="vw-metric", div(class="k","Global Moran’s I (p-value)"),
        div(class="v", paste0(round(I,3), " (", signif(p,3), ")")))
  })

  output$priority_table <- renderTable({
    km <- esda()$km |> st_drop_geometry()
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
      select(county, cluster, n_tests, prevalence, p_Ii) |>
      head(10)
  }, striped=TRUE, digits=3)

  pal_prev <- reactive({
    colorNumeric("viridis", domain = county_summary()$prevalence, na.color="transparent")
  })

  output$map <- renderLeaflet({
    req(rv$kenya_sf)
    km <- kenya_joined()
    pal <- pal_prev()

    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color="rgba(255,255,255,.14)", weight=1, opacity=1,
        fillColor=~pal(prevalence), fillOpacity=0.75,
        label=~paste0("<b>", county, "</b><br/>Tests: ", ifelse(is.na(n_tests),"—",n_tests),
                      "<br/>Prevalence: ", ifelse(is.na(prevalence),"—",round(prevalence,1)),"%") |> lapply(HTML),
        highlightOptions=highlightOptions(weight=2, color="#38E6D6", fillOpacity=0.9, bringToFront=TRUE)
      ) |>
      addLegend("bottomright", pal=pal, values=~prevalence, title="Prevalence (%)", opacity=1)
  })

  output$map_lisa <- renderLeaflet({
    km <- esda()$km
    lisa_pal <- colorFactor(
      palette=c("High-High"="#FF6B6B","Low-Low"="#4DFFB5","High-Low"="#FFC857","Low-High"="#7AA7FF","Not significant"="rgba(255,255,255,.10)"),
      domain=c("High-High","Low-Low","High-Low","Low-High","Not significant")
    )
    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color="rgba(255,255,255,.14)", weight=1, opacity=1,
        fillColor=~lisa_pal(cluster), fillOpacity=0.78,
        label=~paste0("<b>", county, "</b><br/>Cluster: ", cluster,
                      "<br/>Prev: ", ifelse(is.na(prevalence),"—",round(prevalence,1)),"%<br/>p: ",
                      ifelse(is.na(p_Ii),"—",signif(p_Ii,3))) |> lapply(HTML),
        highlightOptions=highlightOptions(weight=2, color="#38E6D6", fillOpacity=0.9, bringToFront=TRUE)
      ) |>
      addLegend("bottomright", pal=lisa_pal, values=~cluster, title="LISA cluster", opacity=1)
  })

  output$trend_plot <- renderPlotly({
    req(input$trend_county)
    df <- df_raw() |> filter(county == input$trend_county)

    wk <- df |>
      group_by(iso_week) |>
      summarise(
        tests = n(),
        cases = sum(parasite_detected == 1, na.rm = TRUE),
        positivity = 100 * cases / tests,
        .groups="drop"
      ) |>
      arrange(iso_week)

    plot_ly(wk, x=~iso_week, y=~positivity, type="scatter", mode="lines+markers",
            hovertemplate="Week: %{x}<br>Positivity: %{y:.1f}%<extra></extra>") |>
      layout(yaxis=list(title="Positivity (%)"), xaxis=list(title="ISO Week"),
             margin=list(l=40,r=10,b=40,t=10))
  })

  output$plot3d <- renderPlotly({
    km <- esda()$km |> st_drop_geometry() |>
      filter(!is.na(mean_age), !is.na(mean_hb), !is.na(prevalence))

    plot_ly(
      data=km,
      x=~mean_age, y=~mean_hb, z=~prevalence,
      type="scatter3d", mode="markers",
      color=~cluster,
      marker=list(size=4, opacity=0.85),
      text=~paste0("County: ", county, "<br>Prev: ", round(prevalence,1), "%<br>Tests: ", n_tests, "<br>Cluster: ", cluster),
      hoverinfo="text"
    ) |>
      layout(scene=list(
        xaxis=list(title="Mean age"),
        yaxis=list(title="Mean Hb (g/dL)"),
        zaxis=list(title="Prevalence (%)")
      ),
      margin=list(l=0,r=0,b=0,t=0))
  })

  output$dl_hotspots <- downloadHandler(
    filename = function(){ paste0("vectorwatch_hotspots_", Sys.Date(), ".csv") },
    content = function(file){
      km <- esda()$km |> st_drop_geometry()
      out <- km |>
        filter(!is.na(prevalence)) |>
        arrange(factor(cluster, levels=c("High-High","High-Low","Low-High","Low-Low","Not significant")),
                desc(prevalence)) |>
        select(county, cluster, n_tests, malaria_cases, prevalence, Ii, p_Ii)
      write.csv(out, file, row.names=FALSE)
    }
  )
}

shinyApp(ui, server)

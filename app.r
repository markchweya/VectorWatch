# VectorWatch — single-file Shiny app (fixed + clean)
# -----------------------------------------------------
# IMPORTANT: This is R code.
# Do NOT paste any Python wrapper (e.g., "from pathlib import Path") into RStudio.
#
# Run like this:
#   shiny::runApp("C:/Users/HP/OneDrive/Documents/School Work/Statistical Modeling")
#
# Demo logins:
#   user  / user123
#   admin / admin123

needed <- c(
  "shiny","bslib","htmltools","dplyr","janitor","stringr","lubridate",
  "digest","leaflet","plotly","shinyFiles","shinyjs",
  "sf","spdep","geodata"
)
missing <- needed[!vapply(needed, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if(length(missing)){
  stop(
    paste0(
      "Missing packages: ", paste(missing, collapse = ", "), "\n\n",
      "Install once (in Console), then re-run:\n",
      "install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "), dependencies=TRUE)\n"
    ),
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(digest)
library(leaflet)
library(plotly)
library(shinyFiles)
library(shinyjs)
library(sf)
library(spdep)
library(geodata)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x

# (Fix for your earlier crash: runjs must come from shinyjs)
runjs <- shinyjs::runjs

hash_pw <- function(x) digest(x, algo = "sha256")

ACCOUNTS <- data.frame(
  user = c("user","admin"),
  pass_hash = c(hash_pw("user123"), hash_pw("admin123")),
  role = c("viewer","admin"),
  stringsAsFactors = FALSE
)

favicon_data_uri <- function(emoji = "🦟"){
  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>",
    "<text y='.9em' font-size='90'>", emoji, "</text></svg>"
  )
  paste0("data:image/svg+xml,", URLencode(svg, reserved = TRUE))
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

    name_col <- intersect(names(kenya_sf), c("NAME_1","shapeName","name","VARNAME_1"))[1]
    if(is.na(name_col) || is.null(name_col)) return(NULL)

    kenya_sf |>
      rename(county = all_of(name_col)) |>
      mutate(
        county = as.character(county),
        county_key = norm_county(county)
      )
  }, error = function(e) NULL)
}

detect_mode <- function(base_dir, coverage){
  folder <- file.path(base_dir, coverage)
  if(!dir.exists(folder)) return(list(mode="none", hint="Coverage folder missing."))

  f <- list.files(folder, full.names = FALSE)
  weekly_pat  <- "^vectorwatch_malaria_2024_W\\d{2}_.+\\.csv$"
  monthly_pat <- "^vectorwatch_malaria_2024_\\d{2}_.+\\.csv$"

  has_weekly  <- any(grepl(weekly_pat,  f, ignore.case = TRUE))
  has_monthly <- any(grepl(monthly_pat, f, ignore.case = TRUE)) && !has_weekly

  if(has_weekly)  return(list(mode="weekly",  hint="Weekly extracts detected (W01..W52)."))
  if(has_monthly) return(list(mode="monthly", hint="Monthly extracts detected (01..12)."))
  list(mode="none", hint="No VectorWatch CSVs detected in this coverage folder.")
}

coerce_schema <- function(df){
  df <- df |> clean_names()

  if(!("county" %in% names(df))) df$county <- NA_character_
  if(!("date" %in% names(df))) df$date <- NA

  if(!("parasite_detected" %in% names(df))){
    cand <- intersect(names(df), c("positive","pos","result","malaria_positive"))
    if(length(cand)) df$parasite_detected <- as.integer(df[[cand[1]]])
    else df$parasite_detected <- 0L
  }

  if(!("age" %in% names(df))){
    cand <- intersect(names(df), c("age_years","years"))
    if(length(cand)) df$age <- suppressWarnings(as.numeric(df[[cand[1]]]))
    else df$age <- NA_real_
  }

  if(!("hemoglobin_g_dl" %in% names(df))){
    cand <- intersect(names(df), c("hb","hgb","hemoglobin"))
    if(length(cand)) df$hemoglobin_g_dl <- suppressWarnings(as.numeric(df[[cand[1]]]))
    else df$hemoglobin_g_dl <- NA_real_
  }

  if(!("wbc_cells_ul" %in% names(df))){
    cand <- intersect(names(df), c("wbc","wbc_ul","white_blood_cells"))
    if(length(cand)) df$wbc_cells_ul <- suppressWarnings(as.numeric(df[[cand[1]]]))
    else df$wbc_cells_ul <- NA_real_
  }

  df |>
    mutate(
      county = as.character(county),
      county_key = norm_county(county),
      date = suppressWarnings(as.Date(date)),
      parasite_detected = as.integer(parasite_detected),
      iso_week = as.integer(ifelse(!is.na(date), lubridate::isoweek(date), NA))
    )
}

read_data <- function(base_dir, coverage, mode, time_vals){
  folder <- file.path(base_dir, coverage)

  files <- if(mode == "weekly"){
    file.path(folder, sprintf("vectorwatch_malaria_2024_W%02d_%s.csv", time_vals, coverage))
  } else {
    file.path(folder, sprintf("vectorwatch_malaria_2024_%02d_%s.csv", time_vals, coverage))
  }

  files <- files[file.exists(files)]
  if(!length(files)) return(NULL)

  df <- do.call(rbind, lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE)))
  coerce_schema(df)
}

make_county_summary <- function(df){
  df |>
    filter(!is.na(county_key), county_key != "") |>
    group_by(county_key) |>
    summarise(
      n_tests = n(),
      malaria_cases = sum(parasite_detected == 1, na.rm = TRUE),
      prevalence = 100 * malaria_cases / n_tests,
      mean_age = mean(age, na.rm = TRUE),
      mean_hb = mean(hemoglobin_g_dl, na.rm = TRUE),
      mean_wbc = mean(wbc_cells_ul, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(prevalence))
}

esda_on_polygons <- function(kenya_joined){
  km <- kenya_joined |> filter(!is.na(prevalence))
  if(nrow(km) < 3){
    return(list(ok=FALSE, msg="Not enough counties for ESDA. Select more time slices or lower minimum tests.", km=km))
  }

  nb <- spdep::poly2nb(km, queen = TRUE)
  if(length(nb) == 0){
    return(list(ok=FALSE, msg="Neighbour list is empty (topology issue).", km=km))
  }

  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  mor <- tryCatch(spdep::moran.test(km$prevalence, listw = lw, zero.policy = TRUE), error=function(e) NULL)
  lisa <- tryCatch(spdep::localmoran(km$prevalence, lw, zero.policy = TRUE), error=function(e) NULL)

  if(is.null(mor) || is.null(lisa)){
    return(list(ok=FALSE, msg="ESDA couldn't compute. Add more time slices.", km=km))
  }

  km$Ii <- lisa[,1]
  km$p_Ii <- lisa[,5]

  z <- as.numeric(scale(km$prevalence))
  lag_z <- spdep::lag.listw(lw, z, zero.policy = TRUE)

  km$cluster <- "Not significant"
  sig <- km$p_Ii <= 0.05
  km$cluster[sig & z > 0 & lag_z > 0] <- "High-High"
  km$cluster[sig & z < 0 & lag_z < 0] <- "Low-Low"
  km$cluster[sig & z > 0 & lag_z < 0] <- "High-Low"
  km$cluster[sig & z < 0 & lag_z > 0] <- "Low-High"

  list(ok=TRUE, km=km, moran=mor)
}

theme_vw <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  base_font = font_google("Plus Jakarta Sans"),
  heading_font = font_google("Sora")
)

ui <- fluidPage(
  theme = theme_vw,
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel="icon", href = favicon_data_uri("🦟")),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root{--glass:rgba(255,255,255,.06);--stroke:rgba(255,255,255,.10);--r:22px;--shadow:0 14px 40px rgba(0,0,0,.35);}
      html,body{height:100%;}
      body{
        background:
          radial-gradient(1200px 650px at 12% 8%, rgba(76,120,255,.18), transparent 55%),
          radial-gradient(900px 520px at 86% 22%, rgba(74,242,214,.14), transparent 55%),
          radial-gradient(1000px 540px at 60% 100%, rgba(181,108,255,.10), transparent 65%),
          linear-gradient(180deg, #050A14, #070D1D);
      }
      .vw-card{border:1px solid var(--stroke);background:var(--glass);border-radius:var(--r);box-shadow:var(--shadow);}
      .vw-pad{padding:1rem;}
      .vw-ghost{background:rgba(255,255,255,.06)!important;border:1px solid rgba(255,255,255,.14)!important;color:#fff!important;border-radius:999px!important;}
      .btn-primary{border:none!important;border-radius:999px!important;background:linear-gradient(135deg, rgba(74,242,214,.95), rgba(76,120,255,.95))!important;}

      .vw-landing{height:100vh; overflow:hidden; position:relative;}
      .vw-sky{position:absolute; inset:0; pointer-events:none; z-index:1;}
      .vw-moz{position:absolute; font-size:14px; opacity:.92;}
      .vw-laser{position:absolute; height:2px; background:linear-gradient(90deg, rgba(74,242,214,0), rgba(74,242,214,.95), rgba(76,120,255,.95)); opacity:0;}
      .vw-zap{position:absolute; width:26px; height:26px; border-radius:999px; border:1px solid rgba(74,242,214,.55); opacity:0;}

      .vw-topbar{height:64px;display:flex;align-items:center;gap:.9rem;padding:0 1rem;border-bottom:1px solid rgba(255,255,255,.10);backdrop-filter:blur(14px);background:rgba(5,10,20,.62);}
      .vw-dot{width:10px;height:10px;border-radius:999px;background:linear-gradient(135deg,#4AF2D6,#4C78FF);}
      .vw-wrap{height:100vh;display:flex;flex-direction:column;overflow:hidden;}
      .vw-shell{flex:1;display:flex;min-height:0;}
      .vw-sidebar{width:340px;padding:1rem;border-right:1px solid rgba(255,255,255,.10);background:rgba(255,255,255,.03);backdrop-filter:blur(14px);overflow:auto;}
      .vw-main{flex:1;min-width:0;padding:1rem;overflow:auto;}
      .vw-collapsed .vw-sidebar{width:0!important;padding:0!important;opacity:0;transform:translateX(-10px);border-right:none;}

      .tabbable > .nav-pills > li > a{border-radius:999px!important;}
      .selectize-control.single .selectize-input{border-radius:14px!important;}

      @media(max-width:992px){
        .vw-sidebar{position:fixed;top:64px;left:0;height:calc(100vh - 64px);width:min(88vw,380px);transform:translateX(-105%);transition:transform .26s ease;z-index:40;box-shadow:0 30px 80px rgba(0,0,0,.55);}
        .vw-side-open .vw-sidebar{transform:translateX(0);}
        .vw-scrim{position:fixed;inset:64px 0 0 0;background:rgba(0,0,0,.45);z-index:35;display:none;}
        .vw-side-open .vw-scrim{display:block;}
      }
    ")),
    tags$script(HTML("
      function vwRand(min, max){ return Math.random() * (max - min) + min; }
      function vwLaserZap(sky, x1,y1, x2,y2){
        const dx=x2-x1, dy=y2-y1;
        const len=Math.sqrt(dx*dx+dy*dy);
        const ang=Math.atan2(dy,dx)*180/Math.PI;

        const laser=document.createElement('div');
        laser.className='vw-laser';
        laser.style.left=x1+'px';
        laser.style.top=y1+'px';
        laser.style.width=len+'px';
        laser.style.transform=`rotate(${ang}deg)`;
        sky.appendChild(laser);

        const zap=document.createElement('div');
        zap.className='vw-zap';
        zap.style.left=(x2-13)+'px';
        zap.style.top=(y2-13)+'px';
        sky.appendChild(zap);

        laser.animate([{opacity:0},{opacity:1},{opacity:0}],{duration:220,easing:'ease-out'});
        zap.animate([{opacity:0,transform:'scale(.6)'},{opacity:1,transform:'scale(1.15)'},{opacity:0,transform:'scale(1.8)'}],{duration:320,easing:'ease-out'});

        setTimeout(()=>{laser.remove();zap.remove();},450);
      }
      function vwSpawnMoz(){
        const sky=document.querySelector('.vw-sky');
        if(!sky) return;
        if(sky.querySelectorAll('.vw-moz').length > 6) return;

        const w=window.innerWidth, h=window.innerHeight;
        const moz=document.createElement('div');
        moz.className='vw-moz';
        moz.textContent='🦟';

        const sx=vwRand(20,w-20), sy=vwRand(80,h-40);
        moz.style.left=sx+'px'; moz.style.top=sy+'px';
        sky.appendChild(moz);

        const flyDur=vwRand(2600,5200);
        moz.animate([{opacity:0},{opacity:.95},{opacity:.95},{opacity:0}],{duration:flyDur,easing:'ease-in-out'});

        const zapAt=vwRand(900, flyDur-600);
        setTimeout(()=>{
          const r=moz.getBoundingClientRect();
          const tx=r.left+r.width/2, ty=r.top+r.height/2;
          const fromX=Math.random()<0.5 ? vwRand(0,60) : vwRand(w-60,w);
          const fromY=vwRand(64,160);
          vwLaserZap(sky, fromX, fromY, tx, ty);
          moz.animate([{opacity:1},{opacity:0}],{duration:160,easing:'ease-out'});
          setTimeout(()=>moz.remove(), 200);
        }, zapAt);

        setTimeout(()=>{ if(moz.isConnected) moz.remove(); }, flyDur+200);
      }
      let vwMozTimer=null;
      function vwStartLanding(){
        if(vwMozTimer) return;
        vwMozTimer=setInterval(()=>{ if(document.querySelector('.vw-landing')) vwSpawnMoz(); }, vwRand(650,1200));
      }
      function vwStopLanding(){ if(vwMozTimer){ clearInterval(vwMozTimer); vwMozTimer=null; } }

      function vwToggleSidebar(){
        const root=document.getElementById('vwRoot');
        if(!root) return;
        const isMobile=window.matchMedia('(max-width: 992px)').matches;
        if(isMobile) root.classList.toggle('vw-side-open');
        else root.classList.toggle('vw-collapsed');
      }
      window.vwToggleSidebar=vwToggleSidebar;
      function vwCloseSidebar(){
        const root=document.getElementById('vwRoot');
        if(root) root.classList.remove('vw-side-open');
      }
      window.vwCloseSidebar=vwCloseSidebar;

      const obs=new MutationObserver(()=>{ if(document.querySelector('.vw-landing')) vwStartLanding(); else vwStopLanding(); });
      window.addEventListener('load', ()=>{
        obs.observe(document.body,{childList:true,subtree:true});
        if(document.querySelector('.vw-landing')) vwStartLanding();
      });
    "))
  ),
  uiOutput("page")
)

server <- function(input, output, session){

  rv <- reactiveValues(step="landing", authed=FALSE, user=NULL, role=NULL, data_dir="", kenya_sf=NULL)

  observe({ rv$kenya_sf <- get_kenya_adm1() })

  # folder picker
  volumes <- shinyFiles::getVolumes()()
  shinyDirChoose(input, "dir", roots = volumes, session = session)

  landing_ui <- function(){
    div(class="vw-landing",
      div(class="vw-sky"),
      div(style="position:absolute; inset:0; display:grid; place-items:center; padding:24px; z-index:2;",
        div(style="width:min(980px,96vw); display:grid; grid-template-columns:1.15fr .85fr; gap:18px;",
          div(class="vw-card vw-pad",
            h1("VectorWatch"),
            p("We fight malaria with early signal. Mosquitoes appear — then get zapped."),
            div(style="display:flex; gap:10px; flex-wrap:wrap;",
              actionButton("go_login","Go to App", class="btn-primary"),
              actionButton("learn_more","What’s inside?", class="vw-ghost")
            ),
            div(style="margin-top:10px; opacity:.78;", "Demo login: user/user123 • admin/admin123")
          ),
          div(class="vw-card vw-pad",
            h4("You’ll see"),
            tags$ul(
              tags$li("County prevalence heat map"),
              tags$li("LISA hotspot clusters"),
              tags$li("Trends + 3D profile")
            )
          )
        )
      )
    )
  }

  login_ui <- function(){
    div(class="vw-landing",
      div(style="position:absolute; inset:0; display:grid; place-items:center; padding:24px; z-index:2;",
        div(class="vw-card vw-pad", style="width:min(620px,92vw);",
          h2("Secure Access"),
          textInput("login_user","Username", placeholder="user"),
          passwordInput("login_pass","Password", placeholder="••••••••"),
          div(style="display:flex; gap:10px; flex-wrap:wrap;",
            actionButton("login_btn","Login", class="btn-primary"),
            actionButton("back_landing","Back", class="vw-ghost")
          ),
          uiOutput("login_msg")
        )
      )
    )
  }

  app_ui <- function(){
    div(id="vwRoot", class="vw-wrap",
      div(class="vw-topbar",
        div(class="vw-dot"),
        div(style="display:flex; flex-direction:column;",
          div(style="font-weight:900;","VectorWatch"),
          div(style="opacity:.7; font-size:.9rem;","Malaria surveillance • hotspots • early signal")
        ),
        div(style="margin-left:auto; display:flex; gap:10px; align-items:center;",
          actionButton("toggle_side","☰", class="vw-ghost", onclick="vwToggleSidebar();"),
          uiOutput("whoami_ui"),
          actionButton("logout","Logout", class="vw-ghost")
        )
      ),
      div(class="vw-scrim", onclick="vwCloseSidebar();"),
      div(class="vw-shell",
        div(class="vw-sidebar",
          div(class="vw-card vw-pad",
            h4("Controls"),
            textInput("data_dir","Dataset folder", value=rv$data_dir, placeholder="Folder containing high/, medium/, low/"),
            shinyDirButton("dir","Pick folder on this device","Choose folder"),

            hr(),
            selectInput("coverage","Coverage", choices=c("high","medium","low"), selected="high"),
            uiOutput("mode_hint"),
            uiOutput("time_selector"),

            hr(),
            sliderInput("min_n","Minimum tests per county", min=100, max=10000, value=500, step=100),
            selectInput("cluster_filter","Show clusters",
              choices=c("All","High-High","Low-Low","High-Low","Low-High","Not significant"), selected="All"
            ),

            hr(),
            downloadButton("dl_hotspots","Download hotspots (CSV)", class="vw-ghost")
          )
        ),
        div(class="vw-main", uiOutput("app_main"))
      )
    )
  }

  output$page <- renderUI({
    if(rv$step == "landing") return(landing_ui())
    if(rv$step == "login") return(login_ui())
    if(rv$step == "app") return(app_ui())
    landing_ui()
  })

  observeEvent(input$go_login, { rv$step <- "login" })
  observeEvent(input$back_landing, { rv$step <- "landing" })
  observeEvent(input$learn_more, {
    showModal(modalDialog(
      title = "VectorWatch — quick tour",
      easyClose = TRUE,
      footer = modalButton("Close"),
      p("1) Pick dataset folder (with high/, medium/, low/)"),
      p("2) Auto-detect weekly or monthly"),
      p("3) Maps + hotspots + trends")
    ))
  })

  tries <- reactiveVal(0)
  output$login_msg <- renderUI(NULL)

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pass %||% ""

    row <- ACCOUNTS |> filter(user == u)
    ok <- nrow(row) == 1 && identical(row$pass_hash[[1]], hash_pw(p))

    if(ok){
      rv$authed <- TRUE
      rv$user <- u
      rv$role <- row$role[[1]]

      # Clear dataset path each login (fixes your "phantom dataset")
      rv$data_dir <- ""
      updateTextInput(session, "data_dir", value = "")

      rv$step <- "app"
      tries(0)
      output$login_msg <- renderUI(NULL)
    } else {
      tries(tries() + 1)
      output$login_msg <- renderUI(
        div(style="margin-top:10px;color:#ff6b6b;font-weight:800;",
            paste0("Login failed (", tries(), "/5)"))
      )
    }
  })

  output$whoami_ui <- renderUI({
    if(!isTRUE(rv$authed)) return(span(style="opacity:.7;","Not signed in"))
    span(style="opacity:.9;", paste0("Signed in: ", rv$user, " • ", rv$role))
  })

  observeEvent(input$logout, {
    rv$authed <- FALSE
    rv$user <- NULL
    rv$role <- NULL
    rv$data_dir <- ""
    rv$step <- "landing"
  })

  observeEvent(input$dir, {
    path <- shinyFiles::parseDirPath(volumes, input$dir)
    if(length(path) && dir.exists(path)){
      rv$data_dir <- normalizePath(path, winslash="/", mustWork=FALSE)
      updateTextInput(session, "data_dir", value = rv$data_dir)
    }
  })

  observeEvent(input$data_dir, {
    rv$data_dir <- input$data_dir %||% ""
  }, ignoreInit = TRUE)

  dataset_status <- reactive({
    if(!isTRUE(rv$authed)) return(list(ready=FALSE, msg="Not signed in."))

    base <- rv$data_dir %||% ""
    if(identical(base,"")) return(list(ready=FALSE, msg="Pick a dataset folder to begin."))
    if(!dir.exists(base)) return(list(ready=FALSE, msg="That folder does not exist. Pick again."))

    cov <- input$coverage %||% "high"
    cov_dir <- file.path(base, cov)
    if(!dir.exists(cov_dir)) return(list(ready=FALSE, msg="Your folder must contain: high/, medium/, low/."))

    mi <- detect_mode(base, cov)
    if(mi$mode == "none"){
      return(list(ready=FALSE, msg=paste0("No VectorWatch CSVs found inside ", cov, "/."), mi=mi))
    }

    list(ready=TRUE, msg="Dataset ready.", mi=mi)
  })

  output$mode_hint <- renderUI({
    st <- dataset_status()
    mi <- st$mi %||% list(hint="Pick a folder to begin.")
    div(style="margin-top:8px; opacity:.8;", paste0("Auto-detect: ", mi$hint))
  })

  output$time_selector <- renderUI({
    st <- dataset_status()
    if(!isTRUE(st$ready)) return(div(style="opacity:.8;", st$msg))

    if(st$mi$mode == "weekly"){
      selectizeInput(
        "time_vals","ISO weeks (2024)",
        choices=1:52, selected=c(10,11,12,13,14,15),
        multiple=TRUE,
        options=list(plugins=list("remove_button"))
      )
    } else {
      selectizeInput(
        "time_vals","Months (2024)",
        choices=1:12, selected=c(3,4,5),
        multiple=TRUE,
        options=list(plugins=list("remove_button"))
      )
    }
  })

  df_raw <- reactive({
    st <- dataset_status()
    req(isTRUE(st$ready))
    req(input$time_vals)

    df <- read_data(rv$data_dir, input$coverage, st$mi$mode, as.integer(input$time_vals))
    validate(need(!is.null(df), "No matching files for your selection."))
    df
  })

  county_summary <- reactive({
    req(df_raw())
    make_county_summary(df_raw()) |> filter(n_tests >= (input$min_n %||% 500))
  })

  kenya_joined <- reactive({
    req(rv$kenya_sf)
    st <- dataset_status()
    if(!isTRUE(st$ready)) return(rv$kenya_sf |> mutate(prevalence = NA_real_))
    rv$kenya_sf |> left_join(county_summary(), by="county_key")
  })

  esda <- reactive({
    req(rv$kenya_sf)
    st <- dataset_status()
    if(!isTRUE(st$ready)) return(list(ok=FALSE, msg=st$msg, km=rv$kenya_sf))
    esda_on_polygons(kenya_joined())
  })

  output$app_main <- renderUI({
    st <- dataset_status()
    if(!isTRUE(st$ready)){
      return(div(class="vw-card vw-pad",
        h3("Connect a dataset"),
        p(style="opacity:.8;", st$msg)
      ))
    }

    tabsetPanel(type="pills",
      tabPanel("Map",      div(class="vw-card vw-pad", leafletOutput("map_prev", height="70vh"))),
      tabPanel("Hotzones", div(class="vw-card vw-pad", leafletOutput("map_lisa", height="70vh"))),
      tabPanel("Trends",   div(class="vw-card vw-pad", plotlyOutput("trend_plot", height="70vh"))),
      tabPanel("3D",       div(class="vw-card vw-pad", plotlyOutput("plot3d", height="70vh")))
    )
  })

  output$map_prev <- renderLeaflet({
    req(rv$kenya_sf)
    km <- kenya_joined()
    pal <- colorNumeric("viridis", domain = km$prevalence, na.color = "transparent")

    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color = "rgba(255,255,255,.16)", weight = 1, opacity = 1,
        fillColor = ~pal(prevalence), fillOpacity = 0.78,
        label = ~paste0(
          "<b>", county, "</b><br/>Tests: ", ifelse(is.na(n_tests),"—",n_tests),
          "<br/>Prevalence: ", ifelse(is.na(prevalence),"—",round(prevalence,1)),"%"
        ) |> lapply(HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#4AF2D6", fillOpacity = 0.92, bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = pal, values = ~prevalence, title = "Prevalence (%)", opacity = 1)
  })

  output$map_lisa <- renderLeaflet({
    req(rv$kenya_sf)
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))
    km <- e$km

    # filter clusters if requested
    cf <- input$cluster_filter %||% "All"
    if(cf != "All"){
      km <- km |> filter(cluster == cf)
    }

    lisa_pal <- colorFactor(
      palette = c(
        "High-High"="#FF4D6D",
        "Low-Low"="#4AF2D6",
        "High-Low"="#FFC857",
        "Low-High"="#4C78FF",
        "Not significant"="#4AF2D5"
      ),
      domain = c("High-High","Low-Low","High-Low","Low-High","Not significant")
    )

    leaflet(km) |>
      addProviderTiles("CartoDB.DarkMatter") |>
      addPolygons(
        color = "rgba(255,255,255,.16)", weight = 1, opacity = 1,
        fillColor = ~lisa_pal(cluster), fillOpacity = 0.80,
        label = ~paste0(
          "<b>", county, "</b><br/>Cluster: ", cluster,
          "<br/>Prev: ", round(prevalence,1), "%<br/>p: ", signif(p_Ii,3)
        ) |> lapply(HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#4AF2D6", fillOpacity = 0.92, bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = lisa_pal, values = ~cluster, title = "LISA cluster", opacity = 1)
  })

  output$trend_plot <- renderPlotly({
    req(df_raw())

    wk <- df_raw() |>
      group_by(iso_week) |>
      summarise(
        tests = n(),
        cases = sum(parasite_detected == 1, na.rm = TRUE),
        positivity = 100 * cases / tests,
        .groups = "drop"
      ) |>
      arrange(iso_week)

    plot_ly(
      wk, x = ~iso_week, y = ~positivity,
      type="scatter", mode="lines+markers",
      hovertemplate="Week: %{x}<br>Positivity: %{y:.1f}%<extra></extra>",
      line=list(width=3)
    ) |>
      layout(
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="rgba(255,255,255,.88)"),
        xaxis=list(title="ISO week", gridcolor="rgba(255,255,255,.08)"),
        yaxis=list(title="Positivity (%)", gridcolor="rgba(255,255,255,.08)"),
        margin=list(l=55,r=15,b=45,t=15)
      )
  })

  output$plot3d <- renderPlotly({
    e <- esda()
    validate(need(isTRUE(e$ok), e$msg))

    km <- e$km |>
      st_drop_geometry() |>
      filter(!is.na(mean_age), !is.na(mean_hb), !is.na(prevalence))

    validate(need(nrow(km) > 3, "Not enough counties for 3D plot. Select more time slices or lower minimum tests."))

    plot_ly(
      km,
      x=~mean_age, y=~mean_hb, z=~prevalence,
      type="scatter3d", mode="markers",
      color=~cluster,
      marker=list(size=4, opacity=0.92),
      text=~paste0(
        "<b>", county, "</b><br>Prev: ", round(prevalence,1), "%<br>",
        "Tests: ", n_tests, "<br>Cluster: ", cluster
      ),
      hoverinfo="text"
    ) |>
      layout(
        paper_bgcolor="rgba(0,0,0,0)",
        font=list(color="rgba(255,255,255,.88)"),
        scene=list(
          bgcolor="rgba(0,0,0,0)",
          xaxis=list(title="Mean age", gridcolor="rgba(255,255,255,.08)"),
          yaxis=list(title="Mean Hb (g/dL)", gridcolor="rgba(255,255,255,.08)"),
          zaxis=list(title="Prevalence (%)", gridcolor="rgba(255,255,255,.08)")
        ),
        margin=list(l=0,r=0,b=0,t=0)
      )
  })

  output$dl_hotspots <- downloadHandler(
    filename = function(){ paste0("vectorwatch_hotspots_", Sys.Date(), ".csv") },
    content = function(file){
      e <- esda()
      if(!isTRUE(e$ok)){
        writeLines("ESDA not available for current selection.", file)
        return()
      }
      out <- e$km |>
        st_drop_geometry() |>
        select(county, cluster, n_tests, malaria_cases, prevalence, Ii, p_Ii) |>
        arrange(desc(prevalence))
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

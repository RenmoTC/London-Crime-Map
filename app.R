

library(shiny)
library(shinydashboard)   
library(leaflet)
library(plotly)
library(DT)               
library(MASS)             
library(dplyr)
library(tidyr)
library(spatstat)
library(spatstat.geom)
library(spatstat.explore)
library(raster)           
library(isoband)          
library(sf)
library(scales)
library(openintro)


## Data Loading and Preprocessing 


# Simple 0-1 normalisation helper (handles zero-variance safely)
normalise <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(0, length(x)))      # if all values are same, return zeros (avoid divide-by-zero)
  } else {
    return((x - rng[1]) / (rng[2] - rng[1]))
  }
}

# Load raw crime point data (expects columns: BoroughName, date, lng, lat, crime_type, etc.) 
crime_points <- read.csv("crime_points.csv", stringsAsFactors = FALSE)
crime_points$date <- as.Date(crime_points$date)  # make sure date is Date-class

#  Aggregate crimes per month per borough 
crime_monthly <- crime_points |>
  mutate(Month = format(date, "%Y-%m")) |>
  group_by(BoroughName, Month) |>
  summarise(MonthlyCrimes = n(), .groups = "drop")

#  Borough-level summary: total + mean monthly frequency 
crime_agg <- crime_monthly |>
  group_by(BoroughName) |>
  summarise(
    TotalCrimes = sum(MonthlyCrimes),
    CrimeFrequency = mean(MonthlyCrimes),
    .groups = "drop"
  )

#  month_cols was unused in your server; keeping for reference if needed later
month_cols <- unique(format(crime_points$date, "%Y%m"))

#  Load contextual data: IMD and Unemployment 
imd_data <- read.csv("imd_borough.csv", stringsAsFactors = FALSE)
unemp_data <- read.csv("unemployment_2024Q4.csv", stringsAsFactors = FALSE)

# Harmonise borough naming for reliable joins (" & " -> " and ")
imd_data$BoroughName  <- gsub(" & ", " and ", imd_data$BoroughName)
unemp_data$BoroughName <- gsub(" & ", " and ", unemp_data$BoroughName)

#  Join crime with IMD + Unemployment and build Risk metrics 
combined <- crime_agg |>
  left_join(imd_data, by = "BoroughName") |>
  left_join(unemp_data, by = "BoroughName") |>
  mutate(
    # basic scaling so components are on similar ranges
    TotalCrimes_norm    = normalise(TotalCrimes),
    CrimeFrequency_norm = normalise(CrimeFrequency),
    IMD_norm            = normalise(IMD_Score),
    Unemployment_norm   = normalise(UnemploymentRate),
    
    # Likelihood = how often crimes occur (frequency > total)
    Likelihood          = 0.70 * CrimeFrequency_norm + 0.30 * TotalCrimes_norm,
    
    # Vulnerability = socioeconomic backdrop (IMD > unemployment)
    Vulnerability       = 0.60 * IMD_norm + 0.40 * Unemployment_norm,
    
    # Overall risk mix (lean slightly towards Likelihood)
    RiskScore           = 0.60 * Likelihood + 0.40 * Vulnerability
  )

# Convert continuous RiskScore into 1–10 and label tiers
combined <- combined |>
  mutate(
    RiskScore10 = pmax(1, pmin(10, round(RiskScore * 10, 1))),
    RiskTier = case_when(
      RiskScore10 < 4 ~ "LOW",
      RiskScore10 < 7 ~ "MEDIUM",
      TRUE ~ "HIGH"
    )
  )

#  Build (quick) borough geometries from openintro points 
#  We convex-hull points per borough. This is a rough polygon and not official boundaries.
# If you need exact shapes, swap this for proper ONS boundary data.
data("london_boroughs", package = "openintro")
london_sf <- london_boroughs |>
  filter(!is.na(x) & !is.na(y)) |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  group_by(borough) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  mutate(geometry = st_convex_hull(geometry)) |>
  st_transform(crs = 4326) |>
  mutate(BoroughName = gsub(" & ", " and ", borough)) |>
  left_join(combined, by = "BoroughName")

# Precompute centroids for centering maps and nearby-area calc
london_sf <- london_sf |> mutate(centroid = st_centroid(geometry))

# Keep original crime points separate for tab filters
real_points <- crime_points


## UI


ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
    # Custom styling block. 
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap');
      
      :root {
        --apple-blue: #007AFF;
        --apple-blue-light: #5AC8FA;
        --apple-gray: #8E8E93;
        --apple-gray-light: #F2F2F7;
        --apple-gray-dark: #1C1C1E;
        --apple-background: #FBFBFD;
        --apple-surface: rgba(255, 255, 255, 0.8);
        --apple-surface-dark: rgba(28, 28, 30, 0.8);
        --apple-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        --apple-shadow-light: 0 4px 16px rgba(0, 0, 0, 0.05);
        --apple-border: rgba(255, 255, 255, 0.2);
        --apple-text: #1D1D1F;
        --apple-text-secondary: #86868B;
      }
      
      /* Global styles */
      * { margin: 0; padding: 0; box-sizing: border-box; }
      body, .main-header, .main-sidebar, .content-wrapper {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
        background: linear-gradient(135deg, #FBFBFD 0%, #F5F5F7 100%) !important;
        color: var(--apple-text) !important; font-weight: 400;
      }
      .content-wrapper { padding: 24px !important; min-height: 100vh; background: transparent !important; }
      
      /* Pretty, subtle glass cards */
      .glass-card {
        background: rgba(255, 255, 255, 0.25) !important;
        backdrop-filter: blur(20px) !important; -webkit-backdrop-filter: blur(20px) !important;
        border: 1px solid rgba(255, 255, 255, 0.18) !important; border-radius: 24px !important;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.08) !important;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important; overflow: hidden;
      }
      .glass-card:hover { transform: translateY(-2px) !important; box-shadow: 0 12px 40px rgba(0, 0, 0, 0.12) !important; }
      .glass-card-dark { background: rgba(28, 28, 30, 0.8) !important; border: 1px solid rgba(255, 255, 255, 0.1) !important; }
      
      /* Header */
      .app-header {
        background: rgba(255, 255, 255, 0.8) !important; backdrop-filter: blur(20px) !important; -webkit-backdrop-filter: blur(20px) !important;
        border: none !important; box-shadow: 0 1px 0 rgba(0, 0, 0, 0.04) !important;
        position: sticky; top: 0; z-index: 1000; padding: 16px 24px; margin-bottom: 32px; border-radius: 0 0 24px 24px;
      }
      .app-title {
        font-size: 34px !important; font-weight: 700 !important; color: var(--apple-text) !important; margin: 0 !important; letter-spacing: -0.02em;
        background: linear-gradient(135deg, #1D1D1F 0%, #86868B 100%); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;
      }
      
      /* Sidebar-like control panel */
      .control-panel {
        background: rgba(255, 255, 255, 0.3) !important; backdrop-filter: blur(30px) !important; -webkit-backdrop-filter: blur(30px) !important;
        border: 1px solid rgba(255, 255, 255, 0.2) !important; border-radius: 30px !important; padding: 32px !important; margin-bottom: 24px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.06) !important; transition: all 0.3s ease !important;
      }
      .control-panel:hover { background: rgba(255, 255, 255, 0.4) !important; transform: translateY(-1px); }
      
      /* Form controls */
      .form-group label {
        font-size: 17px !important; font-weight: 600 !important; color: var(--apple-text) !important;
        margin-bottom: 12px !important; display: block; letter-spacing: -0.01em;
      }
      .form-control, .selectize-input {
        background: rgba(255, 255, 255, 0.6) !important; border: 1px solid rgba(0, 0, 0, 0.1) !important; border-radius: 16px !important;
        padding: 16px 20px !important; font-size: 16px !important; font-weight: 400 !important; color: var(--apple-text) !important;
        transition: all 0.2s ease !important; backdrop-filter: blur(10px) !important; -webkit-backdrop-filter: blur(10px) !important;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--apple-blue) !important; box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.1) !important; background: rgba(255, 255, 255, 0.8) !important; outline: none !important;
      }
      
      /* Emergency CTA */
      .emergency-btn {
        background: linear-gradient(135deg, #FF3B30 0%, #FF6B5A 100%) !important; border: none !important; border-radius: 20px !important;
        color: white !important; font-size: 16px !important; font-weight: 600 !important; padding: 18px 24px !important; width: 100% !important;
        transition: all 0.2s ease !important; box-shadow: 0 4px 16px rgba(255, 59, 48, 0.3) !important; letter-spacing: -0.01em;
      }
      .emergency-btn:hover { transform: translateY(-1px) !important; box-shadow: 0 6px 20px rgba(255, 59, 48, 0.4) !important; background: linear-gradient(135deg, #FF453A 0%, #FF7A6B 100%) !important; }
      .emergency-btn:active { transform: translateY(0) !important; }
      
      /* Content boxes */
      .content-box {
        background: rgba(255, 255, 255, 0.25) !important; backdrop-filter: blur(25px) !important; -webkit-backdrop-filter: blur(25px) !important;
        border: 1px solid rgba(255, 255, 255, 0.18) !important; border-radius: 28px !important; padding: 32px !important; margin-bottom: 24px !important;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.08) !important; transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important; overflow: hidden; position: relative;
      }
      .content-box::before {
        content: ''; position: absolute; top: 0; left: 0; right: 0; height: 1px;
        background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.6), transparent);
      }
      .content-box:hover { transform: translateY(-3px) !important; box-shadow: 0 16px 48px rgba(0, 0, 0, 0.12) !important; background: rgba(255, 255, 255, 0.35) !important; }
      
      .box-header { border-bottom: 1px solid rgba(0, 0, 0, 0.06) !important; background: transparent !important; padding: 0 0 16px 0 !important; margin-bottom: 24px !important; }
      .box-title { font-size: 24px !important; font-weight: 600 !important; color: var(--apple-text) !important; margin: 0 !important; letter-spacing: -0.02em; }
      
      /* Risk pills */
      .risk-indicator {
        display: inline-flex !important; align-items: center !important; justify-content: center !important; padding: 12px 24px !important;
        border-radius: 25px !important; font-size: 16px !important; font-weight: 600 !important; letter-spacing: -0.01em; transition: all 0.2s ease !important;
        backdrop-filter: blur(10px) !important; -webkit-backdrop-filter: blur(10px) !important;
      }
      .risk-high   { background: linear-gradient(135deg, rgba(255, 59, 48, 0.9), rgba(255, 107, 90, 0.9)) !important; color: white !important; border: 1px solid rgba(255, 59, 48, 0.3) !important; box-shadow: 0 4px 16px rgba(255, 59, 48, 0.25) !important; }
      .risk-medium { background: linear-gradient(135deg, rgba(255, 204, 0, 0.9), rgba(255, 214, 51, 0.9)) !important; color: rgba(0, 0, 0, 0.8) !important; border: 1px solid rgba(255, 204, 0, 0.3) !important; box-shadow: 0 4px 16px rgba(255, 204, 0, 0.25) !important; }
      .risk-low    { background: linear-gradient(135deg, rgba(52, 199, 89, 0.9), rgba(104, 212, 124, 0.9)) !important; color: white !important; border: 1px solid rgba(52, 199, 89, 0.3) !important; box-shadow: 0 4px 16px rgba(52, 199, 89, 0.25) !important; }
      
      /* Stat cards */
      .stat-card {
        background: rgba(255, 255, 255, 0.4) !important; backdrop-filter: blur(15px) !important; -webkit-backdrop-filter: blur(15px) !important;
        border: 1px solid rgba(255, 255, 255, 0.25) !important; border-radius: 20px !important; padding: 24px !important; text-align: center !important;
        transition: all 0.3s ease !important; box-shadow: 0 6px 24px rgba(0, 0, 0, 0.06) !important; position: relative; overflow: hidden;
      }
      .stat-card::before { content: ''; position: absolute; top: 0; left: 0; right: 0; height: 2px; background: linear-gradient(90deg, var(--apple-blue), var(--apple-blue-light)); }
      .stat-card:hover { transform: translateY(-2px) !important; background: rgba(255, 255, 255, 0.5) !important; box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1) !important; }
      .stat-value { font-size: 28px !important; font-weight: 700 !important; color: var(--apple-blue) !important; margin: 0 0 8px 0 !important; letter-spacing: -0.02em; }
      .stat-label { font-size: 14px !important; font-weight: 500 !important; color: var(--apple-text-secondary) !important; margin: 0 !important; letter-spacing: -0.01em; }
      
      /* Tables */
      .table { background: transparent !important; border-radius: 16px !important; overflow: hidden !important; box-shadow: 0 4px 16px rgba(0, 0, 0, 0.04) !important; }
      .table th { background: rgba(255, 255, 255, 0.6) !important; border: none !important; padding: 16px !important; font-weight: 600 !important; font-size: 14px !important; color: var(--apple-text) !important; letter-spacing: -0.01em; }
      .table td { background: rgba(255, 255, 255, 0.3) !important; border: 1px solid rgba(0, 0, 0, 0.04) !important; padding: 16px !important; font-size: 15px !important; color: var(--apple-text) !important; }
      
      /* Tabs */
      .nav-tabs { border: none !important; background: rgba(255, 255, 255, 0.3) !important; backdrop-filter: blur(15px) !important; -webkit-backdrop-filter: blur(15px) !important; border-radius: 20px !important; padding: 8px !important; margin-bottom: 24px !important; }
      .nav-tabs .nav-link { border: none !important; border-radius: 16px !important; padding: 12px 24px !important; font-weight: 500 !important; font-size: 15px !important; color: var(--apple-text-secondary) !important; background: transparent !important; transition: all 0.2s ease !important; letter-spacing: -0.01em; }
      .nav-tabs .nav-link.active { background: rgba(255, 255, 255, 0.8) !important; color: var(--apple-text) !important; font-weight: 600 !important; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08) !important; }
      .nav-tabs .nav-link:hover:not(.active) { background: rgba(255, 255, 255, 0.5) !important; color: var(--apple-text) !important; }
      
      /* Leaflet container polish */
      .leaflet-container { border-radius: 20px !important; overflow: hidden !important; box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.1) !important; }
      
      /* Hide default Shiny error traces (keeps UI clean for end-users) */
      .shiny-output-error { display: none; } .shiny-output-error:before { display: none; }
      
      /* Responsive tweaks */
      @media (max-width: 768px) {
        .content-wrapper { padding: 16px !important; }
        .content-box { padding: 24px !important; margin-bottom: 16px !important; }
        .app-title { font-size: 28px !important; }
        .stat-card { margin-bottom: 16px !important; }
      }
      
      /* Entrance animation */
      @keyframes fadeInUp { from { opacity: 0; transform: translateY(20px); } to { opacity: 1; transform: translateY(0); } }
      .fade-in-up { animation: fadeInUp 0.6s cubic-bezier(0.25, 0.46, 0.45, 0.94); }
    "))
  ),
  
  #  App Header 
  div(class = "app-header",
      div(class = "app-title", "London Crime Intelligence")
  ),
  
  #  Main Content wrapper 
  div(class = "content-wrapper",
      # Top Row: controls | main map | risk panel
      fluidRow(
        # Left column: borough selection + emergency button
        column(3,
               div(class = "control-panel fade-in-up",
                   div(
                     h4("Location Selection", style = "margin-bottom: 24px; font-weight: 600; font-size: 20px; color: var(--apple-text);"),
                     selectInput(
                       "borough_select",
                       label = "Choose Borough",
                       choices = sort(unique(combined$BoroughName)),
                       selected = "Westminster",
                       width = "100%"
                     )
                   ),
                   br(),
                   actionButton("emergency_button",
                                "Emergency Assistance",
                                class = "emergency-btn")
               )
        ),
        
        # Middle column: primary map view
        column(6,
               div(class = "content-box fade-in-up",
                   div(class = "box-header",
                       h3(class = "box-title", "Crime Distribution Map")
                   ),
                   leafletOutput("crime_map", height = "500px")
               )
        ),
        
        # Right column: risk quicklook + stat card
        column(3,
               div(class = "content-box fade-in-up",
                   div(class = "box-header",
                       h3(class = "box-title", "Risk Assessment")
                   ),
                   h5("Current Location", style = "color: var(--apple-text-secondary); margin-bottom: 8px; font-weight: 500;"),
                   verbatimTextOutput("current_location", placeholder = TRUE),
                   br(),
                   h5("Risk Level", style = "color: var(--apple-text-secondary); margin-bottom: 16px; font-weight: 500;"),
                   uiOutput("risk_indicator"),  # pill showing LOW/MEDIUM/HIGH
                   br(),
                   h5("Risk Score", style = "color: var(--apple-text-secondary); margin-bottom: 8px; font-weight: 500;"),
                   verbatimTextOutput("risk_score", placeholder = TRUE),
                   br(),
                   
                   # Small stat card (relative to London avg)
                   div(class = "stat-card",
                       div(class = "stat-value", textOutput("vs_avg")),
                       div(class = "stat-label", "vs London Avg")
                   )
               )
        )
      ),
      
      # Full width: tabs with all visualisations
      fluidRow(
        column(12,
               div(class = "content-box fade-in-up",
                   div(class = "box-header",
                       h3(class = "box-title", "All Visualizations")
                   ),
                   tabsetPanel(
                     tabPanel("Spatial Distribution", leafletOutput("spatial_analysis", height = "400px")),  # sampled crime dots with category legend
                     tabPanel("Crime Hotspots", leafletOutput("hotspot_map", height = "400px")),            # kernel density heat layer
                     tabPanel("Crime Types", plotlyOutput("crime_breakdown", height = "400px")),           # pie breakdown (simple buckets)
                     tabPanel("Area Comparison", plotlyOutput("area_comparison", height = "400px")),       # bar vs neighbors + avg
                     tabPanel("IMD Analysis", plotlyOutput("imd_scatter", height = "400px")),              # IMD vs Frequency scatter
                     tabPanel("Employment Analysis", plotlyOutput("unemp_scatter", height = "400px")),     # Unemp vs Frequency scatter
                     tabPanel("Statistics Overview",
                              fluidRow(
                                column(3,
                                       h5("Current Statistics", style = "color: var(--apple-text-secondary); font-weight: 600; margin-bottom: 16px;"),
                                       tableOutput("current_stats")
                                ),
                                column(3,
                                       h5("Historical Trends", style = "color: var(--apple-text-secondary); font-weight: 600; margin-bottom: 16px;"),
                                       tableOutput("historical_comparison")
                                ),
                                column(3,
                                       h5("Nearby Areas", style = "color: var(--apple-text-secondary); font-weight: 600; margin-bottom: 16px;"),
                                       tableOutput("nearby_areas")
                                ),
                                column(3,
                                       h5("Data Sources", style = "color: var(--apple-text-secondary); font-weight: 600; margin-bottom: 16px;"),
                                       verbatimTextOutput("data_info")
                                )
                              )
                     )
                   )
               )
        )
      ),
      
      # Emergency info block at the bottom (static content)
      fluidRow(
        column(12,
               div(class = "content-box fade-in-up",
                   div(class = "box-header",
                       h3(class = "box-title", "Emergency")
                   ),
                   div(style = "background: rgba(255, 59, 48, 0.1); border: 1px solid rgba(255, 59, 48, 0.2); border-radius: 16px; padding: 20px; margin-bottom: 24px;",
                       h4("Emergency Contacts", style = "color: #FF3B30; margin-bottom: 16px; font-weight: 600;"),
                       p(strong("999"), " - Police, Fire, Ambulance (Emergency)", style = "margin-bottom: 8px;"),
                       p(strong("101"), " - Police Non-Emergency Line", style = "margin-bottom: 8px;"),
                       p(strong("0800 555 111"), " - Crimestoppers (Anonymous)", style = "margin-bottom: 0;")
                       
                   )
               )
        )
      )
  )
)




server <- function(input, output, session) {
  # Reactive container for the current location and risk state
  values <- reactiveValues(
    current_coords = NULL,   # c(lat, lng)
    risk_data = NULL,        # list with level + scores
    location_name = NULL     # current borough name
  )
  
  # Helper to update everything when a borough changes (center, label, risk)
  update_for_borough <- function(borough_name) {
    borough_row <- combined |> filter(BoroughName == borough_name)
    if (nrow(borough_row) == 0) return()  # safety check: unknown borough
    
    # Grab centroid in WGS84 to center maps (coords order from st_coordinates is x=lng, y=lat)
    coords <- st_coordinates(london_sf$centroid[london_sf$BoroughName == borough_name])
    
    values$location_name <- borough_row$BoroughName
    values$current_coords <- c(coords[2], coords[1])  # store as (lat, lng) for convenience
    values$risk_data <- list(
      risk_level = as.character(borough_row$RiskTier),
      risk_score = borough_row$RiskScore10,
      raw_score  = borough_row$RiskScore
    )
  }
  
  # Initialisation: set a default borough on first load
  observeEvent(TRUE, {
    if (is.null(values$location_name)) {
      update_for_borough("Westminster")
    }
  }, once = TRUE)
  
  # Whenever the user picks a borough, refresh the state
  observeEvent(input$borough_select, {
    req(input$borough_select)
    update_for_borough(input$borough_select)
  })
  
  # Emergency button pops up a helpful modal with phone numbers
  observeEvent(input$emergency_button, {
    showModal(modalDialog(
      title = "Emergency Assistance",
      div(style = "background: rgba(255, 59, 48, 0.1); border: 1px solid rgba(255, 59, 48, 0.2); border-radius: 16px; padding: 20px;",
          h4("Immediate Emergency: Call 999", style = "color: #FF3B30; margin-bottom: 16px;"),
          p("For immediate police, fire, or medical emergency"),
          br(),
          h4("Your Current Analysis Location:", style = "margin-bottom: 8px;"),
          p(ifelse(is.null(values$location_name),
                   "No location analyzed yet",
                   values$location_name)),
          br(),
          p("Non-Emergency Police: 101"),
          p("Anonymous Crime Reporting: 0800 555 111")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #  Main map: shows a blue halo at the currently analysed location 
  output$crime_map <- renderLeaflet({
    if (!is.null(values$current_coords)) {
      center_lat <- values$current_coords[1]
      center_lng <- values$current_coords[2]
      zoom <- 12
    } else {
      center_lat <- 51.5074   # fall back to central London
      center_lng <- -0.1278
      zoom <- 11
    }
    
    m <- leaflet() |>
      addTiles() |>
      setView(lng = center_lng, lat = center_lat, zoom = zoom)
    
    # Draw a big circle marker as the "focus" for the selected borough
    if (!is.null(values$current_coords)) {
      m <- m |> addCircleMarkers(
        lng = values$current_coords[2],
        lat = values$current_coords[1],
        radius = 30,
        color = "#007AFF",
        fillColor = "#007AFF",
        fillOpacity = 0.3,
        stroke = TRUE,
        weight = 3,
        popup = paste("Analysis Location:", values$location_name)
      )
    }
    m
  })
  
  #  Tiny readouts on the right panel 
  output$current_location <- renderText({
    if (is.null(values$location_name)) {
      "No location analyzed"
    } else {
      paste("Location:", values$location_name)
    }
  })
  
  # Pill-style risk indicator (LOW/MEDIUM/HIGH) with colour
  output$risk_indicator <- renderUI({
    if (is.null(values$risk_data)) {
      p("Analyze a location to see risk assessment")
    } else {
      risk_class <- switch(values$risk_data$risk_level,
                           "HIGH" = "risk-indicator risk-high",
                           "MEDIUM" = "risk-indicator risk-medium",
                           "LOW" = "risk-indicator risk-low")
      div(class = risk_class, values$risk_data$risk_level)
    }
  })
  
  output$risk_score <- renderText({
    if (is.null(values$risk_data)) {
      "No analysis available"
    } else {
      paste0(values$risk_data$risk_score, " / 10")
    }
  })
  
  # mean monthly crimes for current borough
  output$month_avg <- renderText({
    if (is.null(values$location_name)) return("-")
    borough_row <- combined |> filter(BoroughName == values$location_name)
    if (nrow(borough_row) == 0) return("-")
    format(round(borough_row$CrimeFrequency, 1), big.mark = ",")
  })
  
  # Show % difference vs London average crime frequency
  output$vs_avg <- renderText({
    if (is.null(values$location_name)) return("-")
    borough_row <- combined |> filter(BoroughName == values$location_name)
    london_avg <- mean(combined$CrimeFrequency)
    diff_pct <- (borough_row$CrimeFrequency - london_avg) / london_avg * 100
    sign <- ifelse(diff_pct >= 0, "+", "")
    paste0(sign, round(diff_pct, 1), "%")
  })
  
  #  Tab: Spatial Distribution (sample points with type-coloured markers) 
  output$spatial_analysis <- renderLeaflet({
    # Filter to selected borough if present; otherwise show a random sample overall
    if (!is.null(values$location_name)) {
      pts <- real_points |> filter(BoroughName == values$location_name)
    } else {
      pts <- real_points
    }
    
    # If no data, just return a plain map centered on London
    if (nrow(pts) == 0) {
      m <- leaflet() |> addTiles() |> setView(lng = -0.1278, lat = 51.5074, zoom = 11)
      return(m)
    }
    
    # Sample to keep the map responsive (cap at 300)
    plot_data <- pts[sample(nrow(pts), min(300, nrow(pts))), ]
    
    # Colour palette per category buckets
    crime_colors <- c(
      "Theft"    = "#007AFF",
      "Violence" = "#FF3B30",
      "Vehicle"  = "#FF9500",
      "Drugs"    = "#34C759",
      "Other"    = "#AF52DE"
    )
    pal <- colorFactor(
      palette  = unname(crime_colors),
      domain   = names(crime_colors),
      na.color = "#8E8E93"
    )
    
    # Center on selected borough centroid if available
    if (!is.null(values$location_name)) {
      coords <- st_coordinates(london_sf$centroid[london_sf$BoroughName == values$location_name])
      center_lat <- coords[2]
      center_lng <- coords[1]
      zoom_level <- 11
    } else {
      center_lat <- 51.5074
      center_lng <- -0.1278
      zoom_level <- 11
    }
    
    leaflet(plot_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level) |>
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = 6,
        color = "white",              # white stroke for contrast on light basemap
        weight = 2,
        fillColor = ~pal(crime_type),
        fillOpacity = 0.8,
        stroke = TRUE,
        popup = ~paste("Crime Type:", crime_type, "<br>",
                       "Date:", date)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~crime_type,
        title = "Crime Types",
        opacity = 0.8
      )
  })
  
  #  Tab: Crime Hotspots (kernel density heat)- using spatstat + raster 
  output$hotspot_map <- renderLeaflet({
    # Work with either selected borough or all points
    if (!is.null(values$location_name)) {
      pts <- real_points |> filter(BoroughName == values$location_name)
      center <- st_coordinates(london_sf$centroid[london_sf$BoroughName == values$location_name])
      center_lng <- center[1]
      center_lat <- center[2]
    } else {
      pts <- real_points
      center_lng <- -0.1278
      center_lat <- 51.5074
    }
    req(nrow(pts) > 0)  # must have points
    
    # Convert to sf and project to British National Grid for spatial kernel calc
    crimes_wgs84 <- st_as_sf(pts, coords = c("lng", "lat"), crs = 4326, remove = FALSE)
    crimes_bng  <- st_transform(crimes_wgs84, 27700)
    xy <- st_coordinates(crimes_bng)
    
    # Set the window (study area) and a smoothing bandwidth (sigma)
    if (!is.null(values$location_name)) {
      borough_geom_bng <- st_transform(london_sf$geometry[london_sf$BoroughName == values$location_name], 27700)
      win <- as.owin(borough_geom_bng)
      range_x <- diff(range(xy[,1]))
      range_y <- diff(range(xy[,2]))
      sigma <- max(range_x, range_y) / 20   # borough-level: slightly tighter kernel
    } else {
      bb   <- st_bbox(crimes_bng)
      pad  <- 500  # pad a bit so kernel isn't clipped hard at edges
      win  <- owin(xrange = c(bb["xmin"] - pad, bb["xmax"] + pad),
                   yrange = c(bb["ymin"] - pad, bb["ymax"] + pad))
      range_x <- diff(range(xy[,1]))
      range_y <- diff(range(xy[,2]))
      sigma <- max(range_x, range_y) / 30   # city-wide: slightly wider kernel
    }
    
    # Build a point pattern object for spatstat, then density raster
    pp <- ppp(x = xy[,1], y = xy[,2], window = win)
    dens  <- density.ppp(pp, sigma = sigma, at = "pixels")
    
    # Convert to raster and reproject back to WGS84 for Leaflet
    r_bng    <- raster(dens)
    crs(r_bng) <- CRS("+init=epsg:27700")   # classic CRS string (works; can be modernised if desired)
    r_wgs84  <- projectRaster(r_bng, crs = CRS("+init=epsg:4326"))
    
    # Defensive cleanup for weird numeric ranges
    v <- values(r_wgs84)
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NULL)
    v[v < 0] <- 0
    dom_min <- min(v, na.rm = TRUE)
    dom_max <- max(v, na.rm = TRUE)
    if (!is.finite(dom_min) || (dom_max - dom_min) < 1e-12) {
      dom_max <- dom_min + 1e-6
    }
    
    pal_num <- colorNumeric(
      palette  = c("#007AFF", "#5AC8FA", "#FF9500", "#FF3B30"),
      domain   = c(dom_min, dom_max),
      na.color = "transparent"
    )
    
    leaflet() |>
      addTiles() |>
      setView(lng = center_lng, lat = center_lat,
              zoom = ifelse(is.null(values$location_name), 10, 12)) |>
      addRasterImage(r_wgs84, colors = pal_num, opacity = 0.4, project = FALSE)
  })
  
  #  Tab: Crime Types (Pie chart of simplified categories) 
  output$crime_breakdown <- renderPlotly({
    # Use current borough if set
    if (!is.null(values$location_name)) {
      df <- real_points |> filter(BoroughName == values$location_name)
    } else {
      df <- real_points
    }
    
    # If nothing to show, empty chart with a title
    if (nrow(df) == 0) {
      return(plotly_empty(type = "pie") |> layout(title = "No data available"))
    }
    
    # Map free-form crime_type into 5 simple buckets
    df <- df |> mutate(SimpleCat = category_map(crime_type))
    
    # Count each bucket
    crime_summary <- df |>
      group_by(SimpleCat) |>
      summarise(count = n(), .groups = 'drop')
    
    # Make sure all five buckets exist (fill missing as zeros)
    all_cats <- data.frame(SimpleCat = c("Theft", "Violence", "Vehicle", "Drugs", "Other"), stringsAsFactors = FALSE)
    crime_summary <- full_join(all_cats, crime_summary, by = "SimpleCat") |>
      mutate(count = replace_na(count, 0))
    
    # Nice set of colours that match other elements
    apple_colors <- c("#007AFF", "#FF3B30", "#FF9500", "#34C759", "#AF52DE")
    
    plot_ly(crime_summary, labels = ~SimpleCat, values = ~count, type = 'pie',
            marker = list(colors = apple_colors,
                          line = list(color = '#FFFFFF', width = 2))) |>
      layout(title = list(text = "Crime Type Distribution", 
                          font = list(family = "Inter", size = 18, color = "#1D1D1F")),
             showlegend = TRUE,
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  #  Tab: Area Comparison (bars for current, 2 next, London avg) 
  output$area_comparison <- renderPlotly({
    # Pick a default if nothing selected yet
    if (is.null(values$location_name)) {
      target <- "Westminster"
    } else {
      target <- values$location_name
    }
    
    # Order boroughs by risk and find where the target sits
    ordered <- combined |> arrange(desc(RiskScore10))
    idx <- which(ordered$BoroughName == target)
    
    # Grab the next two entries as comparisons (below in rank)
    comps <- c(idx + 1, idx + 2)
    comps <- comps[comps <= nrow(ordered)]
    comp_names <- ordered$BoroughName[comps]
    comp_scores <- ordered$RiskScore10[comps]
    
    # London average (simple mean of scores)
    london_avg <- round(mean(combined$RiskScore10), 1)
    
    # Build plotting frame
    area_data <- data.frame(
      Area = c("Current Area", comp_names, "London Average"),
      Risk_Score = c(round(ordered$RiskScore10[ordered$BoroughName == target], 1), round(comp_scores, 1), london_avg)
    )
    
    colors <- c('#007AFF', '#FF9500', '#AF52DE', '#34C759')
    
    plot_ly(area_data, x = ~Area, y = ~Risk_Score, type = 'bar',
            marker = list(color = colors), hoverinfo = 'text',
            text = ~paste0(Area, ": ", Risk_Score, "/10")) |>
      layout(title = list(text = "Risk Score Comparison",
                          font = list(family = "Inter", size = 18, color = "#1D1D1F")),
             yaxis = list(title = "Risk Score",
                          titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
             xaxis = list(titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  #  Tab: Stats (current 30d totals, risk score, placeholder trend) 
  output$current_stats <- renderTable({
    if (nrow(real_points) == 0) return(data.frame(Metric = c("Total Incidents", "Risk Score", "Trend"), Value = c("0", "-", "-") ))
    
    # Look at the most recent 30 days overall or within selected borough
    last_date <- max(real_points$date, na.rm = TRUE)
    window_start <- last_date - 30
    df <- real_points |> filter(date >= window_start)
    
    # If we have a selected borough, narrow it down
    if (!is.null(values$location_name)) {
      df <- df |> filter(BoroughName == values$location_name)
      risk_score <- combined$RiskScore10[combined$BoroughName == values$location_name]
    } else {
      risk_score <- mean(combined$RiskScore10)
    }
    
    total_incidents <- nrow(df)
    data.frame(
      Metric = c("Total Incidents", "Risk Score", "Trend"),
      Value = c(paste(total_incidents, "(last 30 days)"),
                paste0(round(risk_score, 1), " / 10"),
                "-"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = 's')
  
  #  Tab: Historical comparison (toy values derived from current score) 
  output$historical_comparison <- renderTable({
    # Use current borough score (or mean) as "current"
    if (!is.null(values$location_name)) {
      borough_row <- combined |> filter(BoroughName == values$location_name)
      current <- round(borough_row$RiskScore10, 1)
    } else {
      current <- round(mean(combined$RiskScore10), 1)
    }
    
    # Fake comparisons (as placeholders): 10% lower (last year), 20% lower (3yr avg)
    last_year <- max(1, round(current * 0.9, 1))
    three_year <- max(1, round(current * 0.8, 1))
    change1 <- paste0(round((current - last_year)/last_year * 100, 1), "%")
    change2 <- paste0(round((current - three_year)/three_year * 100, 1), "%")
    
    data.frame(
      Period = c("Last Month", "Last Year", "3 Year Avg"),
      Risk = c(paste0(current, "/10"), paste0(last_year, "/10"), paste0(three_year, "/10")),
      Change = c("", change1, change2),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = 's')
  
  #  Tab: Nearby areas (3 closest centroids) 
  output$nearby_areas <- renderTable({
    if (is.null(values$location_name)) return(NULL)
    
    target <- values$location_name
    target_centroid <- london_sf$centroid[london_sf$BoroughName == target]
    
    # Distances in meters (since CRS is WGS84, st_distance returns meters between points)
    dists <- st_distance(target_centroid, london_sf$centroid)
    dists <- as.numeric(dists)
    names(dists) <- london_sf$BoroughName
    dists <- dists[names(dists) != target]
    
    # Take 3 nearest and compare risk levels
    nearest <- head(sort(dists), 3)
    names_near <- names(nearest)
    risk_scores <- combined$RiskScore10[match(names_near, combined$BoroughName)]
    target_score <- combined$RiskScore10[combined$BoroughName == target]
    status <- ifelse(risk_scores > target_score, "Higher", ifelse(risk_scores < target_score, "Lower", "Equal"))
    
    data.frame(
      Area = names_near,
      Distance = paste0(round(nearest / 1000, 2), " km"),
      Risk = paste0(round(risk_scores, 1), "/10"),
      Status = status,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = 's')
  
  #  Tab: Data sources (static blurb for now) 
  output$data_info <- renderText({
    date_range <- range(crime_points$date, na.rm = TRUE)
    paste(
      "Data Sources:\n• MPS Recorded Crime: Geographic Breakdown\n• Indices of Multiple Deprivation 2019\n• Trust for London unemployment rates\n"
    )
  })
  
  #  Tab: IMD scatter (labels shown for selected borough) 
  output$imd_scatter <- renderPlotly({
    df <- combined |> mutate(
      Selected = BoroughName == values$location_name,
      RiskColour = case_when(
        RiskTier == "LOW" ~ "#34C759",
        RiskTier == "MEDIUM" ~ "#FF9500",
        TRUE ~ "#FF3B30"
      )
    )
    
    plot_ly(
      df,
      x = ~IMD_Score, y = ~CrimeFrequency,
      type = 'scatter', mode = 'markers+text',  # text used only for the selected borough
      color = ~RiskTier, colors = c('#FF3B30', '#34C759', '#FF9500'),  #  mapping is order-dependent
      size = ~ifelse(Selected, 50, 1), sizes = c(8, 32),               # big bubble for selected
      text = ~ifelse(Selected, BoroughName, ""),                        # show label only for the selected
      textposition = 'top center',
      textfont = list(size = 16, color = '#1D1D1F', family = 'Inter'),
      marker = list(opacity = 0.8, line = list(color = 'white', width = 2))
    ) |>
      layout(
        title = list(text = 'IMD Score vs Crime Frequency',
                     font = list(family = "Inter", size = 18, color = "#1D1D1F")),
        xaxis = list(title = 'IMD Average Score',
                     titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
        yaxis = list(title = 'Average Monthly Crimes',
                     titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  #  Tab: Unemployment scatter (same idea as IMD) 
  output$unemp_scatter <- renderPlotly({
    df <- combined |> mutate(
      Selected = BoroughName == values$location_name,
      RiskColour = case_when(
        RiskTier == "LOW" ~ "#34C759",
        RiskTier == "MEDIUM" ~ "#FF9500",
        TRUE ~ "#FF3B30"
      )
    )
    
    plot_ly(df, x = ~UnemploymentRate, y = ~CrimeFrequency, type = 'scatter', mode = 'markers',
            color = ~RiskTier, colors = c('#FF3B30', '#34C759', '#FF9500'),
            size = ~ifelse(Selected, 50, 1), sizes = c(8, 32),
            text = ~ifelse(Selected, BoroughName, ""),
            textposition = 'top center',
            textfont = list(size = 16, color = '#1D1D1F', family = 'Inter'),
            marker = list(opacity = 0.8, line = list(color = 'white', width = 2))) |>
      layout(title = list(text = 'Unemployment Rate vs Crime Frequency',
                          font = list(family = "Inter", size = 18, color = "#1D1D1F")),
             xaxis = list(title = 'Unemployment Rate (%)',
                          titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
             yaxis = list(title = 'Average Monthly Crimes',
                          titlefont = list(family = "Inter", size = 14, color = "#1D1D1F")),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
}

#  Helper to bucket raw crime_type strings into 5 simple categories 
category_map <- function(x) {
  x <- tolower(x)
  case_when(
    grepl("theft", x)   ~ "Theft",
    grepl("robbery", x) ~ "Violence",
    grepl("violence", x)~ "Violence",
    grepl("assault", x) ~ "Violence",
    grepl("vehicle", x) ~ "Vehicle",
    grepl("drug", x)    ~ "Drugs",
    TRUE                ~ "Other"
  )
}

shinyApp(ui = ui, server = server)


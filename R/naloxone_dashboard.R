library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)
library(stringr)

# ==============================================================================
# DATA
# ==============================================================================

facility_data <- data.frame(
    facility = c(
        "If other Please specify", "Collegiate Recovery", "Community Organization",
        "Quick Response Team (QRT)", "Health Department", "Harm Reduction Program",
        "Licensed Behavioral Health Facility", "MAT Program",
        "Crisis Stabilization Unit", "Residential Program", "Pharmacy", "First Responder"
    ),
    count = c(58000, 32000, 31000, 30000, 26000, 22000, 18000, 12000, 5000, 3000, 2200, 1800),
    stringsAsFactors = FALSE
)

ndc_data <- data.frame(
    ndc = c(
        "NDC# 69547-353-02 Nasal Spray 4mg, (2 doses/kit)",
        "NDC# 69547-627-02 Narcan Nasal 4mg/0.1mL (OTC)",
        "NDC# 45802-578-87 Naloxone Nasal Spray 4mg (Padagis)",
        "NDC# 59467-679-01 Kloxxado (naloxone 8mg/0.1mL)",
        "NDC# 00093-2165-68 Teva (generic naloxone) 4 mg/0.1 mL x 2",
        "NDC# 76329-3369-01 Naloxone HCl 1mg/mL"
    ),
    count = c(95000, 62000, 38000, 7500, 6000, 4500),
    stringsAsFactors = FALSE
)

county_data <- data.frame(
    County = c(
        "Barbour", "Berkeley", "Boone", "Braxton", "Brooke",
        "Cabell", "Calhoun", "Clay", "Doddridge", "Fayette",
        "Gilmer", "Grant", "Greenbrier", "Hampshire", "Hancock", "Hardy"
    ),
    Kits = c(
        157, 16260, 1053, 885, 259,
        41163, 45, 458, 93, 11104,
        124, 1368, 4200, 857, 450, 1387
    ),
    stringsAsFactors = FALSE
)
names(county_data)[2] <- "Number of Naloxone Kits"

set.seed(42)
dates <- seq(as.Date("2022-01-01"), as.Date("2026-01-01"), by = "month")
monthly_counts <- c(
    round(runif(12, 200, 3500)),
    round(runif(12, 200, 4500)),
    round(runif(12, 300, 4800)),
    round(c(runif(9, 300, 4000), 7000, 26000, 5000)),
    27000
)
monthly_data <- data.frame(
    date  = dates,
    count = monthly_counts[seq_along(dates)]
)

# ==============================================================================
# PLOT HELPERS
# ==============================================================================

bar_color  <- "#1e5f8e"
text_muted <- "#6b7c93"
text_dark  <- "#1a2b3c"

theme_wv <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
        theme(
            plot.background    = element_rect(fill = "white", color = NA),
            panel.background   = element_rect(fill = "white", color = NA),
            panel.grid.major.x = element_line(color = "#e8ecf0", linewidth = 0.4),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text          = element_text(color = text_muted, size = 9),
            axis.title         = element_blank(),
            plot.title         = element_text(
                color = text_dark, size = 11, face = "bold",
                margin = margin(b = 8)
            ),
            plot.margin     = margin(12, 12, 8, 12),
            legend.position = "none"
        )
}

make_facility_plot <- function() {
    df <- facility_data
    df$facility <- factor(df$facility, levels = df$facility[order(df$count)])
    ggplot(df, aes(x = count, y = facility)) +
        geom_col(fill = bar_color, width = 0.65) +
        scale_x_continuous(
            labels = label_number(scale = 1e-3, suffix = "k"),
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(title = "Number of Naloxone Kits Distributed by Type of Facility") +
        theme_wv()
}

make_ndc_plot <- function() {
    df <- ndc_data
    df$label <- str_wrap(df$ndc, 42)
    df$label  <- factor(df$label, levels = df$label[order(df$count)])
    ggplot(df, aes(x = count, y = label)) +
        geom_col(fill = bar_color, width = 0.65) +
        scale_x_continuous(
            labels = label_number(scale = 1e-3, suffix = "k"),
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(title = "Number of Naloxone Kits Distributed by NDC") +
        theme_wv() +
        theme(axis.text.y = element_text(size = 7.5, lineheight = 1.1))
}

make_monthly_plot <- function() {
    ggplot(monthly_data, aes(x = date, y = count)) +
        geom_line(color = bar_color, linewidth = 0.8) +
        geom_point(
            color = bar_color, size = 2.2,
            fill = "white", shape = 21, stroke = 1.5
        ) +
        scale_x_date(
            date_breaks  = "6 months",
            date_labels  = "%b %Y",
            expand       = expansion(mult = 0.01)
        ) +
        scale_y_continuous(
            labels = label_number(scale = 1e-3, suffix = "k"),
            limits = c(0, 30000),
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(title = "Number of Naloxone Kits Distributed by Month and Year") +
        theme_wv() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#e8ecf0", linewidth = 0.4)
        )
}

# ==============================================================================
# CSS  -- plain string, no sprintf, no percent placeholders
# ==============================================================================

css_styles <- paste0(
    "@import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:wght",
    "@400;600;700&family=Merriweather+Sans:wght@700;800&display=swap');",
    "
* { box-sizing: border-box; }
body {
  font-family: 'Source Sans 3', sans-serif;
  background-color: #eef1f5;
  margin: 0; padding: 0;
  color: #1a2b3c;
}
.dash-header {
  background-color: white;
  border-bottom: 2px solid #dde3ea;
  padding: 14px 24px;
  display: flex; align-items: center; justify-content: space-between;
  margin-bottom: 16px;
}
.dash-header h1 {
  font-family: 'Merriweather Sans', sans-serif;
  font-size: 20px; font-weight: 700;
  color: #122840; margin: 0;
}
.date-badge {
  background: #f0f4f8;
  border: 1px solid #cdd5df;
  border-radius: 6px; padding: 6px 14px;
  font-size: 13px; color: #6b7c93; font-weight: 600;
}
.kpi-card {
  background: #1a3a5c;
  border-radius: 10px; color: white;
  padding: 32px 24px 28px; text-align: center;
  min-height: 340px;
  display: flex; flex-direction: column;
  align-items: center; justify-content: center;
}
.kpi-title  { font-size: 18px; font-weight: 600; line-height: 1.3; margin-bottom: 18px; opacity: 0.92; }
.kpi-number { font-family: 'Merriweather Sans', sans-serif; font-size: 88px; font-weight: 800; line-height: 1; margin-bottom: 16px; }
.kpi-subtitle { font-size: 15px; opacity: 0.88; line-height: 1.4; }
.kpi-updated  { font-size: 11px; opacity: 0.55; margin-top: 18px; }
.kpi-tabs {
  display: flex; gap: 2px; margin-top: 20px;
  border-bottom: 2px solid rgba(255,255,255,0.25);
  width: 100%; justify-content: center;
}
.kpi-tab {
  padding: 7px 18px; border: none; background: none;
  font-size: 13px; font-weight: 600;
  color: rgba(255,255,255,0.6); cursor: pointer;
  border-bottom: 2px solid transparent; margin-bottom: -2px;
}
.kpi-tab.active { color: white; border-bottom-color: white; }
.panel-card {
  background: white; border-radius: 10px;
  padding: 16px 12px 10px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.07);
  margin-bottom: 16px;
}
.updated-text { font-size: 10.5px; color: #9aabbf; margin-top: 4px; padding-left: 4px; }
.county-wrap {
  background: white; border-radius: 10px;
  padding: 12px; box-shadow: 0 1px 4px rgba(0,0,0,0.07);
  margin-bottom: 16px;
}
.county-wrap table.dataTable thead th {
  background: #f5f7fa; color: #1a2b3c;
  font-weight: 700; font-size: 12px;
  border-bottom: 2px solid #dde3ea !important;
}
.county-wrap .dataTables_info,
.county-wrap .dataTables_paginate { font-size: 11px; }
.total-row {
  background: #f5f7fa; border-top: 2px solid #dde3ea;
  padding: 8px 12px; font-weight: 700; font-size: 13px; color: #1a2b3c;
  display: flex; justify-content: space-between;
  margin-top: 4px; border-radius: 0 0 6px 6px;
}
.dash-body { padding: 0 16px 16px; }
"
)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
    title = "Community Naloxone Reporting Dashboard",
    
    tags$head(tags$style(css_styles)),
    
    div(class = "dash-header",
        h1("Community Naloxone Reporting Dashboard"),
        div(
            style = "display:flex; align-items:center; gap:16px;",
            tags$img(
                src    = paste0(
                    "https://upload.wikimedia.org/wikipedia/commons/thumb/",
                    "4/47/West_Virginia_state_seal.svg/",
                    "120px-West_Virginia_state_seal.svg.png"
                ),
                height = "42px",
                style  = "opacity:0.8;"
            ),
            div(class = "date-badge", "1/1/2022 - 2/22/2026")
        )
    ),
    
    div(class = "dash-body",
        
        fluidRow(
            column(4,
                   div(class = "panel-card",
                       plotOutput("facilityPlot", height = "340px"),
                       div(class = "updated-text", "Last update: just now")
                   )
            ),
            column(4,
                   div(class = "kpi-card",
                       div(class = "kpi-title",    "Number of Kits Left At Scene"),
                       div(class = "kpi-number",   "93"),
                       div(class = "kpi-subtitle", "of Suspected Non-fatal Overdose"),
                       div(class = "kpi-updated",  "Last update: just now"),
                       div(class = "kpi-tabs",
                           tags$button(class = "kpi-tab active", "Overall Count"),
                           tags$button(class = "kpi-tab",        "County Table")
                       )
                   )
            ),
            column(4,
                   div(class = "panel-card",
                       plotOutput("ndcPlot", height = "340px"),
                       div(class = "updated-text", "Last update: just now")
                   )
            )
        ),
        
        fluidRow(
            column(8,
                   div(class = "panel-card",
                       plotOutput("monthlyPlot", height = "320px"),
                       div(class = "updated-text", "Last update: just now")
                   )
            ),
            column(4,
                   div(class = "county-wrap",
                       DTOutput("countyTable"),
                       div(class = "total-row",
                           span("Total"),
                           span("164,464")
                       ),
                       div(class = "updated-text", "Last update: just now")
                   )
            )
        )
    )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
    
    output$facilityPlot <- renderPlot({
        make_facility_plot()
    }, bg = "white", res = 110)
    
    output$ndcPlot <- renderPlot({
        make_ndc_plot()
    }, bg = "white", res = 110)
    
    output$monthlyPlot <- renderPlot({
        make_monthly_plot()
    }, bg = "white", res = 110)
    
    output$countyTable <- renderDT({
        datatable(
            county_data,
            options  = list(
                pageLength     = 12,
                dom            = "t",
                scrollY        = "260px",
                scrollCollapse = TRUE,
                ordering       = FALSE,
                columnDefs     = list(list(className = "dt-right", targets = 1))
            ),
            rownames  = FALSE,
            selection = "none",
            class     = "stripe hover compact"
        ) |>
            formatCurrency("Number of Naloxone Kits", currency = "", digits = 0)
    })
}

# ==============================================================================
# RUN
# ==============================================================================

shinyApp(ui = ui, server = server)
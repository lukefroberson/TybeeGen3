# ============================================================================
# TAB 3: HISTORIC DATA - COMPREHENSIVE HISTORICAL ANALYSIS
# ============================================================================
# This tab includes time series visualization and extensive advisory history
# Shows trends, patterns, and context for water quality over 20+ years
# ============================================================================

# Georgia CFU Threshold
GEORGIA_THRESHOLD <- 70

# ===== UI COMPONENT =====

tab3_ui <- tabItem(
  tabName = "timeseries",  # Keep existing tabName for compatibility
  
  # Header
  fluidRow(
    box(
      width = 12,
      title = NULL,
      status = "info",
      solidHeader = FALSE,
      div(
        style = "text-align: center; padding: 15px; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); border-radius: 8px;",
        h2(style = "margin: 0 0 10px 0; color: white;", 
           icon("chart-line"), " Historic Beach Water Quality Data"),
        h4(style = "margin: 0; color: white; font-weight: normal;",
           "20+ years of monitoring shows Tybee beaches are consistently safe")
      )
    )
  ),
  
  # ===== SECTION 1: TIME SERIES EXPLORER =====
  fluidRow(
    box(
      width = 12,
      title = "Water Quality Time Series",
      status = "primary",
      solidHeader = TRUE,
      icon = icon("chart-area"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      p(style = "font-size: 14px; margin-bottom: 15px;",
        "Explore historical bacteria levels at each beach. Use the controls below to customize the view 
        and see how water quality has varied over time."),
      
      fluidRow(
        column(3, 
               selectInput("site_select", 
                          "Select Beach:", 
                          choices = NULL,
                          selected = NULL)
        ),
        column(6,
               div(
                 h5("Date Range:", style = "margin-bottom: 5px;"),
                 uiOutput("date_slider_ui")
               )
        ),
        column(3,
               checkboxInput("show_rainfall", "Show 3-Day Rainfall", value = FALSE),
               checkboxInput("show_water_temp", "Show Water Temperature", value = TRUE),
               checkboxInput("show_threshold", 
                           paste0("Show ", GEORGIA_THRESHOLD, " CFU Threshold"), 
                           value = TRUE)
        )
      ),
      
      plotlyOutput("bacteria_time_plot", height = 450),
      
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
        fluidRow(
          column(6,
                 tags$b("Legend:"),
                 tags$ul(
                   style = "font-size: 13px; margin-top: 5px;",
                   tags$li(tags$span(style = "color: #28a745;", "●"), 
                          " No Advisory (≤ ", GEORGIA_THRESHOLD, " CFU/100mL)"),
                   tags$li(tags$span(style = "color: #dc3545;", "●"), 
                          " Advisory Level (> ", GEORGIA_THRESHOLD, " CFU/100mL)")
                 )
          ),
          column(6,
                 div(style = "text-align: right; padding-top: 10px;",
                     downloadButton("download_data", "Download Data", 
                                   style = "background-color: #0066cc; color: white;")
                 )
          )
        )
      ),
      
      tags$hr(),
      
      h4("Data Table", style = "margin-top: 20px;"),
      p(style = "font-size: 13px; color: #666;",
        "Detailed measurements for the selected beach and date range. Click column headers to sort."),
      DT::dataTableOutput("site_data_table")
    )
  ),
  
  # ===== SECTION 2: ADVISORY HISTORY & ANALYSIS =====
  fluidRow(
    box(
      width = 12,
      title = "Historic Beach Advisories & Closures",
      status = "danger",
      solidHeader = TRUE,
      icon = icon("exclamation-triangle"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      p(style = "font-size: 14px; margin-bottom: 15px;",
        "Historical record of bacterial level advisories for Tybee Island monitoring sites based on 
        levels exceeding ", GEORGIA_THRESHOLD, " CFUs/100mL."),
      
      # Filter Controls
      fluidRow(
        column(3,
               selectInput("advisory_site", "Select Beach Site:", 
                          choices = NULL, selected = NULL)
        ),
        column(3,
               selectInput("advisory_year", "Select Year:", 
                          choices = NULL, selected = NULL)
        ),
        column(3,
               selectInput("advisory_duration_filter", "Duration Filter:", 
                          choices = c("All Durations", "1 Day", "2 Days", "3-7 Days", "8-30 Days", "30+ Days"),
                          selected = "All Durations")
        ),
        column(3,
               br(),
               downloadButton("download_advisory_data", "Download Advisory Data", 
                             class = "btn-success", icon = icon("download"))
        )
      ),
      
      tags$hr(),
      
      p(style = "color: #666; font-size: 14px;",
        icon("info-circle"), " Data source: Georgia Department of Natural Resources - Coastal Resources Division")
    )
  ),
  
  # Advisory Timeline and Statistics
  fluidRow(
    box(
      title = "Advisory Timeline", 
      width = 8, 
      status = "warning", 
      solidHeader = TRUE,
      icon = icon("clock"),
      plotlyOutput("advisory_timeline_plot", height = 400),
      p(style = "font-size: 13px; color: #666;",
        "Timeline showing bacterial level advisories over time. Each point represents an advisory period. 
        Point size indicates duration.")
    ),
    
    box(
      title = "Advisory Statistics", 
      width = 4, 
      status = "info", 
      solidHeader = TRUE,
      icon = icon("calculator"),
      uiOutput("advisory_stats"),
      tags$hr(),
      h5("Duration Distribution"),
      plotOutput("advisory_duration_dist_plot", height = 200)
    )
  ),
  
  # Detailed Advisory Table
  fluidRow(
    box(
      title = "Individual Advisory Events", 
      width = 12, 
      status = "primary", 
      solidHeader = TRUE,
      icon = icon("table"),
      DT::dataTableOutput("advisory_details_table"),
      p(style = "margin-top: 10px; font-size: 12px;",
        strong("Note: "), "All advisories shown are for bacterial contamination (levels exceeding 70 CFUs/100mL)")
    )
  ),
  
  # Annual Trends and Site Comparison
  fluidRow(
    box(
      title = "Annual Advisory Trends", 
      width = 6, 
      status = "success", 
      solidHeader = TRUE,
      icon = icon("chart-line"),
      plotlyOutput("annual_advisory_plot", height = 400),
      p(style = "font-size: 13px; color: #666;",
        "Number of advisories and total days affected by year.")
    ),
    
    box(
      title = "Advisory Duration by Site", 
      width = 6, 
      status = "success", 
      solidHeader = TRUE,
      icon = icon("water"),
      plotlyOutput("site_duration_plot", height = 400),
      p(style = "font-size: 13px; color: #666;",
        "Average advisory duration and frequency by monitoring location. Bubble size represents total days affected.")
    )
  ),
  
  # Seasonal Patterns and Duration Categories
  fluidRow(
    box(
      title = "Seasonal Advisory Patterns", 
      width = 6, 
      status = "warning", 
      solidHeader = TRUE,
      icon = icon("calendar"),
      plotlyOutput("seasonal_advisory_plot", height = 400),
      p(style = "font-size: 13px; color: #666;",
        "When advisories are most likely to occur during the year. Summer months typically show higher frequency.")
    ),
    
    box(
      title = "Advisory Duration Categories", 
      width = 6, 
      status = "warning", 
      solidHeader = TRUE,
      icon = icon("hourglass-half"),
      plotlyOutput("duration_category_plot", height = 400),
      p(style = "font-size: 13px; color: #666;",
        "Distribution of advisory lengths across all sites and years. Most advisories are short-lived (1-2 days).")
    )
  ),
  
  # ===== SECTION 3: DATA QUALITY & NOTES =====
  fluidRow(
    box(
      width = 12,
      title = "Data Quality & Technical Notes",
      status = "success",
      solidHeader = TRUE,
      icon = icon("info-circle"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      fluidRow(
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("database"), " Data Sources"),
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li("Beach water quality data: ", 
                            strong("Georgia Coastal Resources Division")),
                     tags$li("Testing frequency: ", 
                            strong("Weekly (April-October), Bi-weekly (November-March)")),
                     tags$li("Laboratory: ", 
                            strong("EPA-certified analysis methods")),
                     tags$li("Data span: ", 
                            strong("2004-present (~20 years)")),
                     tags$li("Total samples: ", 
                            strong(textOutput("total_samples_stat", inline = TRUE)))
                   )
               )
        ),
        
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("clipboard-check"), " Quality Assurance"),
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li("All samples collected using EPA protocols"),
                     tags$li("Chain of custody maintained for all samples"),
                     tags$li("Results reviewed by trained staff"),
                     tags$li("Data validated before public release"),
                     tags$li("Historical records archived and preserved")
                   ),
                   
                   div(
                     style = "margin-top: 15px; padding: 10px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("award"),
                       " Tybee Island has one of the most comprehensive beach monitoring programs 
                       in Georgia, with 20+ years of high-quality data.")
                   )
               )
        )
      ),
      
      tags$hr(),
      
      div(style = "padding: 15px;",
          h4(icon("file-download"), " Data Files Used"),
          p(style = "font-size: 13px;",
            "This dashboard uses the following data files maintained by Georgia Coastal Resources Division:"),
          tags$ul(
            style = "font-size: 13px;",
            tags$li(code("NorthBeach.xlsx"), " - North Beach monitoring site"),
            tags$li(code("MiddleBeach.xlsx"), " - Middle Beach monitoring site"),
            tags$li(code("SouthBeach.xlsx"), " - South Beach monitoring site"),
            tags$li(code("PolkStreet.xlsx"), " - Polk Street monitoring site"),
            tags$li(code("AdvisoryLengthByYear.xlsx"), " - Annual advisory statistics"),
            tags$li(code("BeachAdvisoryDuration.xlsx"), " - Advisory duration data")
          )
      )
    )
  )
)


# ===== SERVER COMPONENT =====

tab3_server <- function(input, output, session, site_data, beaches, advisory_datasets) {
  
  # Update site selection choices
  observe({
    updateSelectInput(session, "site_select", 
                     choices = beaches,
                     selected = beaches[1])
  })
  
  # Update advisory filter choices
  observe({
    datasets <- advisory_datasets()
    
    if (!is.null(datasets$details) && nrow(datasets$details) > 0) {
      # Get unique sites from advisory data
      site_names <- c("All Sites", sort(unique(datasets$details$MonitoringLocationName)))
      years <- c("All Years", sort(unique(datasets$details$Year), decreasing = TRUE))
      
      updateSelectInput(session, "advisory_site", choices = site_names, selected = "All Sites")
      updateSelectInput(session, "advisory_year", choices = years, selected = "All Years")
    } else {
      updateSelectInput(session, "advisory_site", choices = c("No Data Available"), selected = "No Data Available")
      updateSelectInput(session, "advisory_year", choices = c("No Data Available"), selected = "No Data Available")
    }
  })
  
  # Create date slider UI
  output$date_slider_ui <- renderUI({
    req(site_data())
    
    all_dates <- site_data()$SampleDate
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)
    
    # Default to last 2 years
    default_start <- max(min_date, max_date - lubridate::years(2))
    
    sliderInput("date_range",
                NULL,
                min = min_date,
                max = max_date,
                value = c(default_start, max_date),
                timeFormat = "%b %Y",
                width = "100%")
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    req(input$site_select, input$date_range, site_data())
    
    site_data() %>%
      filter(MonitoringLocationName == input$site_select,
             SampleDate >= input$date_range[1],
             SampleDate <= input$date_range[2]) %>%
      arrange(SampleDate)
  })
  
  # Filter advisory data
  filtered_advisory_data <- reactive({
    datasets <- advisory_datasets()
    
    if (is.null(datasets$details)) {
      return(data.frame())
    }
    
    data <- datasets$details
    
    # Apply site filter
    if (!is.null(input$advisory_site) && input$advisory_site != "All Sites") {
      data <- data %>% filter(MonitoringLocationName == input$advisory_site)
    }
    
    # Apply year filter
    if (!is.null(input$advisory_year) && input$advisory_year != "All Years") {
      data <- data %>% filter(Year == as.numeric(input$advisory_year))
    }
    
    # Apply duration filter
    if (!is.null(input$advisory_duration_filter) && input$advisory_duration_filter != "All Durations") {
      data <- data %>% filter(DurationCategory == input$advisory_duration_filter)
    }
    
    return(data)
  })
  
  # Main time series plot
  output$bacteria_time_plot <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data()
    
    p <- plot_ly(data, x = ~SampleDate) %>%
      add_trace(y = ~Enterococcus,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Bacteria Level',
               line = list(color = '#0066cc'),
               marker = list(
                 size = 8,
                 color = ~ifelse(Enterococcus > GEORGIA_THRESHOLD, '#dc3545', '#28a745')
               ),
               hovertemplate = paste(
                 '<b>Date:</b> %{x|%b %d, %Y}<br>',
                 '<b>Level:</b> %{y:.0f} CFU/100mL<br>',
                 '<extra></extra>'
               ))
    
    # Add threshold line if selected
    if (!is.null(input$show_threshold) && input$show_threshold) {
      p <- p %>%
        add_trace(y = GEORGIA_THRESHOLD,
                 type = 'scatter',
                 mode = 'lines',
                 name = paste(GEORGIA_THRESHOLD, 'CFU Threshold'),
                 line = list(color = '#ffc107', dash = 'dash', width = 2),
                 hoverinfo = 'skip')
    }
    
    # Add rainfall if selected and available
    if (!is.null(input$show_rainfall) && input$show_rainfall && "Rainfall_3day" %in% names(data)) {
      p <- p %>%
        add_trace(y = ~Rainfall_3day * 50,  # Scale for visibility
                 type = 'bar',
                 name = '3-Day Rainfall (scaled)',
                 marker = list(color = 'rgba(100, 149, 237, 0.3)'),
                 yaxis = 'y2',
                 hovertemplate = paste(
                   '<b>Rainfall:</b> %{y:.2f} inches<br>',
                   '<extra></extra>'
                 ))
    }
    
    # Add water temp if selected and available
    if (!is.null(input$show_water_temp) && input$show_water_temp && "WaterTemp" %in% names(data)) {
      p <- p %>%
        add_trace(y = ~WaterTemp,
                 type = 'scatter',
                 mode = 'lines',
                 name = 'Water Temp (°F)',
                 line = list(color = '#17a2b8', dash = 'dot'),
                 yaxis = 'y2',
                 hovertemplate = paste(
                   '<b>Water Temp:</b> %{y:.1f}°F<br>',
                   '<extra></extra>'
                 ))
    }
    
    p <- p %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Bacteria Level (CFU/100mL)"),
        yaxis2 = list(
          overlaying = 'y',
          side = 'right',
          title = 'Rainfall / Temperature'
        ),
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(x = 0, y = 1)
      )
    
    p
  })
  
  # Data table
  output$site_data_table <- renderDT({
    req(filtered_data())
    
    data <- filtered_data() %>%
      select(Date = SampleDate, 
             `Bacteria (CFU)` = Enterococcus) %>%
      mutate(
        Status = ifelse(`Bacteria (CFU)` > GEORGIA_THRESHOLD, 
                       "Advisory Level", 
                       "No Advisory")
      )
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        order = list(list(0, 'desc'))  # Sort by date descending
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('No Advisory', 'Advisory Level'),
          c('#d4edda', '#f8d7da')
        )
      )
  })
  
  # Download handler for site data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$site_select, "_", 
             format(input$date_range[1], "%Y%m%d"), "_to_", 
             format(input$date_range[2], "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # ===== ADVISORY VISUALIZATIONS =====
  
  # Advisory Statistics
  output$advisory_stats <- renderUI({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(div(p("No advisories found for selected criteria.")))
    }
    
    total_advisories <- nrow(data)
    total_days <- sum(data$ActionDurationDays, na.rm = TRUE)
    avg_duration <- round(mean(data$ActionDurationDays, na.rm = TRUE), 1)
    median_duration <- median(data$ActionDurationDays, na.rm = TRUE)
    max_duration <- max(data$ActionDurationDays, na.rm = TRUE)
    
    # Get date range
    date_range <- range(data$ActionStartDate, na.rm = TRUE)
    
    div(
      h5("Summary Statistics"),
      p(strong("Total Advisories: "), total_advisories),
      p(strong("Total Days Affected: "), total_days),
      p(strong("Average Duration: "), avg_duration, " days"),
      p(strong("Median Duration: "), median_duration, " days"),
      p(strong("Longest Advisory: "), max_duration, " days"),
      tags$hr(),
      h6("Data Period:"),
      p(format(date_range[1], "%b %Y"), " to ", format(date_range[2], "%b %Y"))
    )
  })
  
  # Advisory timeline plot
  output$advisory_timeline_plot <- renderPlotly({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No advisory data available for selected filters",
                 x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
               ) %>%
               layout(title = "Beach Advisory Timeline"))
    }
    
    # Create color mapping based on duration
    duration_colors <- c("1 Day" = "#FFF2CC", "2 Days" = "#FFE599", "3-7 Days" = "#FFD966", 
                         "8-30 Days" = "#FF9900", "30+ Days" = "#CC0000", "Unknown" = "#CCCCCC")
    
    # Filter out any rows with missing essential data
    data <- data %>% 
      filter(!is.na(ActionStartDate), !is.na(MonitoringLocationName), !is.na(ActionDurationDays))
    
    if (nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No valid advisory data after filtering",
                 x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
               ) %>%
               layout(title = "Beach Advisory Timeline"))
    }
    
    p <- plot_ly(data, x = ~ActionStartDate, y = ~MonitoringLocationName, 
                 color = ~DurationCategory, colors = duration_colors,
                 type = 'scatter', mode = 'markers',
                 marker = list(size = ~pmax(8, pmin(20, ActionDurationDays)), 
                               line = list(width = 1, color = 'black')),
                 text = ~paste("Site:", MonitoringLocationName,
                               "<br>Start:", format(ActionStartDate, "%Y-%m-%d"),
                               "<br>End:", ifelse(!is.na(ActionEndDate), format(ActionEndDate, "%Y-%m-%d"), "Unknown"), 
                               "<br>Duration:", ActionDurationDays, "days"),
                 hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = "Beach Advisory Timeline",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Beach Site"),
        showlegend = TRUE,
        legend = list(title = list(text = "Duration Category"))
      )
    
    return(p)
  })
  
  # Duration distribution plot (small chart in stats box)
  output$advisory_duration_dist_plot <- renderPlot({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + theme_void() + ggtitle("No data available"))
    }
    
    duration_counts <- table(data$DurationCategory)
    duration_df <- data.frame(
      Category = factor(names(duration_counts), 
                        levels = c("1 Day", "2 Days", "3-7 Days", "8-30 Days", "30+ Days")),
      Count = as.numeric(duration_counts)
    )
    
    ggplot(duration_df, aes(x = Category, y = Count, fill = Category)) +
      geom_col() +
      scale_fill_manual(values = c("1 Day" = "#FFF2CC", "2 Days" = "#FFE599", 
                                   "3-7 Days" = "#FFD966", "8-30 Days" = "#FF9900", 
                                   "30+ Days" = "#CC0000")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "none",
            plot.margin = unit(c(5, 5, 5, 5), "pt")) +
      labs(x = "", y = "Count")
  })
  
  # Detailed advisory table
  output$advisory_details_table <- renderDT({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No advisory data available for selected filters"), 
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    # Format data for display
    display_data <- data %>%
      select(MonitoringLocationName, ActionStartDate, ActionEndDate, ActionDurationDays, Year) %>%
      arrange(desc(ActionStartDate))
    
    names(display_data) <- c("Beach Site", "Start Date", "End Date", "Duration (days)", "Year")
    
    datatable(
      display_data,
      options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Duration (days)",
        backgroundColor = styleInterval(
          c(2, 7, 30),
          c("#FFF2CC", "#FFE599", "#FF9900", "#CC0000")
        )
      ) %>%
      formatDate(columns = c("Start Date", "End Date"), method = "toLocaleDateString")
  })
  
  # Annual advisory trends
  output$annual_advisory_plot <- renderPlotly({
    datasets <- advisory_datasets()
    
    if (is.null(datasets$summary)) {
      return(plot_ly() %>%
               add_annotations(
                 text = "Annual summary data not available",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ))
    }
    
    # Filter summary data if specific site selected
    summary_data <- datasets$summary
    if (!is.null(input$advisory_site) && input$advisory_site != "All Sites") {
      summary_data <- summary_data %>% 
        filter(MonitoringLocationName == input$advisory_site)
    }
    
    # Aggregate by year if multiple sites
    annual_summary <- summary_data %>%
      group_by(Year) %>%
      summarise(
        TotalAdvisories = sum(BeachAdvisories, na.rm = TRUE),
        TotalDays = sum(TotalDaysWithAdvisories, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Year))
    
    p <- plot_ly(annual_summary, x = ~Year) %>%
      add_trace(y = ~TotalAdvisories, type = 'scatter', mode = 'lines+markers',
                name = 'Number of Advisories', line = list(color = 'blue')) %>%
      add_trace(y = ~TotalDays, type = 'scatter', mode = 'lines+markers',
                name = 'Total Days with Advisories', yaxis = 'y2', 
                line = list(color = 'red')) %>%
      layout(
        title = "Annual Advisory Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Advisories", side = "left", color = "blue"),
        yaxis2 = list(title = "Total Days", side = "right", overlaying = "y", color = "red"),
        hovermode = 'x unified'
      )
    
    return(p)
  })
  
  # Site duration comparison
  output$site_duration_plot <- renderPlotly({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No data available",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ))
    }
    
    site_summary <- data %>%
      group_by(MonitoringLocationName) %>%
      summarise(
        AvgDuration = mean(ActionDurationDays, na.rm = TRUE),
        TotalAdvisories = n(),
        TotalDays = sum(ActionDurationDays, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- plot_ly(site_summary, x = ~TotalAdvisories, y = ~AvgDuration, 
                 size = ~TotalDays, sizes = c(10, 100),
                 text = ~paste("Site:", MonitoringLocationName,
                               "<br>Advisories:", TotalAdvisories,
                               "<br>Avg Duration:", round(AvgDuration, 1), "days",
                               "<br>Total Days:", TotalDays),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_markers(marker = list(sizemode = 'diameter', opacity = 0.7, color = 'coral')) %>%
      layout(
        title = "Advisory Frequency vs Duration by Site",
        xaxis = list(title = "Number of Advisories"),
        yaxis = list(title = "Average Duration (days)")
      )
    
    return(p)
  })
  
  # Seasonal patterns
  output$seasonal_advisory_plot <- renderPlotly({
    data <- filtered_advisory_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No data available",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ))
    }
    
    # Count advisories by month
    monthly_counts <- data %>%
      group_by(Month) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      complete(Month = 1:12, fill = list(Count = 0))
    
    month_names <- month.abb
    monthly_counts$MonthName <- factor(month_names[monthly_counts$Month], 
                                       levels = month_names)
    
    p <- plot_ly(monthly_counts, x = ~MonthName, y = ~Count, type = 'bar',
                 marker = list(color = 'lightcoral')) %>%
      layout(
        title = "Advisory Frequency by Month",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of Advisories")
      )
    
    return(p)
  })
  
  # Duration category analysis
  output$duration_category_plot <- renderPlotly({
    datasets <- advisory_datasets()
    
    if (is.null(datasets$summary)) {
      return(plot_ly() %>%
               add_annotations(
                 text = "Duration category data not available",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ))
    }
    
    # Check what columns are actually available
    available_cols <- names(datasets$summary)
    
    # Try to find the duration columns with flexible naming
    duration_cols <- list()
    
    # Look for 1 day columns
    if ("X1dayadvisories" %in% available_cols) {
      duration_cols[["1 Day"]] <- "X1dayadvisories"
    } else if ("1dayadvisories" %in% available_cols) {
      duration_cols[["1 Day"]] <- "1dayadvisories"
    } else if (any(grepl("1.*day", available_cols, ignore.case = TRUE))) {
      duration_cols[["1 Day"]] <- available_cols[grepl("1.*day", available_cols, ignore.case = TRUE)][1]
    }
    
    # Look for 2 day columns
    if ("X2dayadvisories" %in% available_cols) {
      duration_cols[["2 Days"]] <- "X2dayadvisories"
    } else if ("2dayadvisories" %in% available_cols) {
      duration_cols[["2 Days"]] <- "2dayadvisories"
    } else if (any(grepl("2.*day", available_cols, ignore.case = TRUE))) {
      duration_cols[["2 Days"]] <- available_cols[grepl("2.*day", available_cols, ignore.case = TRUE)][1]
    }
    
    # Look for 3-7 day columns
    if ("X37dayadvisories" %in% available_cols) {
      duration_cols[["3-7 Days"]] <- "X37dayadvisories"
    } else if ("3-7dayadvisories" %in% available_cols) {
      duration_cols[["3-7 Days"]] <- "3-7dayadvisories"
    } else if (any(grepl("3.*7.*day", available_cols, ignore.case = TRUE))) {
      duration_cols[["3-7 Days"]] <- available_cols[grepl("3.*7.*day", available_cols, ignore.case = TRUE)][1]
    }
    
    # Look for 8-30 day columns
    if ("X830dayadvisories" %in% available_cols) {
      duration_cols[["8-30 Days"]] <- "X830dayadvisories"
    } else if ("8-30dayadvisories" %in% available_cols) {
      duration_cols[["8-30 Days"]] <- "8-30dayadvisories"
    } else if (any(grepl("8.*30.*day", available_cols, ignore.case = TRUE))) {
      duration_cols[["8-30 Days"]] <- available_cols[grepl("8.*30.*day", available_cols, ignore.case = TRUE)][1]
    }
    
    # Look for >30 day columns
    if ("greaterthan30dayadvisories" %in% available_cols) {
      duration_cols[["30+ Days"]] <- "greaterthan30dayadvisories"
    } else if (any(grepl("greater.*30", available_cols, ignore.case = TRUE))) {
      duration_cols[["30+ Days"]] <- available_cols[grepl("greater.*30", available_cols, ignore.case = TRUE)][1]
    } else if (any(grepl("30.*day", available_cols, ignore.case = TRUE))) {
      duration_cols[["30+ Days"]] <- available_cols[grepl("30.*day", available_cols, ignore.case = TRUE)][1]
    }
    
    if (length(duration_cols) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No duration category columns found in data",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ))
    }
    
    # Calculate duration summary using available columns
    duration_values <- list()
    for (cat_name in names(duration_cols)) {
      col_name <- duration_cols[[cat_name]]
      if (col_name %in% names(datasets$summary)) {
        duration_values[[cat_name]] <- sum(datasets$summary[[col_name]], na.rm = TRUE)
      } else {
        duration_values[[cat_name]] <- 0
      }
    }
    
    duration_summary <- data.frame(
      Duration = names(duration_values),
      Count = unlist(duration_values),
      stringsAsFactors = FALSE
    )
    
    duration_summary$Duration <- factor(duration_summary$Duration, 
                                        levels = c("1 Day", "2 Days", "3-7 Days", "8-30 Days", "30+ Days"))
    
    p <- plot_ly(duration_summary, x = ~Duration, y = ~Count, type = 'bar',
                 marker = list(color = c("#FFF2CC", "#FFE599", "#FFD966", "#FF9900", "#CC0000"))) %>%
      layout(
        title = "Advisory Distribution by Duration Category",
        xaxis = list(title = "Duration Category"),
        yaxis = list(title = "Number of Advisories")
      )
    
    return(p)
  })
  
  # Download handler for advisory data
  output$download_advisory_data <- downloadHandler(
    filename = function() {
      paste0("advisory-data-", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_advisory_data(), file, row.names = FALSE)
    }
  )
  
  # Total samples stat
  output$total_samples_stat <- renderText({
    req(site_data())
    format(nrow(site_data()), big.mark = ",")
  })
}


# ===== INTEGRATION NOTES =====
# 
# TO USE:
# 1. This tab uses the existing tabName "timeseries" for compatibility
# 2. Source this file in your main app.R
# 3. Replace the existing tabItem(tabName = "timeseries", ...) with tab3_ui
# 4. Call tab3_server with your site_data reactive, beaches vector, and advisory_datasets reactive
# 5. Make sure Excel files (including AdvisoryLengthByYear.xlsx and BeachAdvisoryDuration.xlsx) 
#    are in www/ directory
#
# PARAMETERS NEEDED:
# - site_data: reactive containing all beach water quality data
# - beaches: vector of beach names for dropdown
# - advisory_datasets: reactive containing advisory data (summary and details)

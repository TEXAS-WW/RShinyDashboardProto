################# BEGINNING OF SERVER LOGIC OF SHINY #################
# Load required variables for the application
#source("variables.R")

# Define the server function with inputs, outputs, and session information
server <- function(input, output, session) {
  
  ##### BEGINNING OF LEAFLET MAP SETUP #####
  # This block sets up the initial display of the Leaflet map.
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      setView(lng = -99.56666, lat = 31, zoom = 8)  # Center the map on Texas
  })
  
  # Observe changes in county selection to update the map dynamically.
  observeEvent(input$countySelect, {
    if (input$countySelect == "None") {
      # No specific county selected; show all available data.
      temp_filtered_data = merged_CountyWWTP %>% 
        filter(!is.na(totalWWTP)) # Filter to include only entries with data available
      # Create popup labels for the map using HTML for better formatting
      labels <- paste(
        "<strong>",
        temp_filtered_data$NAMELSAD,
        "</strong><br>Numbers of Participating Site:",
        temp_filtered_data$totalWWTP
      ) %>%
        lapply(htmltools::HTML)
      
      # Update the map view
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        # Add the shapefiles for Texas boundaries
        addPolygons(
          data = texas_boundary,
          color = "#000000",
          weight = 3,
          fill = FALSE
        ) %>%
        # Add the shapefiles for the WWTP
        addPolygons(
          data = merged_CountyWWTP %>%
            filter(is.na(totalWWTP)),
          fillColor = ~ Color,
          fillOpacity = ~ FillOpacity,
          color = "black",
          weight = ~ Weight
        ) %>%
        addPolygons(
          data = merged_CountyWWTP %>%
            filter(!is.na(totalWWTP)),
          fillColor = ~ Color,
          fillOpacity = ~ FillOpacity,
          color = "black",
          weight = ~ Weight,
          label = ~ labels,
          labelOptions = labelOptions(
            noHide = FALSE,
            direction = "center",
            textsize = "12px",
            style = list(
              "color" = "black",
              "font-family" = "Arial",
              "font-style" = "normal",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "12px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        setView(lng = -99.56666,
                lat = 31,
                zoom = 6)
    } else {
      county_selected <- WWTP[which(WWTP$County == input$countySelect), ]
      
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        addProviderTiles("CartoDB.Positron") %>%
        # A specific county is selected; focus the map view on it.
        setView(
          lng = unique(WWTP$county_centroid_lon[which(WWTP$County == input$countySelect)]),
          lat = unique(WWTP$county_centroid_lat[which(WWTP$County == input$countySelect)]),
          zoom = 9
        )  %>%
        # Add markers at locations of the WWTP.
        addMarkers(
          data = county_selected,
          lng = county_selected$lon,
          lat = county_selected$lat,
          label = ~ paste0(WWTP[-1], ", ", City),
          labelOptions = labelOptions(
            noHide = FALSE,
            direction = "center",
            textsize = "12px",
            style = list(
              "color" = "black",
              "font-family" = "Arial",
              "font-style" = "normal",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "12px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        )
    }
  })
  
  ##### END OF LEAFLET MAP SET UP #####
  
  
  ##### BEGINNING OF CDS TAB SETUP #####
  
  
  
  # Setup reactive values to store the last selections for easier switching between views
  cds_lastSelected <-
    reactiveValues(
      city_variant = unique(WWTP$City[-1])[1],
      variant_variant = unique(major_path_expand_dt$species)[1],
      city_city = unique(WWTP$City[-1])[1],
      variant_city = unique(major_path_expand_dt$species)[1]
    )
  
  # Dynamically render user interface elements for the CDS (City/Variant) selections
  output$cdsSelectionUI <- renderUI({
    if (input$cdsViewType == 'variant') {
      isolate({
        cds_lastSelected$variant_variant <- input$cdsVariantInput_variant
        cds_lastSelected$city_variant <- input$cdsCityInput_variant
      })
      cds_selectedCity_variant <-
        if (!is.null(cds_lastSelected$city_variant))
          cds_lastSelected$city_variant
      else
        unique(WWTP$City[-1])[1]
      cds_selectedVariant_variant <-
        if (!is.null(cds_lastSelected$variant_variant))
          cds_lastSelected$variant_variant
      else
        unique(major_path_expand_dt$species)[1]
      
      div(
        selectInput(
          "cdsVariantInput_variant",
          "Select Variant:",
          choices = unique(major_path_expand_dt$species),
          selected = cds_selectedVariant_variant
        ),
        selectizeInput(
          "cdsCityInput_variant",
          "Select City(s):",
          choices = unique(WWTP$City[-1]),
          multiple = TRUE,
          selected = cds_selectedCity_variant
        )
      )
    } else if (input$cdsViewType == 'city') {
      isolate({
        cds_lastSelected$city_city <- input$cdsCityInput_city
        cds_lastSelected$variant_city <- input$cdsVariantInput_city
      })
      cds_selectedCity_city <-
        if (!is.null(cds_lastSelected$city_city))
          cds_lastSelected$city_city
      else
        unique(WWTP$City[-1])[1]
      cds_selectedVariant_city <-
        if (!is.null(cds_lastSelected$variant_city))
          cds_lastSelected$variant_city
      else
        unique(major_path_expand_dt$species)[1]
      
      div(
        selectInput(
          "cdsCityInput_city",
          "Select City:",
          choices = unique(WWTP$City[-1]),
          selected = cds_selectedCity_city
        ),
        selectizeInput(
          "cdsVariantInput_city",
          "Select Variant(s):",
          choices = unique(major_path_expand_dt$species),
          multiple = TRUE,
          selected = cds_selectedVariant_city
        )
      )
    }
  })
  
  
  
  ##### BEGINNING OF COLLECTION DATE PLOT SETUP #####
  # Aggregate data on detected strains per sample for plotting
  strains_per_sample_dt <- comb_tax_table %>%
    group_by(sample_ID) %>%
    summarize(detectedStrains = n_distinct(strain))
  
  # Count the number of unique sites for palette preparation
  number_of_sites <- length(unique(comb_metadata_table$Site))
  pal <- wes_palette("FantasticFox1", number_of_sites, type = "continuous")
  
  # Render the collection dates plot using Plotly based on the aggregated data
  output$collectionDatesPlot_cds <- renderPlotly({
    calendarp <- merge(comb_metadata_table, strains_per_sample_dt,
                       by = "sample_ID") %>%
      mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
      filter(gsub(",.*$", "", City) %in% input$cdsCity_collectiondate) %>%
      select(c(City, Site, Week, sample_ID, detectedStrains)) %>%
      distinct()
    
    plot <- ggplot(calendarp,
                   aes(
                     x = Week,
                     y = Site,
                     color = Site,
                     size = detectedStrains
                   )) +
      geom_point() +
      geom_line(size = 0.25, na.rm = TRUE) +
      scale_size_continuous(range = c(0.2, 3.5)) +
      facet_wrap(City ~ ., ncol = 1, scales = "free_y") +
      scale_color_manual(values = pal) +
      scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +  # Adjust date breaks
      scale_y_discrete(
        labels = function(x)
          str_trunc(x, width = 14)
      ) +
      theme_pubclean() +
      labs(x = "", y = "") +
      theme(
        axis.text.x = element_text(
          angle = 45,  # Rotate x-axis labels
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "Off"
      )
    
    ggplotly(
      plot,
      width = input$collectionDatesPlot_cds_dimension[1],
      height = input$collectionDatesPlot_cds_dimension[2]
    )
  })
  
  ##### END OF COLLECTION DATE PLOT SETUP #####
  
  
  ##### BEGINNING OF INTERACTIVE DATA TABLE SETUP FOR CDS TAB #####
  # Setup interactive table displaying trends and interpretations based on data filtered from the CDS trend plot
  
  output$cdsInteractiveTable <- renderReactable({
    trend_major_path_dt %>%
      select(
        c(
          City,
          Week,
          most_recent_week,
          species,
          difference,
          Interpretation,
          moving_average
        )
      ) %>%
      filter(Week == most_recent_week) %>%
      distinct() %>%
      mutate(
        interpretation_color = case_when(
          Interpretation == "Increase from Baseline" ~ "orangered",
          Interpretation == "Decrease from Baseline" ~ "cadetblue",
          Interpretation == "Little Change" ~ "grey60",
          Interpretation == "Constant at 0" ~ "white",
          Interpretation == "(Re)-emerging from 0" ~ "purple",
          Interpretation == "Going to 0" ~ "blue",
          TRUE ~ "black"
        ),
        difference = case_when(
          #difference == "-Inf" ~ 0,
          difference == "NaN" ~ 0,
          Interpretation == "Going to 0" ~ -1,
          Interpretation == "(Re)-emerging from 0" ~ 1,
          TRUE ~ difference
        ),
        difference = difference * 100
      ) %>%
      arrange(desc(difference)) %>%
      reactable(
        .,
        pagination = TRUE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 100),
        defaultPageSize = 10,
        defaultSorted = list(difference = "desc"),
        columns = list(
          difference = colDef(
            minWidth = 200,
            cell = data_bars(
              data = .,
              fill_color = c(mako(8), rev(magma(8))),
              background = '#F1F1F1',
              round_edges = F,
              text_position = 'outside-end',
              number_fmt = scales::label_number(style_positive = "plus", suffix = "%")
            )
          ),
          moving_average = colDef(
            name = "Abundance (moving average)",
            width = 100,
            style = color_scales(
              .,
              colors = c("grey", "gold", "maroon"),
              bias = 10
            ),
            format = colFormat(digits = 2)
          ),
          Interpretation = colDef(
            minWidth = 100,
            style = color_scales(., color_ref = "interpretation_color")
          ),
          species = colDef(minWidth = 100),
          Week = colDef(name = "Most Recent Week"),
          interpretation_color = colDef(show = FALSE),
          most_recent_week = colDef(show = FALSE)
        )
      ) %>%
      google_font(font_family = "Arial") %>%
      suppressWarnings()
    
    
  })
  
  
  
  ####### IMPORTANT PATHOGEN TABLE SETUP #####
  
  # Reactive value to store the filtered data
  filteredData <- reactive({
    data <- combined_react_data_plot
    
    # Filter data based on selected date range
    data <- data[data$Date >= input$cdsDateRange[1] & data$Date <= input$cdsDateRange[2], ]
    
    # Filter data based on selected city and variant if 'View By City' is chosen
    if (input$cdsViewType == 'city' && !is.null(input$citySelector) && !is.null(input$variantSelectorCity)) {
      data <- data[data$City == input$citySelector & data$sequence_name == input$variantSelectorCity, ]
    }
    
    # Filter data based on selected variant if 'View By Variant' is chosen
    if (input$cdsViewType == 'variant' && !is.null(input$variantSelector) && input$variantSelector != "All Variants") {
      data <- data[data$sequence_name == input$variantSelector, ]
    }
    
    return(data)
  })
  
  # Render the Trend Plot
  output$cds_TrendPlot <- renderPlotly({
    data <- filteredData()
    
    # Check if the data is empty and display a message if it is
    validate(
      need(nrow(data) > 0, "There is no data available for this variant in this city.")
    )
    
    city <- input$citySelector
    plot_title <- if (!is.null(city)) paste("Percent Covered in", city) else "Trend Plot"
    
    trend_plot <- data %>%
      ggplot(aes(x = Date, y = Percent_covered, color = sequence_name, group = sequence_name)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = plot_title, x = NULL, y = NULL, color = "Variant")
    
    ggplotly(trend_plot)
  })
  
  # Render the Trend Plot with y-axis from 0 to 1
  output$cds_TrendPlot_Y01 <- renderPlotly({
    data <- filteredData()
    
    # Check if the data is empty and display a message if it is
    validate(
      need(nrow(data) > 0, "There is no data available for this variant in this city.")
    )
    
    variant <- input$variantSelectorCity
    plot_title <- if (!is.null(variant)) paste("Percent Covered by", variant, "out of Total Virome") else "Trend Plot"
    
    trend_plot_y01 <- data %>%
      ggplot(aes(x = Date, y = Percent_covered, color = sequence_name, group = sequence_name)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = plot_title, x = NULL, y = NULL, color = "Variant")
    
    ggplotly(trend_plot_y01)
  })
  
  # Render the Date Range Text
  output$cds_DateRange <- renderText({
    data <- filteredData()
    actual_date_range <- if (nrow(data) > 0) {
      paste(min(data$Date), "to", max(data$Date))
    } else {
      "No data available"
    }
    
    paste("Selected Date Range:", input$cdsDateRange[1], "to", input$cdsDateRange[2], "      ",
          "Available Data Range:", actual_date_range)
  })
  
  # Render the dynamic UI for city or variant selection
  output$cdsSelectionUI <- renderUI({
    # Check if the selected view type is 'city'
    if (input$cdsViewType == 'city') {
      # If the view type is 'city', create a tagList containing two select input elements
      tagList(
        # Dropdown menu for selecting a city, with choices being the unique city names from the dataset
        selectInput("citySelector", "Select City:", choices = unique(combined_react_data_plot$City)),
        
        # Dropdown menu for selecting a variant within the selected city, with choices being the unique variant names from the dataset
        selectInput("variantSelectorCity", "Select Variant:", choices = unique(combined_react_data_plot$sequence_name))
      )
    } else {
      # If the view type is not 'city' (i.e., it is 'variant'), create a single select input element for selecting a variant
      selectInput("variantSelector", "Select Variant:", choices = c("All Variants", unique(combined_react_data_plot$sequence_name)))
    }
  })
  
  ###### END of IMPORTANT PATHOGEN TAB ######
  
  
  
  
  
  ##### BEGINNING OF QPCR SELECTION UI SETUP #####
  # Reactive values to store last user selections for quick updates
  qpcr_lastSelected <-
    reactiveValues(
      city_variant = gsub(",.*$", "", unique(qPCR_ma_p$City))[1],
      variant_variant = unique(qPCR_ma_p$Target)[1],
      city_city = gsub(",.*$", "", unique(qPCR_ma_p$City))[1],
      variant_city = unique(qPCR_ma_p$Target)[1]
    )
  
  # UI elements for qPCR selection, dynamically rendered based on the view type
  output$qpcrSelectionUI <- renderUI({
    if (input$qpcrViewType == 'variant') {
      isolate({
        qpcr_lastSelected$variant_variant <- input$qpcrVariantInput_variant
        qpcr_lastSelected$city_variant <- input$qpcrCityInput_variant
      })
      qpcr_selectedCity_variant <-
        if (!is.null(qpcr_lastSelected$city_variant))
          qpcr_lastSelected$city_variant
      else
        gsub(",.*$", "", unique(qPCR_ma_p$City))[1]
      qpcr_selectedVariant_variant <-
        if (!is.null(qpcr_lastSelected$variant_variant))
          qpcr_lastSelected$variant_variant
      else
        unique(qPCR_ma_p$Target)[1]
      
      div(
        selectInput(
          "qpcrVariantInput_variant",
          "Select Variant:",
          choices = unique(qPCR_ma_p$Target),
          selected = qpcr_selectedVariant_variant
        ),
        selectizeInput(
          "qpcrCityInput_variant",
          "Select City(s):",
          choices = gsub(",.*$", "", unique(qPCR_ma_p$City)),
          multiple = TRUE,
          selected = qpcr_selectedCity_variant
        )
      )
    } else if (input$qpcrViewType == 'city') {
      isolate({
        qpcr_lastSelected$city_city <- input$qpcrCityInput_city
        qpcr_lastSelected$variant_city <- input$qpcrVariantInput_city
      })
      qpcr_selectedCity_city <-
        if (!is.null(qpcr_lastSelected$city_city))
          qpcr_lastSelected$city_city
      else
        gsub(",.*$", "", unique(qPCR_ma_p$City))[1]
      qpcr_selectedVariant_city <-
        if (!is.null(qpcr_lastSelected$variant_city))
          qpcr_lastSelected$variant_city
      else
        unique(qPCR_ma_p$Target)[1]
      
      div(
        selectInput(
          "qpcrCityInput_city",
          "Select City:",
          choices = gsub(",.*$", "", unique(qPCR_ma_p$City)),
          selected = qpcr_selectedCity_city
        ),
        selectizeInput(
          "qpcrVariantInput_city",
          "Select Variant(s):",
          choices = unique(qPCR_ma_p$Target),
          multiple = TRUE,
          selected = qpcr_selectedVariant_city
        )
      )
    }
  })
  
  #### END OF QPCR SELECTION UI ####
  

  ##### BEGINNING OF QPCR TREND PLOT SETUP #####
  # Setup reactive data filtering based on user selections and date range for the QPCR trend plot
  
  # Setup reactive data filtering based on user selections and date range for the QPCR trend plot
  qpcr_filtered_data <- reactive({
    req(input$qpcrDateRange)
    
    dateFiltered_qPCR_ma_p <- qPCR_ma_p %>%
      filter(Week >= input$qpcrDateRange[1] &
               Week <= input$qpcrDateRange[2])
    
    if (input$qpcrViewType == 'city') {
      req(input$qpcrCityInput_city, input$qpcrVariantInput_city)
      
      df <- dateFiltered_qPCR_ma_p %>%
        filter(gsub(",.*$", "", City) %in% input$qpcrCityInput_city &
                 Target %in%  input$qpcrVariantInput_city) %>%
        filter(n_distinct(Week) >= 3) %>%
        drop_na() %>%
        ungroup()
    } else if (input$qpcrViewType == 'variant') {
      req(input$qpcrCityInput_variant, input$qpcrVariantInput_variant)
      
      df <- dateFiltered_qPCR_ma_p %>%
        filter(gsub(",.*$", "", City) %in% input$qpcrCityInput_variant &
                 Target %in%  input$qpcrVariantInput_variant) %>%
        filter(n_distinct(Week) >= 3) %>%
        drop_na() %>%
        ungroup()
    }
    print(head(df))  # Add logging to debug data
    return(df)
  })
  
  # Render the Plotly QPCR trend plot based on the selected view and filtered data
  output$qpcr_TrendPlot <- renderPlotly({
    req(nrow(qpcr_filtered_data()) > 2)  # Ensure filtered data is not empty
    
    if (input$qpcrViewType == 'city') {
      qpcr_plot <- qpcr_filtered_data() %>%
        ggplot(aes(x = Week, y = moving_average, color = City)) +
        geom_line(size = 1, alpha = 0.9) +
        geom_point(size = 0.75, alpha = 0.9) +
        scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL) +
        theme_bw() +
        ylab(NULL) +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      
      if (input$qpcrPlotToggle) {
        qpcr_plot <- qpcr_plot + facet_wrap(vars(Target), scales = "free_y")
      }
      
      ggplotly(
        qpcr_plot,
        width = input$qpcr_plot_dimension[1],
        height = input$qpcr_plot_dimension[2]
      ) %>%
        layout(yaxis = list(
          title = list(
            text = 'City-wide abundance (genome copies/sample)',
            xanchor = 'right',
            yanchor =  'center',
            standoff = 20
          )
        ))
    } else if (input$qpcrViewType == 'variant') {
      qpcr_plot <- qpcr_filtered_data() %>%
        ggplot(aes(x = Week, y = moving_average, color = City)) +
        geom_line(size = 1, alpha = 0.9) +
        geom_point(size = 0.75, alpha = 0.9) +
        scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL) +
        ylab(NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      
      if (input$qpcrPlotToggle) {
        qpcr_plot <- qpcr_plot + facet_wrap(vars(City), scales = "free_y")
      }
      
      ggplotly(
        qpcr_plot,
        width = input$qpcr_plot_dimension[1],
        height = input$qpcr_plot_dimension[2]
      ) %>%
        layout(yaxis = list(
          title = list(
            text = 'City-wide abundance (genome copies/sample)                                                    \n\n\n\n',
            xanchor = 'right',
            yanchor =  'center',
            standoff = 20
          )
        ))
    }
  })
  
  ##### END OF QPCR TREND PLOT SETUP #####
  
  
  ##### START of  Community Similarity Chart #####
  output$city_tsnep <- renderPlotly({
    pal <-
      wes_palette("Darjeeling1", WWTP_citieslength, type = "continuous")
    
    city_tsnep <- embb_dt %>%
      ggplot(aes(
        x = V1,
        y = V2,
        color = City,
        text = Date
      )) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = pal) +
      theme_bw() +
      labs(x = "t-SNE 1", y = "t-SNE 2")
    ggplotly(
      city_tsnep,
      width = input$city_tsnep_dimension[1],
      height = input$city_tsnep_dimension[2],
      tooltip = c("City", "Date")
    )
  })
  
  ##### End of  Community Similarity Chart
  
  #### VIRUS PLOT #####
  # Visualize data regarding virus prevalence and their metrics
  virus_df <- reactive({
    df <- prevalent_sp_dt %>%
      filter(moving_average != "NA" &
               City %in% input$virusCity &
               species %in% input$virus) %>%
      mutate(alabel = str_c(City, ", ", RPKMFS, "\n", Week))
    
  })
  
  output$virusPlot <- renderPlotly({
    virus_plot <- virus_df() %>%
      ggplot(aes(x = Week, y = moving_average, color = City)) +
      geom_line(size = 1, alpha = 0.9) +
      geom_point(size = 0.75, alpha = 0.9) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "City-wide abundance (RPKMF)") +
      theme_bw() +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ))
    
    if (input$virusPlotToggle) {
      virus_plot <- virus_plot + facet_wrap(vars(City),
                                            scales = "free_y")
      
    } else {
      virus_plot <- virus_plot
    }
    
    ggplotly(
      virus_plot,
      width = input$virusPlot_dimension[1],
      height = input$virusPlot_dimension[2]
    ) %>%
      layout(
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'City-Wide Abundance (RPKMF)')
      )
    
  })
  #### END VIRUS PLOT #####

}
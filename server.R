
################# BEGINNING OF SERVER LOGIC OF SHINY #################

server <- function(input, output, session) {
  
  output$cds_DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$cdsDateRange[2] > input$cdsDateRange[1], "End date is earlier than start date"
      )
    )

    
    # make sure end date later than start date
    validate(
      need(input$cdsDateRange[2] <= maxDate_cds, "Date is not available, please select Date Range based on the available sample collection dates below."),
      need(input$cdsDateRange[1] >= minDate_cds, "Date is not available, please select Date Range based on the available sample collection dates below." )
  )
    # 
    # # make sure end date later than start date
    # validate(
    #   need(input$cdsDateRange[2] %in% as.Date(major_path_expand_dt$Week), "Date is not available, please select Date Range based on the available sample collection dates below.")
    #   
    # )
    
    paste("Your date range is", 
          difftime(input$cdsDateRange[2], input$cdsDateRange[1], units="days"),
          "days")
  })
  
  output$qpcr_DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$qpcrDateRange[2] > input$qpcrDateRange[1], "End date is earlier than start date"
      )
    )
    
    
    # make sure end date later than start date
    validate(
      need(input$qpcrDateRange[2] <= maxDate_qpcr, "Date is not available, please select Date Range based on the available sample collection dates below."),
      need(input$qpcrDateRange[1] >= minDate_qpcr, "Date is not available, please select Date Range based on the available sample collection dates below." )
    )
    # 
    # # make sure end date later than start date
    # validate(
    #   need(input$cdsDateRange[2] %in% as.Date(major_path_expand_dt$Week), "Date is not available, please select Date Range based on the available sample collection dates below.")
    #   
    # )
    
    paste("Your date range is", 
          difftime(input$qpcrDateRange[2], input$qpcrDateRange[1], units="days"),
          "days")
  })
   
  ##### BEGINING OF LEAFLET MAP SET UP #####
  # Render the leaflet map in the first tab
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.56666, lat = 31, zoom = 8)
  })
   
  observeEvent(input$countySelect, {
    if (input$countySelect == "None") {
     
      temp_filtered_data = merged_CountyWWTP %>%
                        filter(!is.na(totalWWTP))
      # labels <- sprintf(paste0("<strong> ", "County" , " : %s </strong> <br/> Population: %s <br/> Adjusted screening rates: %s"), 
      #                   shp$GEOID, shp$npop, round(shp@data[,3]) ) %>% lapply(htmltools::HTML)
      labels <- paste("<strong>",temp_filtered_data$NAMELSAD, 
                      "</strong><br>Numbers of Participating Site:",temp_filtered_data$totalWWTP) %>%
                  lapply(htmltools::HTML)
      central_view <- c(min(merged_CountyWWTP$totalWWTP), max(merged_CountyWWTP$totalWWTP))
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        # Add Texas state boundary
        addPolygons(data = texas_boundary,
                    color = "#000000",
                    weight = 3,
                    fill = FALSE) %>%
        addPolygons(data = merged_CountyWWTP %>%
                      filter(is.na(totalWWTP)), fillColor = ~Color,
                    fillOpacity = ~FillOpacity,
                    color = "black",
                    weight = ~Weight
                      # 
                      # ~ifelse(!is.na(NAMELSAD) & !is.na(totalWWTP),
                      #               lapply(paste("<strong>",NAMELSAD, "</strong><br>Numbers of Site:", totalWWTP), htmltools::HTML),
                      #               NA)
                    # labelOptions = labelOptions(
                    #   noHide = FALSE,
                    #   direction = "auto",
                    #   style = list(
                    #     "color" = "white",
                    #     "font-family" = "Arial, Helvetica, sans-serif",
                    #     "font-style" = "normal",
                    #     "font-weight" = "bold",
                    #     "background" = "rgba(0, 0, 0, 0.5)",
                    #     "border-color" = "white",
                    #     "border-width" = "1px",
                    #     "padding" = "3px 6px",
                    #     "border-radius" = "4px"
                    #   ))
        ) %>%
        addPolygons(data = merged_CountyWWTP %>%
                      filter(!is.na(totalWWTP)), 
                    fillColor = ~Color,
                    fillOpacity = ~FillOpacity,
                    color = "black",
                    weight = ~Weight,
                    label = ~labels,
                    labelOptions = labelOptions(noHide = FALSE,
                                                direction = "center",
                                                textsize = "12px",
                                                style = list(
                                                  "color" = "black",
                                                  "font-family" = "serif",
                                                  "font-style" = "bold",
                                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)"
                                                )))%>%
        setView(lng = -99.56666, lat = 31, zoom = 6)# %>%

    } else {

      central_view <- c(min(county_data_shp$wwtp), max(county_data_shp$wwtp))
      county_selected <- WWTP[which(WWTP$County == input$countySelect),]

      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = unique(WWTP$county_centroid_lon[which(WWTP$County == input$countySelect)]),
                lat = unique(WWTP$county_centroid_lat[which(WWTP$County == input$countySelect)]),
                zoom = 9)  %>%
        addMarkers(
          data = county_selected,
          lng = county_selected$lon,
          lat = county_selected$lat,
          label = ~paste0(WWTP[-1],", ", City),
          labelOptions = labelOptions(noHide = FALSE,
                                      direction = "center",
                                      textsize = "12px",
                                      style = list(
                                        "color" = "black",
                                        "font-family" = "serif",
                                        "font-style" = "bold",
                                        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                        "font-size" = "12px",
                                        "border-color" = "rgba(0,0,0,0.5)"
                                        )))

    }
  })


  ##### END OF LEAFLET MAP SET UP #####
  
  
  ##### BEGGINING OF CDS TAB SET UP ##### 
  
  # Define reactive values for storing the last selected value
  
  cds_lastSelected <- reactiveValues(city_variant = unique(WWTP$City[-1])[1], 
                                     variant_variant = unique(major_path_expand_dt$species)[1],
                                     city_city = unique(WWTP$City[-1])[1],
                                     variant_city = unique(major_path_expand_dt$species)[1]
  )

  output$cdsSelectionUI <- renderUI({
    
    if (input$cdsViewType == 'variant') {
      # When viewing by variant, save the last selected location
      isolate({
        cds_lastSelected$variant_variant <- input$cdsVariantInput_variant
        cds_lastSelected$city_variant <- input$cdsCityInput_variant
      })
      # If a variant was previously selected, use that
      cds_selectedCity_variant <- if (!is.null(cds_lastSelected$city_variant)) cds_lastSelected$city_variant else unique(WWTP$City[-1])[1]
      cds_selectedVariant_variant <- if (!is.null(cds_lastSelected$variant_variant)) cds_lastSelected$variant_variant else unique(major_path_expand_dt$species)[1]
      
      div(
        selectInput("cdsVariantInput_variant", "Select Variant:",
                    choices = unique(major_path_expand_dt$species),
                    selected = cds_selectedVariant_variant),
        
        selectizeInput("cdsCityInput_variant", "Select City(s):",
                       choices = unique(WWTP$City[-1]),
                       multiple = TRUE,
                       selected = cds_selectedCity_variant)
      )
      
      
      
    } else if  (input$cdsViewType == 'city') {
      
      # When viewing by variant, save the last selected location and variant
      isolate({
        cds_lastSelected$city_city <- input$cdsCityInput_city
        cds_lastSelected$variant_city <- input$cdsVariantInput_city
      })
      # If a city was previously selected, use that
      cds_selectedCity_city <- if (!is.null(cds_lastSelected$city_city)) cds_lastSelected$city_city else unique(WWTP$City[-1])[1]
      cds_selectedVariant_city <- if (!is.null(cds_lastSelected$variant_city)) cds_lastSelected$variant_city else unique(major_path_expand_dt$species)[1]
      
      div(
        selectInput("cdsCityInput_city", "Select City:",
                    choices = unique(WWTP$City[-1]),
                    selected = cds_selectedCity_city),
        
        selectizeInput("cdsVariantInput_city", "Select Variant(s):",
                       choices = unique(major_path_expand_dt$species),
                       multiple = TRUE,
                       selected = cds_selectedVariant_city)
        
      )
    }
  })
  
  #### BEGGINNG OF SETTING UP CDS_TRENDPLOT ###
  cds_filtered_data <- reactive({
    req(input$cdsDateRange)
    
    dateFiltered_major_path_expand_dt <- major_path_expand_dt %>%
      filter(Week >= input$cdsDateRange[1] & Week <= input$cdsDateRange[2])
    
    if (input$cdsViewType == 'city') {
      req(input$cdsCityInput_city, input$cdsVariantInput_city)  # Ensure these inputs are defined
      
      # Filter data based on location and variant inputs
      df<- dateFiltered_major_path_expand_dt %>%
        filter(City %in% input$cdsCityInput_city & species %in%  input$cdsVariantInput_city) %>%
        group_by(City, species) %>%
        filter(n_distinct(Week) >= 3) %>%
        arrange(City,species, Week, moving_average) %>%
        drop_na() %>%
        ungroup()
      
      
      
    } else if (input$cdsViewType == 'variant'){
      
      req(input$cdsCityInput_variant, input$cdsVariantInput_variant)  # Ensure these inputs are defined
      
      # Filter data based on location and variant inputs
      df<- dateFiltered_major_path_expand_dt %>%
        filter(City %in% input$cdsCityInput_variant & species %in% input$cdsVariantInput_variant) %>%
        group_by(City, species) %>%
        filter(n_distinct(Week) >= 3) %>%
        arrange(City,species, Week, moving_average) %>%
        drop_na() %>%
        ungroup()
      
    }
    
    df
  })
  
  
  output$cds_TrendPlot <- renderPlotly({
    req(nrow(cds_filtered_data()) > 2)  # Ensure filtered data is not empty
    
    
    if (input$cdsViewType == 'city') {
      # pal <- wes_palette("Darjeeling1",
      # nrow(unique(cds_filtered_data()$species)), 
      # type = "continuous")
      plot <- cds_filtered_data() %>%
        ggplot(aes(x = Week, y = moving_average, color = species)) +
        geom_line(linewidth = 1, alpha = 0.9) +
        scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
        # scale_color_manual(values = pal) +
        scale_y_continuous(position = "right") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title.y = element_text(margin = margin(l = 5))) +
        labs(x = "Date", y = "")
      
      
      if(input$cdsPlotToggle) {
        plot <- plot + facet_wrap(vars(species), scales = "free_y") 
        
      } else {
        plot <- plot 
      }
      
      ggplotly(plot, width = 1500, height = 700) %>%
        layout(yaxis = list(title = list(
          text ='City-wide abundance (RPKMF)                                          \n', 
          xanchor = 'right', yanchor =  'center')))
      
    } else if (input$cdsViewType == 'variant') {
      # pal <- wes_palette("Darjeeling1",
      #                    nrow(unique(cds_filtered_data()$City)), 
      #                    type = "continuous")
      plot <- cds_filtered_data() %>%
        ggplot(aes(x = Week, y = moving_average, color = City)) +
        geom_line(linewidth = 1, alpha = 0.9) +
        scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
        # scale_color_manual(values = pal) +
        scale_y_continuous(position = "right") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title.y = element_text(margin = margin(l = 5)))
      
      if(input$cdsPlotToggle) {
        plot <- plot + facet_wrap(vars(City), scales = "free_y") 
        
      } else {
        plot <- plot 
      }
      
      ggplotly(plot, width = 1500, height = 700) %>%
        layout(
          yaxis = list(
            title = list(
              text = 'City-wide abundance (RPKMF)                                          \n', 
              xanchor = 'right', yanchor =  'center'
            )
          )
        )
      
      
    }
    
    
  })
 

  
  #### END OF SETTING UP CDS_TRENDPLOT ###
  
  ##### END OF CDS TAB SET UP ##### 
  
  


  #### BEGINNING OF QPCR SELECTION UI ####

  qpcr_lastSelected <- reactiveValues(city_variant =gsub(",.*$", "", unique(qPCR_ma_p$City))[1], 
                                      variant_variant = unique(qPCR_ma_p$Target)[1],
                                      city_city = gsub(",.*$", "", unique(qPCR_ma_p$City))[1],
                                      variant_city = unique(qPCR_ma_p$Target)[1]
  )
  

output$qpcrSelectionUI <- renderUI({

  if (input$qpcrViewType == 'variant') {
    # When viewing by variant, save the last selected location
    isolate({
      qpcr_lastSelected$variant_variant <- input$qpcrVariantInput_variant
      qpcr_lastSelected$city_variant <- input$qpcrCityInput_variant
    })
    # If a variant was previously selected, use that
    qpcr_selectedCity_variant <- if (!is.null(qpcr_lastSelected$city_variant)) qpcr_lastSelected$city_variant else gsub(",.*$", "", unique(qPCR_ma_p$City))[1]
    qpcr_selectedVariant_variant <- if (!is.null(qpcr_lastSelected$variant_variant)) qpcr_lastSelected$variant_variant else unique(qPCR_ma_p$Target)[1]

    div(
      selectInput("qpcrVariantInput_variant", "Select Variant:",
                  choices = unique(qPCR_ma_p$Target),
                  selected = qpcr_selectedVariant_variant),

      selectizeInput("qpcrCityInput_variant", "Select City(s):",
                     choices = gsub(",.*$", "", unique(qPCR_ma_p$City)),
                     multiple = TRUE,
                     selected = qpcr_selectedCity_variant)
    )



  } else if  (input$qpcrViewType == 'city') {

    # When viewing by variant, save the last selected location and variant
    isolate({
      qpcr_lastSelected$city_city <- input$qpcrCityInput_city
      qpcr_lastSelected$variant_city <- input$qpcrVariantInput_city
    })
    # If a city was previously selected, use that
    qpcr_selectedCity_city <- if (!is.null(qpcr_lastSelected$city_city)) qpcr_lastSelected$city_city else gsub(",.*$", "", unique(qPCR_ma_p$City))[1]
    qpcr_selectedVariant_city <- if (!is.null(qpcr_lastSelected$variant_city)) qpcr_lastSelected$variant_city else unique(qPCR_ma_p$Target)[1]

    div(
      selectInput("qpcrCityInput_city", "Select City:",
                  choices = gsub(",.*$", "", unique(qPCR_ma_p$City)),
                  selected = qpcr_selectedCity_city),

      selectizeInput("qpcrVariantInput_city", "Select Variant(s):",
                     choices = unique(qPCR_ma_p$Target),
                     multiple = TRUE,
                     selected = qpcr_selectedVariant_city)

    )
  }
})

#### END OF QPCR SELECTION UI ####


#### BEGGINNG OF SETTING UP QPCR_TRENDPLOT ###
qpcr_filtered_data <- reactive({
  req(input$qpcrDateRange)

  dateFiltered_qPCR_ma_p <- qPCR_ma_p %>%
    filter(Week >= input$qpcrDateRange[1] & Week <= input$qpcrDateRange[2])

  if (input$qpcrViewType == 'city') {
    req(input$qpcrCityInput_city, input$qpcrVariantInput_city)  # Ensure these inputs are defined

    # Filter data based on location and variant inputs
    df<- dateFiltered_qPCR_ma_p %>%
      filter(gsub(",.*$", "", City) %in% input$qpcrCityInput_city & Target %in%  input$qpcrVariantInput_city) %>%
      filter(n_distinct(Week) >= 3) %>%
      drop_na() %>%
      ungroup()



  } else if (input$qpcrViewType == 'variant'){

    req(input$qpcrCityInput_variant, input$qpcrVariantInput_variant)  # Ensure these inputs are defined

    # Filter data based on location and variant inputs
    df<- dateFiltered_qPCR_ma_p %>%
      filter(gsub(",.*$", "", City) %in% input$qpcrCityInput_variant & Target %in%  input$qpcrVariantInput_variant) %>%
      filter(n_distinct(Week) >= 3) %>%
      drop_na() %>%
      ungroup()
  }

  df
})



output$qpcr_TrendPlot <- renderPlotly({
  req(nrow(qpcr_filtered_data()) > 2)  # Ensure filtered data is not empty


  if (input$qpcrViewType == 'city') {

    qpcr_plot <- qpcr_filtered_data() %>%
      ggplot(aes(x = Week, y = moving_average, color = City)) +
      geom_line(size = 1, alpha = 0.9) +
      geom_point(size = 0.75, alpha = 0.9) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::scientific) +
     #scale_color_manual(values =  wes_palette("Darjeeling1", length(unique(qpcr_filtered_data()$City)), type = "continuous")) +
      labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



    if(input$qpcrPlotToggle) {
      qpcr_plot <- qpcr_plot + facet_wrap(vars(Target),
                                scales = "free_y")
        

    } else {
      qpcr_plot <- qpcr_plot
    }

    ggplotly(qpcr_plot, width = 1500, height = 700) %>%
      layout(yaxis = list(title = list(
        text ='City-wide abundance (genome copies/sample)                                          \n',
        xanchor = 'right', yanchor =  'center')))

  } else if (input$qpcrViewType == 'variant') {

    qpcr_plot <- qpcr_filtered_data() %>%
      ggplot(aes(x = Week, y = moving_average, color = City)) +
      geom_line(size = 1, alpha = 0.9) +
      geom_point(size = 0.75, alpha = 0.9) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::scientific) +
      #scale_color_manual(values =  wes_palette("Darjeeling1", length(unique(qpcr_filtered_data()$City)), type = "continuous")) +
      labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



    if(input$qpcrPlotToggle) {
      qpcr_plot <- qpcr_plot + facet_wrap(vars(City),
                                scales = "free_y")

    } else {
      qpcr_plot <- qpcr_plot
    }

    ggplotly(qpcr_plot, width = 1500, height = 700) %>%
      layout(yaxis = list(title = list(
        text ='City-wide abundance (genome copies/sample)                                          \n',
        xanchor = 'right', yanchor =  'center')))

  }


})


 
  #### BEGINNING OF COLLECTION DATE PLOT SET UP ####
  strains_per_sample_dt <- comb_tax_table %>%
      group_by(sample_ID) %>%
      summarize(detectedStrains = n_distinct(strain))
    
  number_of_sites <- length(unique(comb_metadata_table$Site))
  pal <- wes_palette("FantasticFox1", number_of_sites, type = "continuous")
  
  
  output$collectionDatesPlot_cds <- renderPlotly({

    if (input$cdsViewType == 'city') {
      
      calendarp <- merge(comb_metadata_table, strains_per_sample_dt, 
                         by = "sample_ID") %>%
        mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
        filter(gsub(",.*$", "", City) %in% input$cdsCityInput_city ) %>%
        select(c(City, Site, Week, sample_ID, detectedStrains)) %>%
        distinct() 

      plot <- ggplot(calendarp,
             aes(x = Week, y = Site, color = Site, size = detectedStrains)) +
      geom_point() +
      geom_line(size = 0.25, na.rm = T) +
      scale_size_continuous(range = c(0.2,3.5)) +
      facet_wrap(City~., ncol = 1, scales = "free_y") +
      scale_color_manual(values = pal) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_discrete(labels = function(x) str_trunc(x, width = 14)) +
      theme_pubclean() +
      labs(x="",y="") +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
            legend.position = "Off")
    
    ggplotly(plot)
    
    }
    
    else if (input$cdsViewType == 'variant') {
      calendarp <- merge(comb_metadata_table, strains_per_sample_dt, 
                         by = "sample_ID") %>%
        mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
        filter(gsub(",.*$", "", City) %in% input$cdsCityInput_variant ) %>%
        select(c(City, Site, Week, sample_ID, detectedStrains)) %>%
        distinct() 
      
      
      plot <- ggplot(calendarp,
             aes(x = Week, y = Site, color = Site, size = detectedStrains)) +
        geom_point() +
        geom_line(size = 0.25, na.rm = T) +
        scale_size_continuous(range = c(0.2,3.5)) +
        facet_wrap(City~., ncol = 1, scales = "free_y") +
        scale_color_manual(values = pal) +
        scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
        scale_y_discrete(labels = function(x) str_trunc(x, width = 14)) +
        theme_pubclean() +
        labs(x="",y="") +
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
              legend.position = "Off")
      
      ggplotly(plot) 
    }
  })
# 
  # output$cdsImportantPathogensTable <- renderReactable({
  #   reactable(
  #     combined_react_data,
  #     pagination = TRUE,
  #     filterable = TRUE,
  #     showPageSizeOptions = TRUE,
  #     pageSizeOptions = c(10, 20, 100),
  #     defaultPageSize = 10,
  #     columns = list(
  #       Percent_covered = colDef(name = "Percent"),
  #       RPKM = colDef(name = "RPKM"),
  #       coverage = colDef(name = "Coverage"),
  #       sequence_name = colDef(name = "Sequence"),
  #       sample_ID = colDef(name = "Sample"),
  #       reference_length = colDef(name = "Reference Length")
  #     )
  #   )
  # })  
  output$cdsImportantPathogensTable <- renderReactable({
    reactable(combined_react_data,
      pagination = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 100),
      defaultPageSize = 10,
      defaultSorted = list(Percent_covered = "desc"),
      columns = list(
        Percent_covered = colDef(
          cell = data_bars(
            data = combined_react_data,
            fill_color = viridis::magma(5, direction = -1),
            background = '#F1F1F1',
            min_value = 0,
            max_value = 1,
            round_edges = TRUE,
            text_position = 'outside-end',
            number_fmt = scales::percent
          )
        ),
        RPKM = colDef(
          width = 70,
          format = colFormat(digits = 2)
        ),
        coverage = colDef(
          filterable = FALSE,
          # width = 250
          # ,
          # cell = react_sparkline(
          #   data = combined_react_data$coverage,
          #   decimals = 1,
          #   show_area = TRUE,
          #   area_color = "darkgreen",
          #   line_curve = "cardinal",
          #   highlight_points = highlight_points(max = "blue"),
          #   labels = "max",
          #   statline = "min",
          #   statline_label_size = "0em",
          #   statline_color = "black"
          ),
        sequence_name = colDef(
          width = 150),
        sample_ID = colDef(
          width = 70,
          name = "Sample"),
        reference_length = colDef(
          width = 80,
          name = "Reference Length")
      ))
  })

  
  #####################
  
  # #-# Calculate tSNE
  # 
  # comb_tax_table$RPKM <- as.numeric(comb_tax_table$RPKM)
  # 
  # seqname_sID_wide_dt <- 
  #   comb_tax_table %>% 
  #   subset(select = c("sequence_name", "sample_ID", "RPKM")) %>%
  #   distinct(sequence_name, sample_ID, RPKM) %>%
  #   group_by(sequence_name, sample_ID) %>%
  #   summarize(RPKM = mean(RPKM)) %>%
  #   pivot_wider(names_from = sequence_name, values_from = RPKM, values_fill = 0)
  # 
  # sampleID_l <- seqname_sID_wide_dt$sample_ID
  # 
  # seqname_sID_wide_dt <- seqname_sID_wide_dt %>% subset(select = -sample_ID)
  # 
  # ##
  # tephi_prcomp1 <- prcomp(log10(seqname_sID_wide_dt+1))
  # 
  # emb <- Rtsne::Rtsne(tephi_prcomp1$x[,1:10])
  # 
  # embb <- emb$Y
  # 
  # rownames(embb) <- sampleID_l
  # 
  # embb_df <- as.data.frame(embb)
  # 
  # embb_dt <- setDT(embb_df, keep.rownames = "sample_ID")
  # 
  # embb_dt <- merge(embb_dt, comb_metadata_table, by ="sample_ID")
  # 
  # #embb_dt$City_Site <- str_c(embb_dt$City, ", ", embb_dt$Site)
  # 
  # embb_dt$Date <- as.Date(embb_dt$Date,  "%m-%d-%Y")
  # 
  # pal <- wes_palette("Darjeeling1", WWTP_citieslength, type = "continuous")
  # 
  # city_tsnep <- embb_dt %>%
  #   ggplot(aes(x=V1, y=V2, color=City, text = Date)) +
  #   geom_point(size = 3, alpha = 0.8) +
  #   scale_color_manual(values = pal) +
  #   theme_bw() +
  #   labs(x="t-SNE 1", y="t-SNE 2")
  # 
  # ggplotly(city_tsnep, tooltip = c("City", "Date"))
  # 
  # 
  # 
  # 
  # as.Date_origin <- function(x){
  #   as.Date(x, origin = '1970-01-01')
  # }
  # 
  # embb_dt$City_Date <- str_c(embb_dt$City, ", ", embb_dt$Date)
  # 
  # date_tsnep <- embb_dt %>%
  #   ggplot(aes(x=V1, y=V2, color=as.integer(Date), text = City_Date)) +
  #   geom_point(size = 3, alpha = 0.8) +
  #   scale_color_gradient(low = "#FDD262",
  #                        high = "#3F3F7B", labels=as.Date_origin, name = "Date") +
  #   theme_bw() +
  #   labs(x="t-SNE 1", y="t-SNE 2")
  # 
  # ggplotly(date_tsnep, tooltip = "City_Date")
  # 
  # 
  # 
  # 
  # as.Date_origin <- function(x){
  #   as.Date(x, origin = '1970-01-01')
  # }
  # 
  # anim_date_tsnep <- embb_dt %>%
  #   arrange(Date) %>%
  #   ggplot(aes(x=V1, y=V2, color=as.integer(Date), group = Site)) +
  #   geom_point(size = 3, alpha = 0.8) +
  #   geom_path(linewidth = 1.1, alpha = 0.8) +
  #   scale_color_gradient(low = "#FDD262",
  #                        high = "#3F3F7B", labels=as.Date_origin, name = "Date") +
  #   theme_bw() +
  #   facet_wrap(~City) +
  #   labs(x="t-SNE 1", y="t-SNE 2") +
  #   transition_reveal(along = as.integer(Date)) 
  # 
  # 
  # animate(anim_date_tsnep, nframes = 100, duration = 15, end_pause = 10,
  #         renderer = gifski_renderer())
  # 
  # 

}
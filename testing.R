source("DataPreprocessing.R")
options(shiny.autoreload = TRUE)

### POTENTIAL IMPROVEMENT: DEBOUNCE()
# helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
# sapply(helper_files, source, encoding = "UTF-8")
# 
# firstArg = NULL
# 
# args = commandArgs(trailingOnly=TRUE)
# 
# # test if there is at least one argument: if not, return an error
# if (length(args)==0) {
#   print("No arguments were supplied ... moving on")
# } else if (length(args)==1) {
#   #
#   firstArg = args[1]
# }
# Add a theme to your app
#theme <- shinytheme("cerulean")

# Include custom CSS in your app
css <- "
  /* Apply a light gray background and padding to all select input fields */
  .selectize-input {
    background-color: #f8f9fa !important;
    padding: 5px !important;
    border: none !important;
    border-radius: 4px;
  }
  
  /* Style the radio buttons */
  .shiny-options-group {
    display: inline-flex !important;
    align-items: center;
    justify-content: center;
  }
  
  
  /* Style the text between radio buttons */
  .shiny-options-group label {
    margin-right: 22px;
  }
  
  /* Change the color of checked radio buttons */
input:radio:checked + label {
  color: green !important;
}
  
  /* Style the plot title */
  .js-plotly-plot .plotly .gtitle {
    font-size: 1.2em;
    color: #1a1a1a;
    font-weight: bold;
  }
  
  /* Apply consistent font styling to all tabs */
  .tabbable > .nav > li > a {
    font-size: 1.1em;
    color: #1a1a1a;
    font-weight: normal;
  }
  /* Apply a light background and padding to the selection inputs */
.selection-inputs {
  background-color: #f8f9fa !important;
  padding: 5px !important;
}

  /* Style the date picker input */
  .datepicker-dropdown {
    width: auto !important;
  }
  .datepicker .datepicker-days {
    width: auto !important;
  }
  
  
  #trendPlot{
  height:700px !important;
  width:1100px !important;
  }
  
    
  #cdsPlot{
  height:700px !important;
  width:1500px !important;
  }
             
"

minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))

ui <- fluidPage(theme = shinytheme("cerulean"),
                
  tags$head(tags$style(HTML(css))),
  
  # Define the UI components for the first tab
  tabsetPanel(
    
    tabPanel("Collection Site", 
             tags$h2(tags$b("Wastewater Treatment Site")),
             tags$h4("This should be a blob of text to describe this page"),
             tags$hr(),
             # in order to put an image in, use the following format: 
             fluidRow(tags$img(height = 2, width = 2200, src = 'www/black_line.png')),
             # tags$h4(tags$b(":")),    
             fluidRow(
               column(width = 4,
                      selectInput("countySelect", "Select County:", 
                                  choices = WWTP$County,
                                  selected = WWTP$County[1])
               )
             ),
             
             tags$style(type = "text/css", "#map {height: calc(75vh - 90px) !important;}"),
             leafletOutput("map", width = "100%"),
             tags$p("Zoom in or out and click on the markers to get more information about each site.")
    ),
    
    
    
    # Add the new CDS tab
    tabPanel("CDS",
             tags$h2(tags$b("CDS Title Here")),
             tags$h4("Blob of Text to Explain this Tab"),
             tags$hr(),
             fluidRow(
               column(
                 width = 3,
                 align = "left",
                 box(
                   title = "CDS Analysis",
                   status = "primary",
                   solidHeader = TRUE,
                   radioButtons("cdsViewType", "",
                                choices = list("View By City" = 'city',
                                               "View By Variant" = 'variant'),
                                selected = 'city',
                                inline = TRUE),
                   
                   uiOutput("cdsSelectionUI"),
                   # Add checkbox input for options to see either single or multiple plots
                   checkboxInput("cdsPlotToggle", "Single/Multiple Plot", FALSE),
                   # Add date range input for trend plot
                   airDatepickerInput("cdsDateRange", "Select Date Range:",
                                      value = c(minDate_cds,maxDate_cds), 
                                      range = TRUE,
                                      update_on = "close",
                                      placeholder = "You can pick 5 dates",
                                      todayButton = TRUE, #If TRUE, then button "Today" will be visible to set view to current date, if a Date is used, it will set view to the given date and select it..
                                      clearButton = TRUE,  #If TRUE, then button "Clear" will be visible.
                                      autoClose = TRUE, #If TRUE, then after date selection, datepicker will be closed.
                                      toggleSelected = TRUE), #When TRUE, in range mode, it's not possible to select the same date as start and end.
                 
                   actionButton("cdsFocusRecent", "Focus on Most Recent Date"),
                   
                   verbatimTextOutput("res")
                   
                 )
               ),
               column(
                 width = 9,
                 align = "left",
                 box(
                   title = "CDS Analysis Plot",
                   status = "primary",
                   solidHeader = TRUE,
                   plotlyOutput("cdsPlot"),
                   textOutput("DateRange"),
                   # Add caption
                   tags$p("This plot shows the moving average of genome copies per sample for the selected variant(s) over time. The line color corresponds to the variant.")
                   
                 )
               )
             )
    ),
    
    
    
    # Define the UI components for the third tab
    tabPanel("Time Trend",
             tags$h2(tags$b("qPCR(Targeted)")),
             tags$h4("Blob of Text to Explain this Tab"),
             tags$hr(),
             fluidRow(
               column(
                 width = 3,
                 align = "left",
                 box(
                   title = "Variant Analysis",
                   status = "primary",
                   solidHeader = TRUE,
                   radioButtons("viewType", "",
                               choices = list("View By Location" = 'location',
                                              "View By Variant" = 'variant'),
                               selected = 'location',
                               inline = TRUE),

                   uiOutput("selectionUI"),
                   checkboxInput("caseToggle", "Clinical Case Data", FALSE),
                   checkboxInput("cityToggle", "View by city", FALSE),
                   # Add button to focus on the most recent date
                   # Add date range input for trend plot
                   airDatepickerInput("dateRange", "Select Date Range:",
                                      value = c(minDate_cds,maxDate_cds),
                                      range = TRUE),

                   actionButton("focusRecent", "Focus on Most Recent Date")

                   )
                 ),
               column(
                 width = 9,
                 align = "left",
                 box(
                   title = "Variant Analysis Plot",
                   status = "primary",
                   solidHeader = TRUE,
                   plotlyOutput("trendPlot"),
                   # Add caption
                   tags$p("This plot shows the moving average
                          of genome copies per sample for the
                          selected variant(s) over time.
                          The line color corresponds to the variant.")

                 )
              )
           )
        )
    )
)


############# END OF UI LOGIC OF SHINY #############

############################### BREAK #######################################



################## BEGINNING OF SERVER LOGIC OF SHINY #################


server <- function(input, output, session) {
  
  output$DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$cdsDateRange[2] > input$cdsDateRange[1], "end date is earlier than start date"
      )
    )
    
    # # make sure greater than 2 week difference
    # validate(
    #   need(difftime(input$cdsDateRange[2], input$cdsDateRange[1], "days") > 14, "date range less than 14 days"
    #   )
    # )
    
    
    # make sure end date later than start date
    validate(
      need(input$cdsDateRange[2] <= maxDate_cds, "end date is not available"),
      need(input$cdsDateRange[1] >= minDate_cds, "start date is not available" )
  )
    
    paste("Your date range is", 
          difftime(input$cdsDateRange[2], input$cdsDateRange[1], units="days"),
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
                      "</strong><br>Numbers of Participating Site:",temp_filtered_data$totalWWTP,
                      "</br><br>testing ", WWTP$C) %>%
                  lapply(htmltools::HTML)
      central_view <- c(min(merged_CountyWWTP$totalWWTP), max(merged_CountyWWTP$totalWWTP))
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        # # Add Texas state boundary
        # addPolygons(data = texas_sf,
        #             color = "#000000",
        #             weight = 3,
        #             fill = FALSE
        #             #label = ~paste0(NAMELSAD, "<br>","Numbers of Site: ", wwtp)
        # ) %>%
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
                    label = ~labels)%>%
        setView(lng = -99.56666, lat = 31, zoom = 6)# %>%
        # addCircleMarkers(
        #   data = CountyWWTP,
        #   lng = CountyWWTP$county_centroid_lon,
        #   lat = CountyWWTP$county_centroid_lat,
        #   radius = ~Radius,
        #   color = ~Color,
        #   fillOpacity = 0.9,
        #   fill = TRUE,
        #   opacity = 0.9,
        #   label = ~totalWWTP,
        #   labelOptions = labelOptions(noHide = TRUE,
        #                               textOnly = TRUE,
        #                               direction = "center",
        #                               style = list("font-weight" = "bold"),
        #                               textsize = "12px"
        #   )
        # )

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
          #radius = 6,
          #stroke = FALSE,
          #fillColor = "black",
          #fillOpacity = 1,
          #weight = 5,
          label = ~as.character(WWTP),
          labelOptions = labelOptions(noHide = FALSE,
                                      direction = "center",
                                      textsize = "12px",
                                      style = list(
                                        "color" = "black",
                                        "font-family" = "serif",
                                        "font-style" = "italic",
                                        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                        "font-size" = "12px",
                                        "border-color" = "rgba(0,0,0,0.5)"
                                        )))

    }
  })


  ##### END OF LEAFLET MAP SET UP #####
  
  
  ##### BEGGINING OF CDS TAB SET UP ##### 
  # # Define reactive values for storing the last selected value
  # lastSelected <- reactiveValues(location = NULL, variant = NULL)
  #  output$selectionUI <- renderUI({
  #   
  # 
  #   if (input$viewType == 'location') {
  #     
  #     # observeEvent(input$cityToggle, {
  #     #   updateSelectInput(session, "locationInput",
  #     #                     choices = if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1],
  #     #                     selected = if (input$cityToggle) WWTP$County[1] else WWTP$WWTP[1])
  #     # })
  # 
  #     
  #     # If a location was previously selected, use that
  #     selectedLocation <- if (!is.null(lastSelected$location)) lastSelected$location else WWTP$WWTP[1]
  #     locationChoices <- if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1]
  #     div(
  #       tags$hr("Compare multiple wastewater site locations for a single variant."),
  #       selectInput("locationInput", "Select Location:",
  #                   choices = locationChoices,
  #                   selected = selectedLocation),
  # 
  #       selectizeInput("variantInput", "Select Variant(s):",
  #                      choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
  #                      selected = input$variantInput,
  #                      multiple = TRUE)
  #     )
  #     # When viewing by location, allow selecting one location and multiple variants
  #   } else if (input$viewType == 'variant') {
  #     
  #     # observeEvent(input$cityToggle, {
  #     #   updateSelectInput(session, "locationInput",
  #     #                     choices = if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1],
  #     #                     selected = if (input$cityToggle) WWTP$County[1] else WWTP$WWTP[1])
  #     # })
  #     
  #     # When viewing by variant, save the last selected location
  #     lastSelected$location <- input$locationInput
  #     
  #     # If a variant was previously selected, use that
  #     selectedVariant <- if (!is.null(lastSelected$variant)) lastSelected$variant else "SARSCOV2N1"
  #     locationChoices <- if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1]
  #     div(
  #       selectInput("variantInput", "Select Variant:",
  #                   choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
  #                   selected = selectedVariant),
  # 
  #       selectizeInput("locationInput", "Select Location(s):",
  #                      choices = locationChoices,
  #                      multiple = TRUE,
  #                      selected = input$locationInput)
  #     )
  # 
  #   }
  # })
   
   
  # Define reactive values for storing the last selected value

  lastSelected_variant <- reactiveValues(city_variant = unique(WWTP$City[-1])[1], 
                                 variant_variant = unique(major_path_expand_dt$species)[1]
                                 )
  lastSelected_city <- reactiveValues(city_city = unique(WWTP$City[-1])[1],
                                      variant_city = unique(major_path_expand_dt$species)[1]
                                  )
  
  output$cdsSelectionUI <- renderUI({

    if (input$cdsViewType == 'variant') {
      # When viewing by variant, save the last selected location
      isolate({
      lastSelected_variant$variant_variant <- input$cdsVariantInput_variant
      lastSelected_variant$city_variant <- input$cdsCityInput_variant
      })
      # If a variant was previously selected, use that
      selectedCity_variant <- if (!is.null(lastSelected_variant$city_variant)) lastSelected_variant$city_variant else unique(WWTP$City[-1])[1]
      selectedVariant_variant <- if (!is.null(lastSelected_variant$variant_variant)) lastSelected_variant$variant_variant else unique(major_path_expand_dt$species)[1]
      
      div(
        selectInput("cdsVariantInput_variant", "Select Variant:",
                    choices = unique(major_path_expand_dt$species),
                    selected = selectedVariant_variant),
        
        selectizeInput("cdsCityInput_variant", "Select City(s):",
                       choices = unique(WWTP$City[-1]),
                       multiple = TRUE,
                       selected = selectedCity_variant)
      )
    
      

    } else if  (input$cdsViewType == 'city') {

 # When viewing by variant, save the last selected location and variant
      isolate({
      lastSelected_city$city_city <- input$cdsCityInput_city
      lastSelected_city$variant_city <- input$cdsVariantInput_city
      })
      # If a city was previously selected, use that
      selectedCity_city <- if (!is.null(lastSelected_city$city_city)) lastSelected_city$city_city else unique(WWTP$City[-1])[1]
      selectedVariant_city <- if (!is.null(lastSelected_city$variant_city)) lastSelected_city$variant_city else unique(major_path_expand_dt$species)[1]

      div(
        selectInput("cdsCityInput_city", "Select City:",
                    choices = unique(WWTP$City[-1]),
                    selected = selectedCity_city),

        selectizeInput("cdsVariantInput_city", "Select Variant(s):",
                       choices = unique(major_path_expand_dt$species),
                       multiple = TRUE,
                       selected = selectedVariant_city)

      )
    }
  })


# observeEvent(input$cityToggle, {
#   updateSelectInput(session, "locationInput",
#                     choices = if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1],
#                     selected = if (input$cityToggle) WWTP$County[1] else WWTP$WWTP[1])
# })
  

# # Define a reactive expression that creates a filtered data frame based on the inputs
# filtered_data <- reactive({
#   req(input$locationInput, input$variantInput)  # Ensure these inputs are defined
#   
#   if (input$cityToggle == FALSE & input$caseToggle == FALSE) {
#     # Filter data based on location and variant inputs
#     temp_df <- comb_qPCR_table_average %>%
#       filter(Site %in% input$locationInput & Target %in% input$variantInput) %>%
#       group_by(Site, Target) %>%
#       filter(n_distinct(Week) >= 3) %>%
#       arrange(Site, Target, Week) %>%
#       mutate(moving_average = ma(average_genome_copies_L)) %>%
#       drop_na() %>%
#       ungroup()
#   } else if (input$caseToggle == TRUE | input$cityToggle == TRUE) {
#           if (!(input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
#                    input$variantInput == "SARSCOV2N1"))
# 
#             temp_df = data.frame()
# 
#             if (input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
#                 input$variantInput == "SARSCOV2N1")
#             {
#               temp_df <- organized.data %>%
#                 filter(
#                   WWTPSite == input$locationInput
#                 )
# 
#             }
#   }
#   temp_df
# })

  

# # Add an observer to update the date range when the focus button is clicked
# observeEvent(input$focusRecent, {
#   updatePickerInput(session, "dateRange",
#                            min = max(comb_qPCR_table_average$Week, na.rm = TRUE),
#                     max = max(comb_qPCR_table_average$Week, na.rm = TRUE), 
#                     value = c(max(comb_qPCR_table_average$Week), 
#                               max(comb_qPCR_table_average$Week)))
# })
  
  # CDSdata
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
  

  output$cdsPlot <- renderPlotly({
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
        layout(yaxis = list(title = list(
          text ='City-wide abundance (RPKMF)                                          \n', 
          xanchor = 'right', yanchor =  'center')))
 
      
    }
    
  })
  
  #Add observer to update the date range input when the button is clicked
  observeEvent(input$cdsFocusRecent, {
    # Get the maximum date from the data
    maxDate <- max(cds_filtered_data()$Week, na.rm = TRUE)
    
    # Update the date range input to the most recent date
    updateAirDateInput(session, "cdsDateRange",
                       min = maxDate - 3,  # You can adjust the range according to your needs
                       max = maxDate,
                       value = c(maxDate - 3, maxDate))
  })
 
  ##### END OF CDS TAB SET UP ##### 
  
# Render the plot based on the filtered data
# output$trendPlot <- renderPlotly({
#   req(nrow(filtered_data()) > 1)  # Ensure filtered data is not empty
#   
#   plot <- ggplot(filtered_data(), 
#                  aes(x = Week, 
#                      y = moving_average, 
#                      color = Target, 
#                      group = Target)) +
#     geom_line(size = 0.7, alpha = 0.9) +
#     scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
#     scale_y_continuous(labels = scales::scientific) +
#     labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#   
#   ggplotly(plot)
# })

}

################## END OF SERVER LOGIC OF SHINY #################

##################################


options(shiny.port = 8100)

runApp(shinyApp(ui = ui, server = server ))

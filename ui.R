

minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))

minDate_qpcr<- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "tephi_ww_dashboard.css")
                ),
                
                # Define the UI components for the first tab
                div(
                 tabsetPanel(
                
                  tabPanel("Collection Site", 
                           tags$h2(tags$b("Wastewater Treatment Site")),
                           tags$h4("Explore a comprehensive map of wastewater treatment sites. Use the dropdown menu to filter by county."),
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
                           leafletOutput("map", width = "100%")
                  ),
                  
                  
                  
                  # Add the new CDS tab
                  tabPanel("Comprehensive Deep Sequencing",
                           tags$h2(tags$b("Comprehensive Deep Sequencing: Important Pathogens by City and by Species")),
                           tags$h5("The upper plot shows the city-wide pathogen trends over time. Different lines represent the relative abundance of each pathogen in the sampled wastewater."),
                           tags$h5("The lower plot displays the sample history of each site and its overall virus diversity. Use the controls on the left to adjust the view."),
                           tags$hr(),
                           fluidRow(
                             column(
                               width = 3,
                               align = "left",
                               box(
                                 title = "Control Panel",
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
                                                    #placeholder = "You can pick 5 dates",
                                                    todayButton = TRUE, #If TRUE, then button "Today" will be visible to set view to current date, if a Date is used, it will set view to the given date and select it..
                                                    clearButton = TRUE,  #If TRUE, then button "Clear" will be visible.
                                                    autoClose = TRUE, #If TRUE, then after date selection, datepicker will be closed.
                                                    toggleSelected = TRUE) #When TRUE, in range mode, it's not possible to select the same date as start and end.
                                 # 
                                 # actionButton("cdsFocusRecent", "Focus on Most Recent Date"),
                                 # 
                                 # verbatimTextOutput("res")
                                 
                               )
                             ),
                             column(
                               width = 9,
                               align = "left",
                               box(
                                 title = "",
                                 status = "primary",
                                 solidHeader = TRUE,
                                
                                 fluidRow( plotlyOutput("cds_TrendPlot")),
                                 div(class = 'cds-daterange-text', textOutput("cds_DateRange")),
                                 tags$br(),
                                 fluidRow(plotlyOutput("cds_TrendPlot_zoomed")),
                                 fluidRow(plotlyOutput("collectionDatesPlot_cds"))
                                 
 
                               )
                             )
                           )
                  ),

                  #### ADD QPCR TAB ####

                  tabPanel("qPCR (Targeted)",
                           tags$h2(tags$b("Quantification of Specific Pathogens in Wastewater")),
                           tags$h5("The top figure shows the City-wide pathogen trends. Lines show the change in relative abundance of each pathogen in sampled wastewater"),
                           tags$h5("The bottom figure shows the sample history of each site and its overall virus diversity.\n This approach provides fast, quantitative measurements on a variety of known virus pathogens. Only recently detected pathogens from the targeted list are shown."),
                           tags$hr(),
                           fluidRow(
                             column(
                               width = 3,
                               align = "left",
                               box(
                                 title = "Control Panel",
                                 inline = TRUE,
                                 status = "primary",
                                 solidHeader = TRUE,
                                 radioButtons("qpcrViewType", "",
                                              choices = list("View By City" = 'city',
                                                             "View By Variant" = 'variant'),
                                              selected = 'city',
                                              inline = TRUE),

                                 uiOutput("qpcrSelectionUI"),
                                 # Add checkbox input for options to see either single or multiple plots
                                 checkboxInput("qpcrPlotToggle", "Single/Multiple Plot", FALSE),
                                 # Add date range input for trend plot
                                 airDatepickerInput("qpcrDateRange", "Select Date Range:",
                                                    value = c(minDate_qpcr,maxDate_qpcr),
                                                    range = TRUE,
                                                    update_on = "close",
                                                    #placeholder = "You can pick 5 dates",
                                                    todayButton = TRUE, #If TRUE, then button "Today" will be visible to set view to current date, if a Date is used, it will set view to the given date and select it..
                                                    clearButton = TRUE,  #If TRUE, then button "Clear" will be visible.
                                                    autoClose = TRUE, #If TRUE, then after date selection, datepicker will be closed.
                                                    toggleSelected = TRUE) #When TRUE, in range mode, it's not possible to select the same date as start and end.

                                 # actionButton("cdsFocusRecent", "Focus on Most Recent Date"),
                                 #
                                 # verbatimTextOutput("res")

                               )
                             ),
                             column(
                               width = 9,
                               align = "left",
                               box(
                                 title = "",
                                 status = "primary",
                                 solidHeader = TRUE,

                                 fluidRow(plotlyOutput("qpcr_TrendPlot")),
                                 div(class = 'qpcr_daterange-text', textOutput("qpcr_DateRange")),
                                 tags$br(),
                                 fluidRow(plotlyOutput("collectionDatesPlot_qpcr"))


                               )
                             )
                           )
                  )

                  ### END OF QPCR UI TAB ###
         )           #DIV WRAP
    )
)

############# END OF UI LOGIC OF SHINY #############


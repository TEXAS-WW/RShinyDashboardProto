
#source("DataPreprocessing.R")
source("variables.R")
source("screen_detector_script.R")


ui <- fluidPage(theme = shinytheme("cerulean"),
                includeCSS("www/tephi_ww_dashboard.css"),
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "tephi_ww_dashboard.css")
                ),
                tags$script(HTML(screen_detector_script)),
                
                # Define the UI components for the first tab
                tags$div( id = "wrappingEverything",
                          tabsetPanel(
                            tabPanel("Collection Site", 
                                     tags$h2(tags$b("Wastewater Treatment Site")),
                                     tags$h4("Explore a comprehensive map of wastewater treatment sites. Use the dropdown menu to filter by county."),
                                     tags$hr(),
                                     # in order to put an image in, use the following format: 
                                     #fluidRow(tags$img(height = 2, width = 2200, src = black_line_src)),
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
                                     tabsetPanel( 
                                       tabPanel("Date and Site",
                                                tags$h2(tags$b("Collection Dates and Sites for all Comprehensive Deep Sequencing Samples")),
                                                tags$h5("Here, you can find the sample history of each site and its overall virus diversity"),
                                                tags$hr(),
                                                fluidRow(
                                                  column(
                                                    width = 2,
                                                    align = "left",
                                                    box(
                                                      title = "Control Panel",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      selectizeInput("cdsCity_collectiondate", "Select County(s):",
                                                                     choices = WWTP$City[-1],
                                                                     multiple = TRUE,
                                                                     selected = WWTP$City[2])
                                                      
                                                    )
                                                  ),
                                                  column(
                                                    width = 10,
                                                    align = "left",
                                                    box(
                                                      title = "",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      fluidRow(plotlyOutput("collectionDatesPlot_cds"))
                                                    )
                                                  )
                                                )
                                       ),
                                       tabPanel("Important Pathogens",
                                                tags$h2(tags$b("Comprehensive Deep Sequencing: Important Pathogens by City and by Species")),
                                                tags$h5("The upper plot shows the city-wide pathogen trends over time. Different lines represent the relative abundance of each pathogen in the sampled wastewater."),
                                                tags$h5("The lower plot displays the sample history of each site and its overall virus diversity. Use the controls on the left to adjust the view."),
                                                tags$hr(),
                                                fluidRow(
                                                  column(
                                                    width = 2,
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
                                                    )
                                                  ),
                                                  column(
                                                    width = 10,
                                                    align = "left",
                                                    box(
                                                      title = "",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      
                                                      fluidRow( plotlyOutput("cds_TrendPlot")),
                                                      div(class = 'cds-daterange-text', textOutput("cds_DateRange"))
                                                    )
                                                  )
                                                )
                                       ),
                                       # BEGINNING OF SECOND TAB 
                                       tabPanel("Interactive Table with Info About Pathogens Trends",
                                                tags$h2(tags$b("Comprehensive Deep Sequencing: Interactive Table with Info About Pathogens Trends")),
                                                tags$h5("ully searchable and sortable table. Change since 4 weeks before “Most Recent Week”. Re-emergent virus sequences are marked as +100% for simplicity."),
                                                tags$hr(),
                                                # Add a spot for the table
                                                reactableOutput("cdsInteractiveTable")
                                       ),
                                       # BEGINNING OF SECOND TAB 
                                       tabPanel("Genome Coverage of Important Pathogens",
                                                tags$h2(tags$b("Comprehensive Deep Sequencing: Genome Coverage of Important Pathogens")),
                                                tags$h5("Fully searchable and sortable table. Percent covered represents the fraction of the genome detected in the sequencing data. Accession number is the unique identifier for that genome in NCBI."),
                                                tags$hr(),
                                                # Add a spot for the table
                                                reactableOutput("cdsImportantPathogensTable")
                                       ),
                                       
                                       ### BEGINNING OF THIRD TAB 
                                       tabPanel("Community Similarity Chart (t-SNE)",
                                                tags$h2(tags$b("Comprehensive Deep Sequencing: Community Similarity Chart (t-SNE)")),
                                                tags$h5("Each dot represents the overall “virus community” of a sample and the distance between dots correlates with the dissimilarity of those communities."),
                                                tags$hr(),
                                                column(
                                                  width = 6,
                                                  fluidRow(plotlyOutput("city_tsnep"))) , 
                                                # column(
                                                #   width = 3,
                                                #   fluidRow(plotlyOutput("date_tsnep"))),
                                                column(
                                                  width = 6,
                                                  uiOutput("animation"))
                                       ) ,
                                       tabPanel("Select a Virus",
                                                tags$h2(tags$b("Comprehensive Deep Sequencing: Select a virus")),
                                                tags$h5("Use the drop-down menu to see the temporal trend of any virus (binned at the species level) in each city’s wastewater"),
                                                tags$hr(),
                                                fluidRow(
                                                  column(
                                                    width = 2,
                                                    align = "left",
                                                    box(
                                                      title = "Control Panel",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      
                                                      selectInput("virus", "Select Virus:",
                                                                  choices = unique(prevalent_sp_dt$species),
                                                                  selected = unique(prevalent_sp_dt$species)[1]),
                                                      
                                                      selectizeInput("virusCity", "Select City(s):",
                                                                     choices = unique(prevalent_sp_dt$City),
                                                                     multiple = TRUE,
                                                                     selected = unique(prevalent_sp_dt$City)[1]),
                                                      
                                                      # Add checkbox input for options to see either single or multiple plots
                                                      checkboxInput("virusPlotToggle", "Single/Multiple Plot", FALSE)
                                                    )
                                                  ),
                                                  column(
                                                    width = 10,
                                                    align = "left",
                                                    box(
                                                      title = "",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      fluidRow( plotlyOutput("virusPlot"))
                                                    )
                                                  )
                                                )
                                                
                                       )
                                     )
                            ),
                            ### ADD QPCR TAB ####
                            
                            tabPanel("qPCR (Targeted)",
                                     
                                     tags$head(
                                       tags$style(
                                         HTML("
                                 .tabbable > .nav.nav-tabs {
                                   display: flex;
                                   justify-content: center;
                                 }
                                 ")
                                       )
                                     ),
                                     tabsetPanel( 
                                       
                                       tabPanel("Specific Pathogens Trend for qPCR",
                                                tags$h2(tags$b("Quantification of Specific Pathogens in Wastewater")),
                                                tags$h5("This approach provides fast, quantitative measurements on a variety of known virus pathogens. Only recently detected pathogens from the targeted list are shown."),
                                                tags$hr(),
                                                fluidRow(
                                                  column(
                                                    width = 2,
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
                                                      
                                                    )
                                                  ),
                                                  column(
                                                    width = 10,
                                                    align = "left",
                                                    box(
                                                      title = "",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      
                                                      fluidRow(plotlyOutput("qpcr_TrendPlot")),
                                                      div(class = 'qpcr_daterange-text', textOutput("qpcr_DateRange"))
                                                      
                                                    )
                                                  )
                                                )
                                       )#,
                                       # # BEGINNING OF SECOND TAB 
                                       # tabPanel("El Paso ",
                                       #          tags$h2(tags$b("Wastewater Concentration Trend in El Paso and its relationship with case data")),
                                       #          #tags$h5("This approach provides fast, quantitative measurements on a variety of known virus pathogens. Only recently detected pathogens from the targeted list are shown."),
                                       #          tags$hr(),
                                       #          fluidRow(
                                       #            column(
                                       #              width = 2,
                                       #              align = "left",
                                       #              box(
                                       #                title = "Control Panel",
                                       #                inline = TRUE,
                                       #                status = "primary",
                                       #                solidHeader = TRUE,
                                       #                selectInput("WWTP_input", "Select WWTP site:", 
                                       #                            choices = unique(merged_data$WWTP), selected = merged_data$WWTP[1]),
                                       #                checkboxInput("wrap_plot", "Wrap plot:", FALSE)
                                       #              )
                                       #            ),
                                       #            column(
                                       #              width = 10,
                                       #              align = "left",
                                       #              box(
                                       #                title = "",
                                       #                status = "primary",
                                       #                solidHeader = TRUE,
                                       #                fluidRow(plotlyOutput("qpcr_ElPaso_reactivePlot")),
                                       #                
                                       #              )
                                       #            )
                                       #          )
                                       #          # Add a spot for the table
                                       #          
                                       # )#,

                                       # ### BEGINNING OF THIRD TAB
                                       # tabPanel("Predictive Model"
                                       # 
                                       # )
                                     )
                            )
                            ### END OF QPCR UI TAB ###
                          )           #DIV WRAP
                )
)

############# END OF UI LOGIC OF SHINY #############


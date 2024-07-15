# Load necessary R scripts for data handling and variables
source("variables.R") # Loads global variables used across the dashboard
source("screen_detector_script.R") # Loads a script to adjust UI based on the screen size

# Define the main UI layout of the Shiny app

ui <- fluidPage(
  theme = shinytheme("cerulean"), # Apply the 'cerulean' theme from shinythemes package for a consistent look
  includeCSS("www/tephi_ww_dashboard.css"), # Include custom CSS for additional styling, influencing the overall aesthetics
  
  # HTML tags for including additional resources and styles
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "tephi_ww_dashboard.css"), # External CSS link for custom styles specific to this dashboard
    tags$style(HTML(
      ".content-wrapper, .right-side {padding-right: 0px;}" # Specific CSS styles to adjust padding for content-wrapper and right-side elements, enhancing layout fit
    ))
  ),
  tags$script(HTML(screen_detector_script)), # Custom JavaScript embedded directly for handling screen size adjustments dynamically
  
  # Main container that wraps all tab panels
  tags$div(id = "wrappingEverything",
           tabsetPanel(
             # First main tab for visualizing collection sites on a map
             tabPanel(
               "Collection Site",
               tags$h2(tags$b("Wastewater Treatment Site")), # Main title for the tab, bolded for emphasis
               tags$h4("Explore a comprehensive map of wastewater treatment sites. Use the dropdown menu to filter by county."), # Description for user guidance
               tags$hr(), # Visual separator to enhance layout structure
               fluidRow(
                 column(
                   width = 4, # Defines the width of the dropdown within the grid layout
                   selectInput(
                     "countySelect", "Select County:", # Dropdown input for selecting counties, labeled clearly
                     choices = WWTP$County, # Populates dropdown choices from the 'County' column in WWTP dataset
                     selected = WWTP$County[1] # Automatically selects the first county as default
                   )
                 )
               ),
               tags$style(type = "text/css", "#map {height: calc(75vh - 90px) !important;}"), # CSS rule to dynamically adjust the height of the map based on the viewport height
               leafletOutput("map", width = "100%") # Placeholder for rendering the Leaflet map, taking full width of the container
             ),
             
             # Second main tab for comprehensive deep sequencing with nested tabs for detailed views
             tabPanel(
               "Comprehensive Deep Sequencing",
               tabsetPanel(
                 # Nested tab for viewing data and site collection by date
                 tabPanel(
                   "Date and Site",
                   tags$h2(tags$b("Collection Dates and Sites for all Comprehensive Deep Sequencing Samples")), # Section header, bolded for emphasis
                   tags$h5("Here, you can find the sample history of each site and its overall virus diversity"), # Descriptive text to guide user expectations
                   tags$hr(), # Horizontal rule for visual separation of sections
                   fluidRow(
                     column(
                       width = 2, # Narrow column for control panel inputs
                       align = "left", # Aligns content to the left
                       box(
                         title = "Control Panel", # Titled box for grouping related UI elements
                         status = "primary", # Uses a primary color theme for the box
                         solidHeader = TRUE, # Ensures the header of the box is solid colored
                         selectizeInput(
                           "cdsCity_collectiondate", "Select County(s):", # Advanced select input allowing for multiple selections
                           choices = WWTP$City[-1], # Excludes the first city from choices for specific reasons, perhaps data quality
                           multiple = TRUE, # Enables multiple selections
                           selected = WWTP$City[2] # Defaults to the third city in the list
                         ) 
                       )
                     ),
                     column(
                       width = 10, # Wider column for displaying plots
                       align = "left", # Aligns content to the left
                       box(
                         title = "", # Anonymous box to avoid distraction, focusing on content within
                         status = "primary", # Primary theme for consistent styling
                         solidHeader = TRUE, # Solid header for better visual separation
                         fluidRow(plotlyOutput("collectionDatesPlot_cds")) # Placeholder for Plotly plot output
                       )
                     )
                   )
                 ),

                 # Sub-tab for Important Pathogens analysis
                 tabPanel(
                   "Important Pathogens",
                   tags$h2(tags$b("Comprehensive Deep Sequencing: Important Pathogens by City and by Species")), # Header describing the content of this tab
                   tags$h5("The upper plot shows the city-wide pathogen trends over time. Different lines represent the relative abundance of each pathogen in the sampled wastewater."), # Explanation of what the first plot represents
                   tags$h5("The lower plot displays the sample history of each site and its overall virus diversity. Use the controls on the left to adjust the view."), # Details about the second plot
                   tags$hr(), # Visual separator
                   fluidRow(
                     column(
                       width = 2, # Smaller column for control elements
                       align = "left", # Left alignment for better visual structure
                       box(
                         title = "Control Panel", # Label for the box containing input controls
                         status = "primary", # Designates the importance of this box with a primary color
                         solidHeader = TRUE, # Ensures the header is visually distinct
                         radioButtons(
                           "cdsViewType", # Radio buttons for user to select view type
                           "", # No label needed here for a cleaner look
                           choices = list("View By City" = 'city', "View By Variant" = 'variant'), # Options for user selection
                           selected = 'city', # Default selected view type
                           inline = TRUE # Display options in a row (inline) for compactness
                         ),
                         uiOutput("cdsSelectionUI"), # Dynamic UI output for further user interaction options
                         checkboxInput("cdsPlotToggle", "Single/Multiple Plot", FALSE), # Checkbox to toggle between single and multiple plot views
                         airDatepickerInput(
                           "cdsDateRange", # Component ID
                           "Select Date Range:", # Label
                           value = c(minDate_cds, maxDate_cds), # Default value set to the range of dates available in the data
                           range = TRUE, # Enables range selection
                           update_on = "close", # Updates the input value when the datepicker closes
                           todayButton = TRUE, # Button to select today's date quickly
                           clearButton = TRUE, # Button to clear the selected date
                           autoClose = TRUE, # Datepicker closes automatically upon date selection
                           toggleSelected = TRUE # Prevents reselecting the start date as the end date
                         )
                       )
                     ),
                     column(
                       width = 10, # Larger column for displaying plots
                       align = "left", # Left alignment for the content
                       box(
                         title = "", # No title for simplicity
                         status = "primary", # Indicates the box's importance with a primary color
                         solidHeader = TRUE, # Solid header to differentiate from other content
                         fluidRow(plotlyOutput("cds_TrendPlot")), # Placeholder for trend plot output using Plotly
                         div(class = 'cds-daterange-text', textOutput("cds_DateRange")) # Displays selected date range dynamically
                       )
                     )
                   )
                 ),
                 
                 
                 # Sub-tab 3: Interactive Table with Information About Pathogens Trends
                 tabPanel(
                   "Interactive Table with Info About Pathogens Trends",
                   tags$h2(tags$b("Comprehensive Deep Sequencing: Interactive Table with Info About Pathogens Trends")),  # Title for the sub-tab.
                   tags$h5("Fully searchable and sortable table. Change since 4 weeks before 'Most Recent Week'. Re-emergent virus sequences are marked as +100% for simplicity."),  # Description of the table's functionality and data presentation.
                   tags$hr(),  # Visual separator for aesthetic enhancement.
                   reactableOutput("cdsInteractiveTable")  # Output placeholder for an interactive table generated using Reactable package.
                 ),
                 
                 
                 # Sub-tab 4: Genome Coverage of Important Pathogens
                 tabPanel(
                   "Genome Coverage of Important Pathogens",
                   tags$h2(tags$b("Comprehensive Deep Sequencing: Genome Coverage of Important Pathogens")),  # Heading for this section, emphasizing its focus on genome coverage.
                   tags$h5("Fully searchable and sortable table. Percent covered represents the fraction of the genome detected in the sequencing data. Accession number is the unique identifier for that genome in NCBI."),  # Explanation of what data the table contains and how to interpret it.
                   tags$hr(),  # Horizontal rule for separating content visually.
                   reactableOutput("cdsImportantPathogensTable")  # Placeholder for a table displaying genomic coverage data.
                 ),
                 
                 
                 # Sub-tab 5: Community Similarity Chart (t-SNE)
                 tabPanel(
                   "Community Similarity Chart (t-SNE)",
                   tags$h2(tags$b("Comprehensive Deep Sequencing: Community Similarity Chart (t-SNE)")),  # Header for the sub-tab, highlighting the use of t-SNE for community similarity.
                   tags$h5("Each dot represents the overall 'virus community' of a sample and the distance between dots correlates with the dissimilarity of those communities."),  # Details about the plot's interpretation.
                   tags$hr(),  # Visual separator.
                   fluidRow(plotlyOutput("city_tsnep"))  # Output for a Plotly interactive plot showing t-SNE results based on city data.
                 ),
                 
                 
                 # Sub-tab 6: Select a Virus
                 tabPanel(
                   "Select a Virus",
                   tags$h2(tags$b("Comprehensive Deep Sequencing: Select a virus")),  # Section title for virus selection and visualization.
                   tags$h5("Use the drop-down menu to see the temporal trend of any virus (binned at the species level) in each city’s wastewater"),  # Guide for using the dropdown to filter data in the plot.
                   tags$hr(),  # Horizontal rule.
                   fluidRow(
                     column(
                       width = 2,
                       align = "left",
                       box(
                         title = "Control Panel",  # Box title for input controls.
                         status = "primary",  # Box styling option.
                         solidHeader = TRUE,  # Box styling option for header.
                         selectInput("virus", "Select Virus:", choices = unique(prevalent_sp_dt$species), selected = unique(prevalent_sp_dt$species)[1]),  # Dropdown for selecting virus species.
                         selectizeInput("virusCity", "Select City(s):", choices = unique(prevalent_sp_dt$City), multiple = TRUE, selected = unique(prevalent_sp_dt$City)[1]),  # Multi-select dropdown for selecting cities.
                         checkboxInput("virusPlotToggle", "Single/Multiple Plot", FALSE)  # Checkbox to toggle between single and multiple plot views.
                       )
                     ),
                     column(
                       width = 10,
                       align = "left",
                       box(
                         title = "",  # Anonymously titled box for cleaner UI.
                         status = "primary",  # Box styling option.
                         solidHeader = TRUE,  # Ensures the header is visually distinct.
                         fluidRow(plotlyOutput("virusPlot"))  # Placeholder for displaying the virus trend plot.
                       )
                     )
                   )
                 )
                )
               ),
                 
                 
                 # Define the UI components for the third tab: qPCR
                 tabPanel(
                   "qPCR (Targeted)",
                   tags$head(tags$style(
                     HTML(
                       ".tabbable > .nav.nav-tabs { display: flex; justify-content: center; }"  # Custom CSS to center the navigation tabs within this panel.
                     )
                   )),
                   
                   tabsetPanel(
                     # Sub-tab 1: Specific Pathogens Trend for qPCR
                     tabPanel(
                       "Specific Pathogens Trend for qPCR",
                       tags$h2(tags$b("Quantification of Specific Pathogens in Wastewater")),  # Main title for this subsection.
                       tags$h5("This approach provides fast, quantitative measurements on a variety of known virus pathogens. Only recently detected pathogens from the targeted list are shown."),  # Description of the qPCR methodology and its focus.
                       tags$hr(),  # Visual separator.
                       fluidRow(
                         column(
                           width = 2,
                           align = "left",
                           box(
                             title = "Control Panel",  # Box for input controls.
                             inline = TRUE,  # Option for inline display of contents.
                             status = "primary",  # Styling option for importance.
                             solidHeader = TRUE,  # Visual distinction for the header.
                             radioButtons("qpcrViewType", "", choices = list("View By City" = 'city', "View By Variant" = 'variant'), selected = 'city', inline = TRUE),  # Radio buttons for selecting data view type.
                             uiOutput("qpcrSelectionUI"),  # Dynamic UI output based on selection.
                             checkboxInput("qpcrPlotToggle", "Single/Multiple Plot", FALSE),  # Checkbox to toggle plot views.
                             sliderInput("qpcrDateRange", "Select Date Range:", min = minDate_qpcr, max = maxDate_qpcr, value = c(minDate_qpcr, maxDate_qpcr), timeFormat = "%Y-%m-%d"),  # Slider for date range selection.
                             checkboxGroupInput("qpcrQuickDateRange", "Quick Date Range:", choices = list("Past 6 Months" = "6m"), selected = NULL)  # Quick selection for date ranges.
                           )
                         ),
                         column(
                           width = 10,
                           align = "left",
                           box(
                             title = "",  # No title for a cleaner look.
                             status = "primary",  # Box styling indicating importance.
                             solidHeader = TRUE,  # Ensures header is visually distinct.
                             fluidRow(plotlyOutput("qpcr_TrendPlot")),  # Output for displaying the qPCR trend plot.
                             div(class = 'qpcr_daterange-text', textOutput("qpcr_DateRange"))  # Div to display selected date range.
                           )
                         )
                       )
                     )
                   )
                 )
               
               
               )
           )
)
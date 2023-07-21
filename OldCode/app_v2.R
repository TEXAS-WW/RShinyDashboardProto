library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(plotly)
library(xtable)
library(DT)
library(shinythemes)
library(maps)
#library(ggChernoff)
#library(emoGG)
#library(emojifont)

source("Read_Data.R")
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


ui <- fluidPage(
  
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
             tags$h4("This should be a blob of text to describe this page")
    ),
    
    
    # Define the UI components for the third tab
    tabPanel("Time Trend",
             tags$h2(tags$b("qPCR(Targeted)")),
             tags$h4("Blob of Text to Explain this Tab"),
             tags$hr(),
             fluidRow(
               tags$head(
                 tags$style(HTML("
                      .shiny-options-group {
                        display: inline-block;
                        align-items: center;
                        justify-content: center;
                      }
                      .shiny-options-group label {
                        margin-right: 22px;
                      }
                      input:radio:checked + label {
                        color: green;
                      }
                    "))
               ),
               column(
                 width = 4,
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
                   checkboxInput("cityToggle", "View by city", FALSE)
                   # conditionalPanel(
                   #   condition = "input.viewType == 'location'",
                   #   
                   #   tagList(
                   #     selectizeInput("locationInput", "Select Location:",
                   #                 choices = WWTP$WWTP[-1],
                   #                 selected = WWTP$WWTP[1]),
                   #     checkboxGroupInput("variantInput", "Select Variant(s):",
                   #                    choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
                   #                    selected = "SARSCOV2N1"
                   #                    ),
                   #     checkboxInput("caseToggle", "Clinical Case Data", FALSE),
                   #     checkboxInput("cityToggle", "View by city", FALSE)
                   #   )
                   # 
                   # ),
                   # 
                   # conditionalPanel(
                   #   condition = "input.viewType == 'variant'",
                   #   tagList(
                   #     selectizeInput("variantInput", "Select Variant:",
                   #                 choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
                   #                 selected = "SARSCOV2N1"),
                   #     selectizeInput("locationInput", "Select Location(s):",
                   #                    choices = WWTP$WWTP[-1],
                   #                    selected = WWTP$WWTP[1]) ,
                   #     checkboxInput("caseToggle", "Clinical Case Data", FALSE),
                   #     checkboxInput("cityToggle", "View by city", FALSE)
                   #   )
                   # 
                   # )

                   )
                 ),
               column(
                 width = 8,
                 box(
                   title = "Variant Analysis Plot",
                   width = 8,
                   status = "primary",
                   solidHeader = TRUE,
                   plotlyOutput("trendPlot")
                 )
               )
               )
                  )
             )
)

# Define a color palette for the legend
server <- function(input, output, session) {
  
  
  # Render the leaflet map in the first tab
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.56666, lat = 31, zoom = 8)
  })
  
  observeEvent(input$countySelect, {
    if (input$countySelect == "None") {
      central_view <- c(min(merged_CountyWWTP$totalWWTP), max(merged_CountyWWTP$totalWWTP))
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        addPolygons(data = merged_CountyWWTP, fillColor = ~Color,
                    fillOpacity = ~FillOpacity,
                    color = "black",
                    weight = ~Weight
                    #label = ~paste0(NAMELSAD, "<br>","Numbers of Site: ", wwtp)
        ) %>%
        setView(lng = -99.56666, lat = 31, zoom = 6) %>%
        addCircleMarkers(
          data = CountyWWTP,
          lng = CountyWWTP$centroid_lon,
          lat = CountyWWTP$centroid_lat,
          radius = ~Radius,
          color = ~Color,
          fillOpacity = 0.9,
          fill = TRUE,
          opacity = 0.9,
          label = ~totalWWTP,
          labelOptions = labelOptions(noHide = TRUE,
                                      textOnly = TRUE,
                                      direction = "center",
                                      style = list("font-weight" = "bold"),
                                      textsize = "12px"
          )
        )
      
    } else {
      
      central_view <- c(min(county_data_shp$wwtp), max(county_data_shp$wwtp))
      county_selected <- WWTP[which(WWTP$County == input$countySelect),]
      
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearPopups() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = unique(WWTP$centroid_lon[which(WWTP$County == input$countySelect)]), 
                lat = unique(WWTP$centroid_lat[which(WWTP$County == input$countySelect)]), 
                zoom = 9)  %>%
        addCircleMarkers(
          data = county_selected,
          lng = county_selected$lon,
          lat = county_selected$lat,
          radius = 6,
          stroke = FALSE,
          fillColor = "red",
          fillOpacity = 1,
          weight = 5,
          label = ~WWTP,
          labelOptions = labelOptions(noHide = FALSE,
                                      direction = "center",
                                      textsize = "12px",
                                      style = list("font-weight" = "bold")))  
      
      
    }   
  })
  
  # 
  # observe({
  #   updateSelectizeInput(session, 
  #                        "locationInput", 
  #                        choices = if(input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1])
  # })
# 
#   observeEvent(c(input$viewType, input$cityToggle), {
#     req(input$viewType == 'variant' || input$viewType == 'location') 
#     updateSelectizeInput(session, "locationInput",
#                          choices = if(input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1],
#                          selected = if(input$cityToggle) WWTP$County[1] else WWTP$WWTP[1]
#     )
#   })
#   


output$selectionUI <- renderUI({

  if (input$viewType == 'location') {

    div(
      tags$hr("Compare multiple wastewater site locations for a single variant."),
      selectInput("locationInput", "Select Location:",
                  choices = WWTP$WWTP[-1],
                  selected = WWTP$WWTP[1]),

      selectizeInput("variantInput", "Select Variant(s):",
                     choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
                     selected = "SARSCOV2N1",
                     multiple = TRUE)
    )
    # When viewing by location, allow selecting one location and multiple variants
  } else if (input$viewType == 'variant') {
    # When viewing by variant, allow selecting one variant and multiple locations
    div(
      selectInput("variantInput", "Select Variant:",
                  choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
                  selected = "SARSCOV2N1"),

      selectizeInput("locationInput", "Select Location(s):",
                     choices = WWTP$WWTP[-1],
                     multiple = TRUE,
                     selected = WWTP$WWTP[1])
    )

  }
})
observeEvent(input$cityToggle, {
  updateSelectInput(session, "locationInput",
                    choices = if (input$cityToggle) WWTP$County[-1] else WWTP$WWTP[-1],
                    selected = if (input$cityToggle) WWTP$County[1] else WWTP$WWTP[1])
})
  
  

# Define a reactive expression that creates a filtered data frame based on the inputs
filtered_data <- reactive({
  req(input$locationInput, input$variantInput)  # Ensure these inputs are defined
  
  if (input$cityToggle == FALSE & input$caseToggle == FALSE) {
    # Filter data based on location and variant inputs
    df <- comb_qPCR_table_average %>%
      filter(Site %in% input$locationInput & Target %in% input$variantInput) %>%
      group_by(Site, Target) %>%
      filter(n_distinct(Week) >= 3) %>%
      arrange(Site, Target, Week) %>%
      mutate(moving_average = ma(average_genome_copies_L)) %>%
      drop_na() %>%
      ungroup()
  } else if (input$caseToggle == TRUE | input$cityToggle == TRUE) {
          if (!(input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
                   input$variantInput == "SARSCOV2N1"))

                df = data.frame()
               # no_data_plot = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) +
    #             annotate("text", x=5, y=5, size=5, col="red",
    #                      label="Clinical Case Data") +
    #             annotate("text", x=5, y=3, size=5, col="red",
    #                      label="Only available for El Paso and SARSCOV2N1 DATA") +
    #             #geom_text(aes(x=5, y=8),emoji('smile'),family='EmojiOne', size=5) +
    #             theme(axis.title.x = element_blank(),
    #                   axis.title.y = element_blank(),
    #                   axis.ticks.x=element_blank(),
    #                   axis.ticks.y=element_blank() )
    # 
    # 
    #           reactive_value$trendPlot = ggplotly(no_data_plot)}
    # 
            if (input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
                input$variantInput == "SARSCOV2N1")
            {
              filtered_data <- organized.data %>%
                filter(
                  WWTPSite == input$locationInput
                )
    #           break.vec <- c(seq(from = as.Date("2021-11-18"), to = as.Date("2022-06-01"), by = "week"))
    #           qPCR_ElPaso = ggplot(filtered_data, aes(x=Date)) +
    #             geom_line(aes(y = N1, colour = "N1")) +
    #             geom_line(aes(y = N2, colour = "N2")) +
    #             scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    #             labs(x ="", y = "Waste water concentration (cp/L) in 10k",colour=NULL)+
    #             scale_x_date(breaks = break.vec, date_breaks="4 weeks", date_labels = "%y-%m-%d")+
    #             theme_bw() +
    #             theme(legend.position=c(0.9,0.9),legend.text = element_text(size=7),
    #                   axis.text.x =element_text (angle=45,hjust=1, size=6),
    #                   axis.text.y =element_text (size=6), axis.title.y = element_text(size=7))
    # 
    #           reactive_value$trendPlot = ggplotly(qPCR_ElPaso)
    #         }
            }
  }
  df
})

# Render the plot based on the filtered data
output$trendPlot <- renderPlotly({
  req(nrow(filtered_data()) > 0)  # Ensure filtered data is not empty
  
  plot <- ggplot(filtered_data(), aes(x = Week, y = moving_average, color = Target, group = Target)) +
    geom_line(size = 0.7, alpha = 0.9) +
    scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::scientific) +
    labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  ggplotly(plot)
})

  # Render the data analysis plot 2
# 
#     reactive_value = reactiveValues(trendPlot = NULL)
# 
#     listInput = reactive({list(input$locationInput,
#                                input$variantInput,
#                                input$caseToggle,
#                                input$cityToggle)})
#     observeEvent(listInput(), {
# 
#       if ((input$caseToggle == FALSE & input$cityToggle == FALSE)) {
#         filtered_data <- comb_qPCR_table_average %>%
#           filter(
#             Site == input$locationInput &
#               Target %in% input$variantInput
#           ) %>%
#           group_by(Site, Target) %>%
#           filter(n_distinct(Week) >= 3) %>%
#           arrange(Site, Target, Week) %>%
#           mutate(moving_average = ma(average_genome_copies_L)) %>%
#           drop_na() %>%
#           ungroup()
#         reactive_value$trendPlot <- ggplotly( ggplot(filtered_data, aes(x = Week, y = moving_average, color = Target, group = Target)) +
#                                                 geom_line(size = 0.7, alpha = 0.9) +
#                                                 scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
#                                                 scale_y_continuous(labels = scales::scientific) +
#                                                 labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
#                                                 theme_bw() +
#                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)))
# 
#       } else if (input$caseToggle == TRUE | input$cityToggle == TRUE){
# 
#         if (!(input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
#               input$variantInput == "SARSCOV2N1"))
#         {#reactive_value$text = print("Only available for El Paso and SARSCOV2N1 DATA")
#           df = data.frame()
#           no_data_plot = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) +
#             annotate("text", x=5, y=5, size=5, col="red",
#                      label="Clinical Case Data") +
#             annotate("text", x=5, y=3, size=5, col="red",
#                      label="Only available for El Paso and SARSCOV2N1 DATA") +
#             #geom_text(aes(x=5, y=8),emoji('smile'),family='EmojiOne', size=5) +
#             theme(axis.title.x = element_blank(),
#                   axis.title.y = element_blank(),
#                   axis.ticks.x=element_blank(),
#                   axis.ticks.y=element_blank() )
# 
# 
#           reactive_value$trendPlot = ggplotly(no_data_plot)}
# 
#         if (input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
#             input$variantInput == "SARSCOV2N1")
#         {
#           filtered_data <- organized.data %>%
#             filter(
#               WWTPSite == input$locationInput
#             )
#           break.vec <- c(seq(from = as.Date("2021-11-18"), to = as.Date("2022-06-01"), by = "week"))
#           qPCR_ElPaso = ggplot(filtered_data, aes(x=Date)) +
#             geom_line(aes(y = N1, colour = "N1")) +
#             geom_line(aes(y = N2, colour = "N2")) +
#             scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
#             labs(x ="", y = "Waste water concentration (cp/L) in 10k",colour=NULL)+
#             scale_x_date(breaks = break.vec, date_breaks="4 weeks", date_labels = "%y-%m-%d")+
#             theme_bw() +
#             theme(legend.position=c(0.9,0.9),legend.text = element_text(size=7),
#                   axis.text.x =element_text (angle=45,hjust=1, size=6),
#                   axis.text.y =element_text (size=6), axis.title.y = element_text(size=7))
# 
#           reactive_value$trendPlot = ggplotly(qPCR_ElPaso)
#         }
#       }
# 
#       output$trendPlot = renderPlotly({reactive_value$trendPlot})
# 
#   })
}


##################################


options(shiny.port = 8100)

runApp(shinyApp(ui = ui, server = server ))

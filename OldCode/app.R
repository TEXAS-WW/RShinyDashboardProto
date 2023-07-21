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
             leafletOutput("map", width = "55%"),
             tags$h4("This should be a blob of text to describe this page")
    ),
    
    
    # Define the UI components for the third tab
    tabPanel("Time Trend",
             tags$h2(tags$b("qPCR(Targeted)")),
             tags$h4("Blob of Text to Explain this Tab"),
             tags$hr(),
             fluidRow(

               # column(
               #   
               #   width = 5,
               #   box(
               #     title = "Variant Analysis",
               #     status = "primary",
               #     solidHeader = TRUE,
               #   radioButtons("viewType", "View Type:",
               #                choices = list("View By Location" = 'location',
               #                               "View By Variant" = 'variant'),
               #                selected = 'location'),
               #   checkboxInput("caseToggle", "Clinical Case Data", FALSE),
               #   conditionalPanel(
               #     condition = "input.viewType == 'location'",
               #     checkboxInput("cityToggle", "View by city", FALSE),
               #     if(input$cityToggle) {
               #       choices <- WWTP$City  # fill in city choices here
               #     } else {
               #       choices <- WWTP$County  # fill in county choices here
               #     },
               #     selectInput("locationInput", "Select Location:", 
               #                 choices = choices,
               #                 selected = choices[1]),
               #     checkboxGroupInput("variantInput", "Select Variants:",
               #                        choices = c("SARSCOV2N1", 
               #                                    "INFLUENZAA", 
               #                                    "INFLUENZAB", 
               #                                    "NOROVIRUS", 
               #                                    "MONKEYPOX"),
               #                        selected = "SARSCOV2N1")
               #     # uiOutput("locationUI"),
               #     # uiOutput("variantUI")
               #     
               #   ),
               #   conditionalPanel(
               #     condition = "input.viewType == 'variant'",
               #     checkboxInput("cityToggle", "View by city", FALSE),
               #     if(input$cityToggle) {
               #       choices <- WWTP$City  # fill in city choices here
               #     } else {
               #       choices <- WWTP$County  # fill in county choices here
               #     },
               #     
               #     selectInput("variantInput", "Select Variant:",
               #                 choices = c("SARSCOV2N1", 
               #                             "INFLUENZAA", 
               #                             "INFLUENZAB", 
               #                             "NOROVIRUS", 
               #                             "MONKEYPOX"),
               #                 selected = "SARSCOV2N1"),
               #     selectizeInput("locationInput", "Select Locations:", 
               #                    choices = choices, 
               #                    multiple = TRUE,
               #                    selected = choices[1])
               #     # uiOutput("variantUI"),
               #     # uiOutput("locationUI")
               # ))
               # ),


               column(
                 width = 3,
               box(
                 title = "Variant Analysis",
                 status = "primary",
                 solidHeader = TRUE,
                      selectInput("locationInput",
                                  "Collection Site:",
                                  choices = WWTP$WWTP[-1]),

               checkboxGroupInput("variantInput", "Select Variant(s):",
                                  choices = c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", "NOROVIRUS", "MONKEYPOX"),
                                 # multiple = TRUE,
                                  selected = c("SARSCOV2N1", "INFLUENZAA")),


               checkboxInput("caseToggle", "Clinical Case Data", FALSE)
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
                  #textOutput("text")
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
  
  # # Location UI
  # output$locationUI <- renderUI({
  #   if(input$cityToggle) {
  #     choices <- WWTP$City  # fill in city choices here
  #   } else {
  #     choices <- WWTP$County  # fill in county choices here
  #   }
  # 
  #   if (input$viewType == 'location') {
  #     selectInput("locationInput", "Select Location:", choices = choices)
  #   } else {
  #     selectizeInput("locationInput", "Select Locations:", choices = choices, multiple = TRUE)
  #   }
  # })
  # 
  # # Variant UI
  # output$variantUI <- renderUI({
  # 
  #   if (input$viewType == 'location') {
  #     checkboxGroupInput("variantInput", "Select Variants:",
  #                        choices = c("SARSCOV2N1",
  #                                    "INFLUENZAA",
  #                                    "INFLUENZAB",
  #                                    "NOROVIRUS",
  #                                    "MONKEYPOX"))
  #   } else {
  #     selectInput("variantInput", "Select Variant:",
  #                 choices = c("SARSCOV2N1",
  #                             "INFLUENZAA",
  #                             "INFLUENZAB",
  #                             "NOROVIRUS",
  #                             "MONKEYPOX"))
  #   }
  # })
 # Render the data analysis plot 2

  reactive_value = reactiveValues(trendPlot = NULL, text = NULL)

  listInput = reactive({list(input$locationInput,
                             input$variantInput,
                             input$caseToggle,
                             input$cityToggle)})
  observeEvent(listInput(), {

    if (input$caseToggle == FALSE) {
    filtered_data <- comb_qPCR_table_average %>%
      filter(
        Site == input$locationInput &
          Target %in% input$variantInput
      ) %>%
      group_by(Site, Target) %>%
      filter(n_distinct(Week) >= 3) %>%
      arrange(Site, Target, Week) %>%
      mutate(moving_average = ma(average_genome_copies_L)) %>%
      drop_na() %>%
      ungroup()
    reactive_value$trendPlot <- ggplotly( ggplot(filtered_data, aes(x = Week, y = moving_average, color = Target, group = Target)) +
     geom_line(size = 0.7, alpha = 0.9) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::scientific) +
      labs(x = "Date", y = "City-wide abundance (genome copies/sample)\n") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)))
    reactive_value$text = NULL

    } else {

      if (!(input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
            input$variantInput == "SARSCOV2N1"))
      {#reactive_value$text = print("Only available for El Paso and SARSCOV2N1 DATA")
      df = data.frame()
      no_data_plot = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) +
        annotate("text", x=5, y=5, size=5, col="red",
                 label="Clinical Case Data") +
        annotate("text", x=5, y=3, size=5, col="red",
        label="Only available for El Paso and SARSCOV2N1 DATA") +
        #geom_text(aes(x=5, y=8),emoji('smile'),family='EmojiOne', size=5) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank() )


       reactive_value$trendPlot = ggplotly(no_data_plot)}

      if (input$locationInput %in% c("Fred Hurvey", "Haskell", "John T. Hickerson", "Roberto Bustamante") &
          input$variantInput == "SARSCOV2N1")
      {
        filtered_data <- organized.data %>%
          filter(
            WWTPSite == input$locationInput
          )
        break.vec <- c(seq(from = as.Date("2021-11-18"), to = as.Date("2022-06-01"), by = "week"))
        qPCR_ElPaso = ggplot(filtered_data, aes(x=Date)) +
          geom_line(aes(y = N1, colour = "N1")) +
          geom_line(aes(y = N2, colour = "N2")) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
          labs(x ="", y = "Waste water concentration (cp/L) in 10k",colour=NULL)+
          scale_x_date(breaks = break.vec, date_breaks="4 weeks", date_labels = "%y-%m-%d")+
          theme_bw() +
          theme(legend.position=c(0.9,0.9),legend.text = element_text(size=7),
                axis.text.x =element_text (angle=45,hjust=1, size=6),
                axis.text.y =element_text (size=6), axis.title.y = element_text(size=7))

        reactive_value$trendPlot = ggplotly(qPCR_ElPaso)
        #reactive_value$text = NULL
      }
    }

  output$trendPlot = renderPlotly({reactive_value$trendPlot})
  #output$text = renderText({reactive_value$text})
    })
}


# 
# ####################################
# 
# u<- shinyUI(fluidPage(
#   titlePanel("title panel"),
#   
#   sidebarLayout(position = "left",
#                 sidebarPanel("sidebar panel",
#                              checkboxInput("do2", "Make 2 plots", value = T)
#                 ),
#                 mainPanel("main panel",
#                           fluidRow(
#                             splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,200,100), 
#                                         plotOutput("plotgraph1"), 
#                                         plotOutput("plotgraph2"),
#                                         plotOutput("plotgraph3")
#                             )
#                           )
#                 )
#   )
# )
# )
# s <- shinyServer(function(input, output){
#   set.seed(1234)
#   pt1 <- qplot(rnorm(500),fill=I("red"),binwidth=0.2,title="plotgraph1")
#   pt3 <- qplot(rnorm(600),fill=I("blue"),binwidth=0.2,title="plotgraph3")
#   pt2 <- reactive({
#     input$do2
#     if (input$do2){
#       return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,title="plotgraph2"))
#     } else {
#       return(NULL)
#     }
#   })
#   output$plotgraph1 = renderPlot({pt1})
#   output$plotgraph2 = renderPlot({pt2()})
#   output$plotgraph3 = renderPlot({pt3}
#   )
# })


##################################


options(shiny.port = 8100)

runApp(shinyApp(ui = ui, server = server ))

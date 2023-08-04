source("DataPreprocessing.R")
source("server.R")
dirpath= file.path(getwd(), 'www')
addResourcePath(prefix = 'www', directoryPath = dirpath)
source("ui.R")

options(shiny.autoreload = TRUE)
  

##################################

options(shiny.port = 8100)
runApp(shinyApp(ui = ui, server = server))


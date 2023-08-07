

source("DataPreprocessing.R")

dirpath= file.path(getwd(), 'www')
addResourcePath(prefix = 'www', directoryPath = dirpath)
source("server.R")

source("ui.R")

options(shiny.autoreload = TRUE)
  

##################################

options(shiny.port = 8100)
runApp(shinyApp(ui = ui, server = server))


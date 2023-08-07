source("DataPreprocessing.R")
source("server.R")
source("ui.R")
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



minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))

minDate_qpcr<- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))

options(shiny.port = 8100)
shinyApp(ui = ui, server = server)

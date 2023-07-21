#Google API Key
library(ggmap)
register_google(key="AIzaSyA4h-W7Ie8An3EnYx3a_LadnKrVUUKLDfs", write=TRUE)

# AIzaSyB6RsVkY1vs1iFB8kQLZcrashAdYEQEmXU

#Census API Key
library(tidycensus)
census_api_key("49626ba8c2a4dc3fc0fa50c345c5f2ba8078e3ff", install="TRUE", overwrite = TRUE)


#census_api_key("9cf4df041974f96e1a6a74d6fbc47dc73bd6934c", install="TRUE", overwrite = TRUE)

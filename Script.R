
####################
####################
# Source Documents #
####################
####################

source("C:/R Portfolio/Leaflet_Maps/Functions.R")
source("C:/R Portfolio/Leaflet_Maps/Data_Objects.R")

# Data ---------------------------------------------
table(GTD_WD_geo$Region)

ALG <- GTD_WD_geo %>% filter(Country == "Algeria")
glimpse(ALG)
table(ALG$City)
unique(ALG$Longitude)

# Filter out rows with missing latitude or longitude values
attacks_data <- ALG %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# Create the map

# This code creates a Leaflet map using the leaflet package in R. Here's a breakdown of what each part does:

# map <- leaflet(data = attacks_data) %>%: This initializes a Leaflet map object using the leaflet() function and assigns it to the variable map. The data argument specifies the data frame (attacks_data) containing the geographic coordinates (latitude and longitude) of the attacks.
# addTiles() %>%: This adds the default map tiles (typically OpenStreetMap) to the Leaflet map. The %>% operator (pipe operator) chains the function call to the previous one, allowing for a more concise syntax.
# addCircleMarkers(: This function adds circle markers to the map to represent the locations of the attacks.
# ~Longitude, ~Latitude,: These specify the longitude and latitude columns from the attacks_data data frame to be used as the coordinates for the circle markers.
# radius = 3, color = "red",: These parameters set the radius and color of the circle markers.
# popup = paste(...): This parameter specifies the content of the popups that appear when you click on the circle markers. The paste() function concatenates strings to form the popup content. Each element inside paste() represents a line in the popup. The iconv() function is used to convert the text content to UTF-8 format, which helps handle any invalid UTF-8 characters that might cause errors.
# "Attack:", iconv(attacks_data$Attack, to = "UTF-8"), "<br>",: This line adds the type of attack to the popup content.
# "Date:", iconv(paste(attacks_data$Year, "-", attacks_data$Month, "-", attacks_data$Day), to = "UTF-8"), "<br>",: This line adds the date of the attack to the popup content.
# "Country:", iconv(attacks_data$Country, to = "UTF-8"), "<br>",: This line adds the country of the attack to the popup content.
# "City:", iconv(attacks_data$City, to = "UTF-8")): This line adds the city of the attack to the popup content.

map <- leaflet(data = attacks_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 3,
    color = "red",
    popup = paste("Attack:", 
                  iconv(attacks_data$Attack, to = "UTF-8"), 
                  "<br>",
                  "Date:", 
                  iconv(paste(attacks_data$Year, 
                              "-", 
                              attacks_data$Month, 
                              "-", 
                              attacks_data$Day), 
                        to = "UTF-8"), 
                  "<br>",
                  "Country:", 
                  iconv(attacks_data$Country, 
                        to = "UTF-8"), "<br>",
                  "City:",
                  iconv(attacks_data$City, 
                        to = "UTF-8"))
  )

# Display the map
map

# Function to change the map terrain type
changeTerrain <- function(terrain) {
  if (terrain == "Default") {
    map %>% addProviderTiles("OpenStreetMap")
  } else if (terrain == "Satellite") {
    map %>% addProviderTiles("Esri.WorldImagery")
  } else if (terrain == "Terrain") {
    map %>% addProviderTiles("Esri.WorldTerrain")
  } else if (terrain == "Topographic") {
    map %>% addProviderTiles("Esri.WorldTopoMap")
  } else {
    print("Invalid terrain type. Choose from Default, Satellite, Terrain, or Topographic.")
  }
}

# Change the map terrain type
changeTerrain("Satellite")

# Zoom into Algiers City:

# Filter the data for Algiers city
algiers_data <- attacks_data %>%
  filter(City == "Algiers")

# Get the center coordinates of Algiers city
center_longitude <- mean(algiers_data$Longitude)
center_latitude <- mean(algiers_data$Latitude)

# Create the map with default tile layer (OpenStreetMap) and zoomed into Algiers city
map <- leaflet(data = algiers_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 3,
    color = "red",
    popup = paste("Attack:", 
                  iconv(algiers_data$Attack, to = "UTF-8"), 
                  "<br>",
                  "Date:", 
                  iconv(paste(algiers_data$Year, 
                              "-", 
                              algiers_data$Month, 
                              "-", 
                              algiers_data$Day), 
                        to = "UTF-8"), 
                  "<br>",
                  "Country:", 
                  iconv(algiers_data$Country, 
                        to = "UTF-8"), "<br>",
                  "City:",
                  iconv(algiers_data$City, 
                        to = "UTF-8"))
  ) %>%
  setView(center_longitude, center_latitude, zoom = 14)  # Adjust the zoom level as needed

# Display the map
map

# Change the map terrain type
changeTerrain("Satellite")
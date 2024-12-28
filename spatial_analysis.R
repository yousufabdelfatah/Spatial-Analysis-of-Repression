
# Set Up ------------------------------------------------------------------


# load packages

library(sf)
library(tidyverse)
library(leaflet)
library(spdep)
library(spatialreg)

# load shapefiles
admin3_boundaries <- st_read("egy_admbnda_adm3_capmas_20170421/egy_admbnda_adm3_capmas_20170421.shp")


# load the csvs
arrests <- read_csv("arrests.csv")
locations_1 <- read_csv("locations_2011_2018.csv")
locations_2 <- read_csv("locations_2019_2020.csv")

# original protest excel files
df_2011_2018 <- readxl::read_excel("event_catalogue_2_2011-2018 - academic_version.xlsx", skip = 1)
df_2019_2020 <- readxl::read_excel("event_catalogue_2019-2020 - academic_version.xlsx")

# Data Manipulation -------------------------------------------------------

# plot shapefiles

plot(shape_adm3)

# need to add in the dates from the excel files

locations_1$protest_date <- as.POSIXct(df_2011_2018[, 2]$`تاريخ_الفعالية`)

locations_2$protest_date <- as.POSIXct(df_2019_2020[, 2]$`تاريخ الفعالية`)

locations_1$protest_type <- as.factor(df_2011_2018$نوع_الفعالية)

locations_2$protest_type <- as.factor(df_2019_2020$`نوع الفعالية`)

locations_1$focal_point <- as.factor(df_2011_2018$حدث_سياسي_عام_مرتبط)

locations_2$focal_point <- as.factor(df_2019_2020$`حدث سياسي عام مرتبط`)

locations_1$organizers <- as.factor(df_2011_2018$نوع_الجهة_المنظمة_للفعالية)

locations_2$organizers <- as.factor(df_2019_2020$`نوع الجهة المُنظمة`)

# can we just bind the location data on top of each other

protest_locations <- rbind(locations_1, locations_2)
rm(locations_1, locations_2)

# get rid of nas in locations data
arrests <- arrests %>% 
  drop_na(lat)

protest_locations <- protest_locations %>% 
  drop_na(lat)

# Map Protests and Arrests ------------------------------------------------

# Convert data frames to spatial objects
arrests_sf <- st_as_sf(arrests, coords = c("lon", "lat"), crs = 4326)
protests_sf <- st_as_sf(protest_locations, coords = c("lon", "lat"), crs = 4326)

#bounding box
bbox <- st_bbox(admin3_boundaries)

# Plot the map with shapefile
ggplot() +
  geom_sf(data = admin3_boundaries, fill = "gray80", color = "black") +  # Plot the shapefile
  geom_sf(data = arrests_sf, aes(color = "Arrests"), size = 2, alpha = 0.5) +
  geom_sf(data = protests_sf, aes(color = "Protests"), size = 2, alpha = 0.2) +
  scale_color_manual(values = c("Arrests" = "red", "Protests" = "blue")) +
  theme_minimal() +
  labs(title = "Map of Arrests and Protests",
       color = "Event Type") +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))


# let's say we want to filter for Cairo

Cairo <- admin3_boundaries %>% 
  filter(ADM1_EN == "Cairo")

cairo_bbox <- st_bbox(Cairo)

# Plot the map with shapefile
ggplot() +
  geom_sf(data = Cairo, fill = "gray80", color = "black") +  # Plot the shapefile
  geom_sf(data = arrests_sf, aes(color = "Arrests"), size = 2, alpha = 0.5) +
  geom_sf(data = st_jitter(protests_sf, amount = 0.01), aes(color = "Protests"), size = 2, alpha = 0.2) +
  scale_color_manual(values = c("Arrests" = "red", "Protests" = "blue")) +
  theme_minimal() +
  labs(title = "Map of Arrests and Protests",
       color = "Event Type") +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(cairo_bbox["xmin"], cairo_bbox["xmax"]), ylim = c(cairo_bbox["ymin"], cairo_bbox["ymax"]))

# interactive with leaflet
# Create a leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = Cairo, 
              fillColor = "gray80", 
              color = "black", 
              weight = 1, 
              opacity = 1, 
              fillOpacity = 0.5,
              label = ~paste(ADM1_EN),
              popup = ~paste("Administrative Unit:", ADM1_EN)) %>%
  addCircleMarkers(data = arrests_sf, 
                   color = "red", 
                   radius = 1, 
                   fillOpacity = 0.8,
                   group = "Arrests") %>% 
  addCircleMarkers(data = protests_sf, 
                   color = "blue", 
                   radius = 1, 
                   fillOpacity = 0.5,
                   group = "Protests") %>% 
  setView(lng = mean(cairo_bbox[c("xmin", "xmax")]), 
          lat = mean(cairo_bbox[c("ymin", "ymax")]), 
          zoom = 10) %>% 
  addLegend("bottomright",
            colors = c("red", "blue"),
            labels = c("Arrest", "Protest"),
            title = "Event Type")

# Save the leaflet map as an image
mapview::mapshot(map, file = "leaflet_map_Cairo.png")

# Alexandria mao
Alex <- admin3_boundaries %>% 
  filter(ADM1_EN == "Alexandria")

Alex_bbox <- st_bbox(Alex)

map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = Alex, 
              fillColor = "gray80", 
              color = "black", 
              weight = 1, 
              opacity = 1, 
              fillOpacity = 0.5,
              label = ~paste(ADM1_EN),
              popup = ~paste("Administrative Unit:", ADM1_EN)) %>%
  addCircleMarkers(data = arrests_sf, 
                   color = "red", 
                   radius = 1, 
                   fillOpacity = 0.8,
                   group = "Arrests") %>% 
  addCircleMarkers(data = protests_sf, 
                   color = "blue", 
                   radius = 1, 
                   fillOpacity = 0.5,
                   group = "Protests") %>% 
  setView(lng = mean(Alex_bbox[c("xmin", "xmax")]), 
          lat = mean(Alex_bbox[c("ymin", "ymax")]), 
          zoom = 10) %>% 
  addLegend("bottomright",
            colors = c("red", "blue"),
            labels = c("Arrest", "Protest"),
            title = "Event Type")

mapview::mapshot(map, file = "leaflet_map_Alex.png")

# Spatial regressions -----------------------------------------------------

# Convert to spatial objects
arrests_sf <- st_as_sf(arrests, coords = c("lon", "lat"), crs = 4326)
protests_sf <- st_as_sf(protest_locations, coords = c("lon", "lat"), crs = 4326)

# Perform spatial join to assign each arrest and protest to an admin3 boundary
arrests_admin3 <- st_join(arrests_sf, admin3_boundaries, join = st_within) %>%
  st_drop_geometry() %>%
  select(arrest_date, ADM3_PCODE)

protests_admin3 <- st_join(protests_sf, admin3_boundaries, join = st_within) %>%
  st_drop_geometry() %>%
  select(protest_date, ADM3_PCODE)

# Aggregate data by admin3 boundaries and week
arrests_admin3 <- arrests_admin3 %>%
  mutate(week_date = floor_date(arrest_date, unit = "week")) %>%
  group_by(ADM3_PCODE, week_date) %>%
  summarize(arrests_count = n(), .groups = 'drop')

protests_admin3 <- protests_admin3 %>%
  mutate(week_date = floor_date(protest_date, unit = "week")) %>%
  group_by(ADM3_PCODE, week_date) %>%
  summarize(protests_count = n(), .groups = 'drop')


# Lag arrests data by one time period (e.g., one week)
arrests_admin3_lagged <- arrests_admin3 %>%
  mutate(week_date = week_date + weeks(1))

# Merge lagged arrests data with protests data based on ADM3_PCODE and week_date
merged_data <- protests_admin3 %>%
  left_join(arrests_admin3_lagged, by = c("ADM3_PCODE", "week_date")) %>%
  rename(arrests_count_t0 = arrests_count)

# Join merged data back to admin3 boundaries for spatial context
merged_data_sf <- admin3_boundaries %>%
  left_join(merged_data, by = "ADM3_PCODE")

merged_data_sf <- 
  merged_data_sf %>% 
  drop_na(protests_count) %>% 
  drop_na(arrests_count_t0)

# Define neighbors within a certain distance threshold (e.g., 5km) for admin3 centroids
admin3_centroids <- st_centroid(merged_data_sf)
threshold <- 5000 # distance in meters
neighbors <- dnearneigh(st_coordinates(admin3_centroids), 0, threshold)

# Calculate distances
distances <- nbdists(neighbors, st_coordinates(admin3_centroids))

# Define the Gaussian decay function
gaussian_decay <- function(distance, sigma) {
  exp(- (distance^2) / (2 * sigma^2))
}

# Set sigma (e.g., 2000 meters)
sigma <- 2000

# Calculate Gaussian weights
gaussian_weights <- lapply(distances, function(dist) gaussian_decay(dist, sigma))

# Create spatial weights list
listw <- nb2listw(neighbors, glist = gaussian_weights, style = "W")

# Drop spatial information for the model fitting
merged_data_df <- st_drop_geometry(merged_data_sf)

# Prepare model variables
y <- merged_data_df$protests_count
X <- as.matrix(merged_data_df[, c("arrests_count_t0")])

# Fit the Spatial Durbin Model
sdr_model <- lagsarlm(y ~ X, data = merged_data_df, listw = listw, type = "mixed")
summary(sdr_model)


####### try with arrest counts that are na and make them zero


merged_data_sf <- admin3_boundaries %>%
  left_join(merged_data, by = "ADM3_PCODE")

merged_data_sf$arrests_count_t0[is.na(merged_data_sf$arrests_count_t0)] <- 0

merged_data_sf <- 
  merged_data_sf %>% 
  drop_na(protests_count)

# Define neighbors within a certain distance threshold (e.g., 5km) for admin3 centroids
admin3_centroids <- st_centroid(merged_data_sf)
threshold <- 5000 # distance in meters
neighbors <- dnearneigh(st_coordinates(admin3_centroids), 0, threshold)

# Calculate distances
distances <- nbdists(neighbors, st_coordinates(admin3_centroids))

# Define the Gaussian decay function
gaussian_decay <- function(distance, sigma) {
  exp(- (distance^2) / (2 * sigma^2))
}

# Set sigma (e.g., 2000 meters)
sigma <- 2000

# Calculate Gaussian weights
gaussian_weights <- lapply(distances, function(dist) gaussian_decay(dist, sigma))

# Create spatial weights list
listw <- nb2listw(neighbors, glist = gaussian_weights, style = "W")

# Drop spatial information for the model fitting
merged_data_df <- st_drop_geometry(merged_data_sf)

merged_data_df$arrests_count_t0_sq <- merged_data_df$arrests_count_t0^2

# Prepare model variables
y <- merged_data_df$protests_count
X <- as.matrix(merged_data_df[, c("arrests_count_t0")])

# Fit the Spatial Durbin Model
sdr_model_2 <- lagsarlm(y ~ X, data = merged_data_df, listw = listw, type = "mixed")
summary(sdr_model_2)

# Let's try with a polynomial
X <- as.matrix(merged_data_df[, c("arrests_count_t0", "arrests_count_t0_sq")])

sdr_model_poly <- lagsarlm(y ~ X, data = merged_data_df, listw = listw, type = "mixed")
summary(sdr_model_poly)


# and let's try it with just a dumb lm
# Create spatially lagged arrest variables
arrests_count_t0_lag <- lag.listw(listw, merged_data_df$arrests_count_t0, zero.policy = TRUE)
arrests_count_t0_sq_lag <- lag.listw(listw, merged_data_df$arrests_count_t0_sq, zero.policy = TRUE)

WX <- data.frame(arrests_count_t0_lag, arrests_count_t0_sq_lag)

# Combine into a single dataframe for regression
model_data <- cbind(y, X, WX)

# Fit the standard linear regression model
model <- lm(y ~ ., data = model_data)
summary(model)


# do we think it's coming up NA bc the distance is too big or too small
# if it still comes up as NA try queen neighbor

# Define queen neighbors for admin3 centroids
queen_neighbors <- poly2nb(merged_data_sf, queen = TRUE)

# Identify regions with no neighbors
no_neighbors <- which(card(queen_neighbors) == 0)

# Remove regions with no neighbors from the dataset
if (length(no_neighbors) > 0) {
  merged_data_sf <- merged_data_sf[-no_neighbors, ]
  queen_neighbors <- poly2nb(merged_data_sf, queen = TRUE)
}

# Create spatial weights list using binary weights
listw_queen <- nb2listw(queen_neighbors, style = "W")

# Drop spatial information for the model fitting
merged_data_df <- st_drop_geometry(merged_data_sf)
merged_data_df$arrests_count_t0_sq <- merged_data_df$arrests_count_t0^2

# Prepare model variables
y <- merged_data_df$protests_count
X <- as.matrix(merged_data_df[, c("arrests_count_t0")])

# Fit the Spatial Durbin Model
sdr_model_queen <- lagsarlm(y ~ X, data = merged_data_df, listw = listw_queen, type = "mixed")
summary(sdr_model_queen)

# polynomial model
X <- as.matrix(merged_data_df[, c("arrests_count_t0", "arrests_count_t0_sq")])

sdr_model_queen_poly <- lagsarlm(y ~ X, data = merged_data_df, listw = listw_queen, type = "mixed")
summary(sdr_model_queen_poly)

summary(sdr_model_queen_poly)

# regular lm model
# Create spatially lagged arrest variables
arrests_count_t0_lag <- lag.listw(listw_queen, merged_data_df$arrests_count_t0, zero.policy = TRUE)
arrests_count_t0_sq_lag <- lag.listw(listw_queen, merged_data_df$arrests_count_t0_sq, zero.policy = TRUE)

WX <- data.frame(arrests_count_t0_lag, arrests_count_t0_sq_lag)

# Combine into a single dataframe for regression
model_data <- cbind(y, X, WX)

# Fit the standard linear regression model
model_queen <- lm(y ~ ., data = model_data)
summary(model_queen)


#fit non-poly
X <- as.matrix(merged_data_df[, c("arrests_count_t0")])
WX <- data.frame(arrests_count_t0_lag)
model_data <- cbind(y, X, WX)
model_queen_sing <- lm(y ~ ., data = model_data)
summary(model_queen_sing)




# Plot Effects ------------------------------------------------------------
impacts <- impacts(sdr_model_queen_poly, listw = listw_queen)

# Visualize the impacts
# Extract direct and indirect impacts
direct_impacts <- impacts$direct
indirect_impacts <- impacts$indirect

# Combine impacts into a data frame
impact_df <- data.frame(
  Variable = c("Arrests", "Arrests^2"),
  Direct_Impact = direct_impacts,
  Indirect_Impact = indirect_impacts
)

# Plot impacts using ggplot2
ggplot(impact_df, aes(x = Variable)) +
  geom_bar(aes(y = Direct_Impact, fill = "Direct"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Indirect_Impact, fill = "Indirect"), stat = "identity", position = "dodge") +
  labs(title = "Direct and Indirect Impacts (Diffusion)",
       x = "Variables",
       y = "Impact") +
  scale_fill_manual(name = "Impact Type", values = c("Direct" = "blue", "Indirect" = "red")) +
  theme_minimal()


# Extract total impacts
total_impacts <- impacts$total

# Add total impacts to the data frame
impact_df$Total_Impact <- total_impacts

# Plot total impacts using ggplot2
ggplot(impact_df, aes(x = Variable, y = Total_Impact, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Impacts",
       x = "Variables",
       y = "Total Impact") +
  theme_minimal()


stargazer::stargazer(sdr_model_queen, sdr_model_queen_poly)



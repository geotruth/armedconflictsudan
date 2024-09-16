# remotes::install_github("walkerke/mapgl")
library(mapgl)
library(sf)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Define the bounding box for Dallas (approximately)
dallas_bbox <- st_bbox(c(
  xmin = -97.0,
  ymin = 32.6,
  xmax = -96.5,
  ymax = 33.0
), crs = st_crs(4326))

# Generate 1 million random points within the Dallas bounding box
random_points <- st_as_sf(
  data.frame(
    id = 1:1000000,
    lon = runif(1000000, dallas_bbox["xmin"], dallas_bbox["xmax"]),
    lat = runif(1000000, dallas_bbox["ymin"], dallas_bbox["ymax"])
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# Create the map with clustered circles
map <- mapboxgl(style = carto_style("positron"), 
                center = c(-96.75, 32.8), zoom = 7) %>%
  add_circle_layer(
    id = "dallas-points",
    source = random_points,
    circle_color = "#11b4da",
    circle_radius = 4,
    circle_stroke_width = 1,
    circle_stroke_color = "#fff",
    cluster_options = cluster_options(
      circle_blur = 0.25
    )
  )

# Display the map
map

usethis::edit_r_environ()
library(use)
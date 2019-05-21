library(dggridR)
library(robis)
library(tidyverse)
library(viridis)
library(mapproj)
library(scales)

# parameters

startdepth <- 3000
enddepth <- 6000
var <- "records" # which variable to plot
folder <- "maps_overall" # output folder

# fetch data from OBIS

data <- occurrence(startdepth = startdepth, enddepth = enddepth, exclude = "bath_issue", fields = c("decimalLongitude", "decimalLatitude", "aphiaID", "scientificName", "speciesid"))
data <- data %>% filter(decimalLatitude < 88) # fix map artifacts

# generate grid

dggs <- dgconstruct(projection = "ISEA", area = 75000, resround = "down")

# assign cells to data

data$cell <- dgtransform(dggs, data$decimalLatitude, data$decimalLongitude)

# aggregate data on grid

stats <- data %>% group_by(cell) %>% summarise(records = n(), species = length(unique(speciesid)))
grid <- dgcellstogrid(dggs, stats$cell, frame = TRUE)
grid <- merge(grid, stats, by.x = "cell", by.y = "cell")

# generate map

ggplot() + 
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "#dddddd", color = NA) +
  geom_polygon(data = grid, aes_string(x = "long", y = "lat", group = "group", fill = var), color = "black", size = 0) +
  scale_fill_viridis(trans = "log10") +
  coord_quickmap() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "#fafafa")
  ) +
  xlim(c(-180, 180)) +
  xlab("longitude") +
  ylab("latitude")

dir.create(folder)
ggsave(paste0(folder, "/", var, "_", startdepth, "_", enddepth, ".png"), scale = 1.2, dpi = 600, width = 12, height = 6)

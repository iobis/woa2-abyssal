library(dggridR)
library(robis)
library(tidyverse)
library(viridis)
library(mapproj)
library(scales)
library(obistools)

# parameters

startdepth <- 2000
enddepth <- 6000
depth_delta <- 500 # minimal difference between sample depth and bathymetry to be assigned to pelagic
var <- "records" # which variable to plot
folder <- "maps_component" # output folder
comp <- "benthic" # which component to map

# data with depth info

data <- occurrence(startdepth = startdepth, enddepth = enddepth, exclude = "bath_issue", fields = c("scientificName", "aphiaID", "species", "speciesid", "decimalLongitude", "decimalLatitude", "minimumDepthInMeters", "maximumDepthInMeters", "lifeStage"))
taxa <- unique(data$aphiaID)
#write.csv(data.frame(aphiaid = taxa), file = "taxa.csv", row.names = FALSE) # output for WoRMS

# add bathymetry and determine which are pelagic based on maximumDepthInMeters

data$bathymetry <- lookup_xy(data, shoredistance = FALSE, grids = TRUE)$bathymetry
data$pelagic_depth <- NA
data$pelagic_depth[which(data$maximumDepthInMeters < data$bathymetry - depth_delta)] <- TRUE

# determine which taxa are pelagic according to WoRMS

aphia <- readxl::read_xlsx("WOA_taxa_functional_group_WoRMS_2019-05-19.xlsx")

functional <- aphia %>%
  group_by(AphiaID) %>%
  summarize(
    benthic_aphia = all(functional_group %in% c("benthos", "macrobenthos", "edaphofauna", "hyperbenthos", "epibenthos")),
    pelagic_aphia = all(functional_group %in% c("zooplankton", "phytoplankton", "nekton", "neuston"))
  )

data <- data %>% left_join(functional, by = c("aphiaID" = "AphiaID"))

# determine benthic/pelagic

data$component <- NA

data$component[which(
  (data$pelagic_depth %in% TRUE | data$pelagic_aphia %in% TRUE) & !(data$benthic_aphia %in% TRUE)
)] <- "pelagic"

data$component[which(
  data$benthic_aphia %in% TRUE & !(data$pelagic_aphia %in% TRUE)
)] <- "benthic"

# generate grid

dggs <- dgconstruct(projection = "ISEA", area = 75000, resround = "down")

# assign cells to data

data$cell <- dgtransform(dggs, data$decimalLatitude, data$decimalLongitude)

# aggregate data on grid

stats <- data %>% filter(component == comp) %>% group_by(cell) %>% summarise(records = n(), species = length(unique(speciesid)))
grid <- dgcellstogrid(dggs, stats$cell, frame = TRUE)
grid <- merge(grid, stats, by.x = "cell", by.y = "cell")

# generate maps

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
ggsave(paste0(folder, "/", comp, "_", var, "_", startdepth, "_", enddepth, ".png"), scale = 1.8, dpi = 900)

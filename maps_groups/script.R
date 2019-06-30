library(dggridR)
library(robis)
library(tidyverse)
library(viridis)
library(mapproj)
library(scales)
library(gridExtra)
library(dplyr)

group <- "Gastrotricha"

# fetch data from OBIS

data <- occurrence(group, exclude = "bath_issue", fields = c("decimalLongitude", "decimalLatitude", "speciesid", "minimumDepthInMeters", "maximumDepthInMeters"))
data <- data %>% filter(decimalLatitude < 88) 

# generate grid

dggs <- dgconstruct(projection = "ISEA", area = 75000, resround = "down")

# assign cells to data

data$cell <- dgtransform(dggs, data$decimalLatitude, data$decimalLongitude)

# aggregate data on grid

makeMap <- function(mindepth, maxdepth, var) {

  # summarize
  
  zonedata <- data %>% filter(minimumDepthInMeters >= mindepth & minimumDepthInMeters < maxdepth)
  stats <- zonedata %>% group_by(cell) %>% summarise(records = n(), species = length(unique(speciesid)))
  grid <- dgcellstogrid(dggs, stats$cell, frame = TRUE)
  grid <- merge(grid, stats, by.x = "cell", by.y = "cell")
  
  # generate map
  
  ggplot() + 
    ggtitle(paste0(group, " ", var, " (", mindepth, " - ", maxdepth, " m)")) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "#dddddd", color = NA) +
    geom_polygon(data = grid, aes_string(x = "long", y = "lat", group = "group", fill = var), color = "black", size = 0) +
    scale_fill_viridis(trans = "log10") +
    coord_quickmap(xlim = c(-180, 180)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "#fafafa")
    ) +
    xlim(c(-200, 200)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.title = element_blank()
    ) 
  
}

p1 <- makeMap(0, 200, "records")
p2 <- makeMap(200, 3000, "records")
p3 <- makeMap(3000, 6000, "records")
p4 <- makeMap(6000, 11000, "records")

g <- arrangeGrob(p1, p2, p3, p4, nrow = 2)
g

ggsave(paste0(tolower(group), "_", var, ".png"), g, scale = 1.2, dpi = 600, width = 12, height = 6)

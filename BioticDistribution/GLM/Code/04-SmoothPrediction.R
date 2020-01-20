# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)
library(btb)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-= #
#      Study area
# =-=-=-=-=-=-=-=-=-=-= #
load('./eDriversGrids/Data/egslSimple.RData')


# =-=-=-=-=-=-=-=-=-=-= #
#     Distributions
# =-=-=-=-=-=-=-=-=-=-= #
load('./BioticDistribution/GLM/Data/predictionGLM.RData')

# Spatial information as matrix
XY <- xyFromCell(spPred, 1:ncell(spPred))
colnames(XY) <- c('Longitude','Latitude')

# Occurrence data as matrix
cooc <- as.matrix(spPred)

# Remove all NAs
uid <- apply(cooc, 1, function(x) !all(is.na(x)))
XY <- XY[uid, ]
cooc <- cooc[uid, ]

# Export as sf object
biotic <- cbind(cooc, XY) %>%
          as.data.frame() %>%
          st_as_sf(x = .,
                   coords = c("Longitude", "Latitude"),
                   crs = projection(spPred))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Smoothing functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Kernel weighted smoothing with arbitrary bounding area
#' Adapted from: https://www.r-bloggers.com/kernel-spatial-smoothing-transforming-points-pattern-to-continuous-coverage/
#' @param df sf object (points)
#' @param field weigth field in sf
#' @param bandwith kernel bandwidth (map units)
#' @param resolution output grid resolution (map units)
#' @param grid sf study grid (polygon)
#' @param out_crs EPSG (should be an equal-area projection)
#'
#' @return a raster object
#' @import btb, raster, fasterize, dplyr, plyr, sf
lissage <- function(df, field, bandwidth, resolution, grid, out_crs = 32198) {
  # Kernel
  message("computing kernel...")
  kernel <- df %>%
    cbind(., st_coordinates(.)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(x = X, y = Y, field) %>%
    btb::kernelSmoothing(dfObservations = .,
                         sEPSG = out_crs,
                         iCellSize = resolution,
                         iBandwidth = bandwidth,
                         vQuantiles = NULL,
                         dfCentroids = zone_xy)

  # Values > 1 to 1
  kernel[,field] <- ifelse(kernel[,field,drop=T] > 1, 1, kernel[,field,drop=T])

  # Rasterization
  message("\nrasterizing...")
  raster::raster(xmn = plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
                 ymn = plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor),
                 xmx = plyr::round_any(zone_bbox[3] + bandwidth, resolution, f = ceiling),
                 ymx = plyr::round_any(zone_bbox[4] + bandwidth, resolution, f = ceiling),
                 resolution = resolution) %>%
    fasterize::fasterize(kernel, ., field = field)
}

zoneGrid <- function(studyArea, resolution, bandwidth) {
  # Bounding box
  zone_bbox <- st_bbox(studyArea)

  # Grid for analyses
  studyArea %>%
  dplyr::select(geometry) %>%
  st_make_grid(cellsize = resolution,
               offset = c(plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
                          plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor)),
               what = "centers") %>%
  st_sf() %>%
  st_join(studyArea, join = st_intersects, left = FALSE) %>%
  st_coordinates() %>%
  as_tibble() %>%
  dplyr::select(x = X, y = Y)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Smoothing parameters
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resolution
resolution <- 1000

# Bandwidth
bandwidth <- 5000

# Grid
zone_xy <- zoneGrid(egslSimple, resolution, bandwidth)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Smoothing species distributions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sp <- colnames(st_drop_geometry(biotic))
species <- vector('list', length(sp))
names(species) <- sp

for(i in sp) {
  cat(i, '\r')
  species[[i]] <- lissage(df = biotic,
                          field = i,
                          bandwidth = bandwidth,
                          resolution = resolution,
                          grid = zone_xy)
}

species <- stack(species)

save(species, file = './BioticDistribution/GLM/Data/speciesDistribution.RData')

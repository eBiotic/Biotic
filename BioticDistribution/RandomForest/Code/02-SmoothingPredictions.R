# source('./BioticDistribution/RandomForest/Code/02-SmoothingPredictions.R')
# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)
library(btb)
load('./eDriversGrids/Data/RasterGrid-1000m2.RData')
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
  # message("computing kernel...")
  suppressMessages({
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
  })

  # Values > 1 to 1
  kernel[,field] <- ifelse(kernel[,field,drop=T] > 1, 1, kernel[,field,drop=T])

  # Rasterization
  # message("\nrasterizing...")
  rasterGrid %>%
  # raster::raster(xmn = plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
  #                ymn = plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor),
  #                xmx = plyr::round_any(zone_bbox[3] + bandwidth, resolution, f = ceiling),
  #                ymx = plyr::round_any(zone_bbox[4] + bandwidth, resolution, f = ceiling),
  #                resolution = resolution) %>%
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

# Study area
load('./eDriversGrids/Data/egslSimple.RData')

# Bounding box
zone_bbox <- st_bbox(egslSimple)

# Grid
zone_xy <- zoneGrid(egslSimple, resolution, bandwidth)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-= #
#      Regression
# =-=-=-=-=-=-=-=-=-=-= #
load('./BioticDistribution/RandomForest/Data/RandForest_Regression.RData')

# Spatial information as matrix
XY <- xyFromCell(RF_regression, 1:ncell(RF_regression))
colnames(XY) <- c('Longitude','Latitude')

# Occurrence data as matrix
cooc <- as.matrix(RF_regression)

# Remove all NAs
uid <- apply(cooc, 1, function(x) !all(is.na(x)))
XY <- XY[uid, ]
cooc <- cooc[uid, ]

# Export as sf object
bioticReg <- cbind(cooc, XY) %>%
             as.data.frame() %>%
             st_as_sf(x = .,
                      coords = c("Longitude", "Latitude"),
                      crs = projection(RF_regression))

# =-=-=-=-=-=-=-=-=-=-= #
#     Classification
# =-=-=-=-=-=-=-=-=-=-= #
load('./BioticDistribution/RandomForest/Data/RandForest_Classification.RData')

# Spatial information as matrix
XY <- xyFromCell(RF_classification, 1:ncell(RF_classification))
colnames(XY) <- c('Longitude','Latitude')

# Occurrence data as matrix
cooc <- as.matrix(RF_classification)

# Remove all NAs
uid <- apply(cooc, 1, function(x) !all(is.na(x)))
XY <- XY[uid, ]
cooc <- cooc[uid, ]

# Export as sf object
bioticClass <- cbind(cooc, XY) %>%
               as.data.frame() %>%
               st_as_sf(x = .,
                        coords = c("Longitude", "Latitude"),
                        crs = projection(RF_classification))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Smoothing species distributions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-= #
#      Regression
# =-=-=-=-=-=-=-=-=-=-= #
sp <- colnames(st_drop_geometry(bioticReg))
species <- vector('list', length(sp))
names(species) <- sp

for(i in sp) {
  cat(i, '\r')
  species[[i]] <- lissage(df = bioticReg,
                          field = i,
                          bandwidth = bandwidth,
                          resolution = resolution,
                          grid = zone_xy)
}

RF_regression_smooth <- stack(species)

# =-=-=-=-=-=-=-=-=-=-= #
#     Classification
# =-=-=-=-=-=-=-=-=-=-= #
sp <- colnames(st_drop_geometry(bioticClass))
species <- vector('list', length(sp))
names(species) <- sp

for(i in sp) {
  cat(i, '\r')
  species[[i]] <- lissage(df = bioticClass,
                          field = i,
                          bandwidth = bandwidth,
                          resolution = resolution,
                          grid = zone_xy)
}

RF_classification_smooth <- stack(species)

values(RF_classification_smooth) <- ceiling(values(RF_classification_smooth))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save(RF_regression_smooth, file = './BioticDistribution/RandomForest/Data/RandForest_Regression_Smooth.RData')
save(RF_classification_smooth, file = './BioticDistribution/RandomForest/Data/RandForest_Classification_Smooth.RData')

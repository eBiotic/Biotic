# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script evaluates the distribution of species from the plurispecific
# survey data.

# Steps:
#    1. Environmental data
#    2. Biotic data
#    3. Intersect biotic with environmental
#    4. Remove species below minimum record number (50)
#    5. Format data for HMSC analysis
#    6. Monte Carlo Markoc Chain sampling
#    7. Producing MCMC trace and density plots
#    8. Producing posterior summaries
#    9. Variance partitioning
#   10. Variance partitioning for individual parameters (species diagnotics)
#   11. Computing the explanatory power of the model
#   12. Generating predictions for validation data


# Libraries
library(magrittr)
library(sf)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. LOAD ENVIRONMENTAL DATA & STUDY GRID
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./Environment/Environment.RData')
load('./eDriversGrids/Data/HexaGrid-1000m2.RData')
env <- dplyr::left_join(egslGrid, env, by = 'ID')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. LOAD BIOTIC DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')
biotic <- pluri
rm(pluri)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. INTERSECT BIOTIC W/ ENVIRONMENTAL DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Polygon values to points
biotic <- st_join(biotic, env, join = st_intersects)

# Remove duplicates, if any
uid <- duplicated(biotic)
biotic <- biotic[!uid, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. REMOVE SPECIES BELOW MINIMUM RECORD NUMBER
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Minimum record number accepted (subjective)
minRec <- 50

# Species list and count
sp <- biotic[,,drop = T] %>%
      group_by(species) %>%
      summarize(Count = n()) %>%
      filter(Count >= minRec) %>%
      select(species) %>%
      mutate(ID = paste0('sp',1:n()))

# Number of species
nSp <- nrow(sp)

# Select species with count > minimal record number
uid <- biotic$species %in% sp$species
biotic <- biotic[uid, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. FORMAT DATA FOR HMSC ANALYSIS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------
# Long to wide
# ------------
# The data has to be formatted so that lines are trawl sessions and columns are species
# Creating wide version of dataset to have stations as rows and species captured as columns
# Only for presence absence
# Other fields could also be used for count or weight
biotic <- st_drop_geometry(biotic) %>%
          tidyr::spread(species, presence, fill = 0)

# ---------------------------------
# Y: sample units by species matrix
# ---------------------------------
# Extracting only presence/absence data
# This corresponds to the Y matrix for the HMSC package
# A unique ID for each station is used as rownames
# The biotic data has, in theory, already a column name called `surveyID`
Y <- biotic[, sp$species]
# colnames(Y) <- sp[,'ID']

# Make sure all columns are numeric
Y <- apply(Y, 2, as.numeric)

# Add unique ID as column name
Station <- biotic$surveyID
rownames(Y) <- Station
# rownames(Y) <- paste0('site', 1:length(Station))


# -----------------------------------------
# Pi: sample units by random effects matrix
# -----------------------------------------
# Create a dataframe for random effects, columns have to be factors
# Using survey number, which correspond to years, as a random effect in the analysis
# Ultimately, there is likely a correlation between stations done during a single year in a single strata
# It would be a good thing to analyze spatial dependence between stations
# Survey number
Releve <- stringr::str_split(Station, '-') %>%
          unlist() %>%
          .[seq(1,length(.), by = 2)]

# Pi data
Pi <- data.frame(sampling_unit = Station,
                 survey_number = Releve,
                 stringsAsFactors = F)


# ----------------------------------------------------
# X: sampling units by environmental covariates matrix
# ----------------------------------------------------
# Create a matrix for the values of environmental covariates at each sampling unit location
# Environmental variables
envCov <- c('Bathy_Mean','SSAL_MEAN','SalMoyMoy','sst-2017-08','sbt',#'cil-2017',
            'sat','y','x')

# Groups of environmental covariables, for variance partitioning
envGroup <- c('Intercept','Bathymetry','Salinity','Salinity',#'Temperature',
              'Temperature','Temperature','Oxygen', 'Spatial', 'Spatial')

# The values have to be numeric
X <- biotic[, envCov]

# Make sure all columns are numeric
X <- apply(X, 2, as.numeric)

# Add unique ID as column name
rownames(X) <- Station
# rownames(X) <- paste0('site', 1:length(Station))
# colnames(X) <- paste0('env', 1:ncol(X))

# ----------------------------------------
# Remove any values with environmental NAs
# ----------------------------------------
# Identify NA values
uid <- apply(X, 2, is.na) %>%
       apply(., 1, any)

# Subset datasets
Y <- Y[!uid, ]
X <- X[!uid, ]
Pi <- Pi[!uid, ]

# Other subset
# uid <- Pi$survey_number %in% c('8','9','10','11','12')
# Y <- Y[uid, ]
# X <- X[uid, ]
# Pi <- Pi[uid, ]

# Transform Pi data as factors (for HMSC analysis)
Pi <- apply(Pi, 2, as.factor) %>%
      as.data.frame()


# ----------------------------
# as.HMSCdata for HMSC package
# ----------------------------
# Creating HMSC dataset for analyses
# biotic <- HMSC::as.HMSCdata(Y = Y, X = X, Random = Pi, interceptX = T, scaleX = T)
biotic <- HMSC::as.HMSCdata(Y = Y, X = X, Random = Pi, interceptX = TRUE, scaleX = TRUE)

# Export
save(biotic, file = 'BioticDistribution/Combine_RelevePluriSp_MPO/Data/dataHMSC.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. MONTE CARLO MARKOV CHAIN SAMPLING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sampling of posterior distribution
model <- HMSC::hmsc(biotic,
                    family = "probit",
                    niter = 100000, # 100000,
                    nburn = 1000, # 1000,
                    thin = 10) # 100)

# save model
save(model, file = './BioticDistribution/Combine_RelevePluriSp_MPO/Data/modelHMSC.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script evaluates the distribution of species from the plurispecific
# survey data.

# Steps:
#    1. Environmental data
#    2. Biotic data
#    3. Intersect biotic with environmental
#    4. Select & format environmental data
#    5. Remove species below minimum record number (50)
#    6. Format data for HMSC analysis
#    7. Monte Carlo Markoc Chain sampling


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

# Remove x,y column, use that of biotic data instead
env <- env[, !colnames(env) %in% c('X','Y')]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. LOAD BIOTIC DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')
biotic <- pluri
rm(pluri)

# Rename xy column
biotic <- biotic %>%
          rename(X = longitude_st,
                 Y = latitude_st)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. INTERSECT BIOTIC W/ ENVIRONMENTAL DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Polygon values to points
biotic <- st_join(biotic, env, join = st_intersects)

# Remove duplicates, if any
uid <- duplicated(biotic)
biotic <- biotic[!uid, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Select & format environmental data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------
# Sea surface temperatures
# ------------------------
# The northern survey is conducted in August
# The southern survey is conducted in September
# Empty vectors
biotic$sst <- NA

# Northern survey
uid <- biotic$releve == 'North'
np <- paste('sst', c('2013','2014','2015','2016','2017'), '08', sep = '-')
biotic$sst[uid] <- rowMeans(biotic[uid, np, drop = T], na.rm = T)

# Southern survey
uid <- !uid
sp <- paste('sst', c('2013','2014','2015','2016','2017'), '09', sep = '-')
biotic$sst[uid] <- rowMeans(biotic[uid, sp, drop = T], na.rm = T)

# -----------------------
# Sea bottom temperatures
# -----------------------
# This layer is annual, so I only need the mean value
uid <- paste('sbt', c('2013','2014','2015'), sep = '-')
biotic$sbt <- rowMeans(biotic[, uid, drop = T], na.rm = T)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. REMOVE SPECIES BELOW MINIMUM RECORD NUMBER
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
# 6. FORMAT DATA FOR HMSC ANALYSIS
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
# Y <- biotic[, sp$species[!sp$species %in% uid]]
Y <- biotic[, sp$species]
# colnames(Y) <- sp[,'ID']

# Make sure all columns are numeric
Y <- apply(Y, 2, as.numeric)

# Add unique ID as column name
Station <- biotic$surveyID
rownames(Y) <- Station


# -----------------------------------------
# Pi: sample units by random effects matrix
# -----------------------------------------
# Create a dataframe for random effects, columns have to be factors
# Using survey number, which correspond to years, as a random effect in the analysis
# Ultimately, there is likely a correlation between stations done during a single year in a single strata
# It would be a good thing to analyze spatial dependence between stations
Pi <- data.frame(sampling_unit = Station,
                 survey = biotic$releve)


# ----------------------------------------------------
# X: sampling units by environmental covariates matrix
# ----------------------------------------------------
# Create a matrix for the values of environmental covariates at each sampling unit location
# Environmental variables
envCov <- c('Bathy_Mean',
            'SSAL_MEAN','SalMoyMoy',
            'sst','sbt',
            'ARAG','Present.Surface.pH.tif',
            'sat','Present.Surface.Dissolved.oxygen.Mean',
            'Present.Benthic.Mean.Depth.Primary.productivity.Mean','Present.Surface.Primary.productivity.Mean',
            'Y','X')

# The values have to be numeric
X <- biotic[, envCov]

# Make sure all columns are numeric
X <- apply(X, 2, as.numeric)

# Add unique ID as column name
rownames(X) <- Station


# ----------------------------------------
# Remove any values with environmental NAs
# ----------------------------------------
# Identify NA values
uid <- apply(X, 2, is.na) %>%
       apply(., 1, any)

# Subset datasets
Y <- Y[!uid, ]
X <- X[!uid, ]
Pi <- droplevels(Pi[!uid, ])

# ----------------------------
# as.HMSCdata for HMSC package
# ----------------------------
# Creating HMSC dataset for analyses
biotic <- HMSC::as.HMSCdata(Y = Y, X = X, Random = Pi, interceptX = TRUE, scaleX = TRUE)

# Export
save(biotic, file = 'BioticDistribution/Combine_RelevePluriSp_MPO/Data/dataHMSC.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. MONTE CARLO MARKOV CHAIN SAMPLING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sampling of posterior distribution
model <- HMSC::hmsc(biotic,
                    family = "probit",
                    niter = 100000, # 100000,
                    nburn = 1000, # 1000,
                    thin = 100) # 100)

# save model
save(model, file = './BioticDistribution/Combine_RelevePluriSp_MPO/Data/modelHMSC.RData')

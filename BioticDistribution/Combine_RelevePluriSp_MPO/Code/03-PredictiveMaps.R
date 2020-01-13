# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces predictive maps from the HMSC model

# Steps:
#    1. HMSC data & model
#    2. Environmental data
#    3. New HMSC data for predictions
#    4. Generate predictions



# Libraries
library(magrittr)
library(tidyverse)
library(HMSC)
library(sf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. HMSC data & model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticDistribution/Combine_RelevePluriSp_MPO/Data/dataHMSC.RData')
load('./BioticDistribution/Combine_RelevePluriSp_MPO/Data/modelHMSC.RData')

# Parameters
sp <- colnames(biotic$Y)
nSp <- length(sp)
envCov <- colnames(biotic$X)[-1]
nParam <- length(envCov)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Environmental data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./Environment/Environment.RData')

# ------------------------
# Sea surface temperatures
# ------------------------
# The northern survey is conducted in August
# The southern survey is conducted in September
# Empty vectors
env$sst <- NA

# Northern survey
uid <- env$Region == 'North'
np <- paste('sst', c('2013','2014','2015','2016','2017'), '08', sep = '-')
env$sst[uid] <- rowMeans(env[uid, np, drop = T], na.rm = T)

# Southern survey
uid <- !uid
sp <- paste('sst', c('2013','2014','2015','2016','2017'), '09', sep = '-')
env$sst[uid] <- rowMeans(env[uid, sp, drop = T], na.rm = T)


# -----------------------
# Sea bottom temperatures
# -----------------------
# This layer is annual, so I only need the mean value
uid <- paste('sbt', c('2013','2014','2015'), sep = '-')
env$sbt <- rowMeans(env[, uid, drop = T], na.rm = T)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. New HMSC data for predictions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ----------------------------------------------------
# X: sampling units by environmental covariates matrix
# ----------------------------------------------------
X <- env[, envCov]

# -----------------------------------------
# Pi: sample units by random effects matrix
# -----------------------------------------
# Create a dataframe for random effects, columns have to be factors
Pi <- data.frame(sampling_unit = env$ID,
                 survey = "1")


# ----------------------------------------
# Remove any values with environmental NAs
# ----------------------------------------
# Identify NA values
uid <- apply(X, 2, is.na) %>%
       apply(., 1, any)

# Subset datasets
X <- X[!uid, ]
Pi <- droplevels(Pi[!uid, ])


# ----------------------------
# as.HMSCdata for HMSC package
# ----------------------------
# Creating HMSC dataset for analyses
# Embed in function so that I can use it iteratively
dataPred <- function(X, Pi, range) {
  # Subset
  X <- X[range[1]:range[2], ]
  Pi <- droplevels(Pi[range[1]:range[2], ])

  # Data
  return(as.HMSCdata(X = X, Random = Pi, scaleX = T, interceptX = T))
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Generate predictions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This process requires a lot of memory.
# To facilitate the predictions, I divide them into iteration of 10000 grids and
# then I put them back together.

# Range of iterations
nP <- nrow(X)
r1 <- seq(1, nP, by = 1000)
r2 <- c(seq(1000, nP, by = 1000), nP)
uid <- cbind(r1,r2)

# List to store results
pred <- vector('list', nrow(uid))

# Predictions
for(i in 1:nrow(uid)) {
  cat(i, ' of ', nrow(uid), '\r')
  # New data
  dat <- dataPred(X, Pi, uid[i, ])

  # Predictions
  pred[[i]] <- predict(model, newdata = dat)
}


# Single object
pred <- do.call(rbind, pred)

# Data.frame
pred <- as.data.frame(pred)

# Add colnames
colnames(pred) <- colnames(biotic$Y)

# Add ID
pred$ID <- as.character(Pi$sampling_unit)

# Join with grid
distPluri <- data.frame(ID = env$ID, stringsAsFactors = F) %>%
             left_join(., pred, by = 'ID')

save(distPluri, file = './BioticDistribution/Combine_RelevePluriSp_MPO/Data/DistributionPlurispecific.RData')

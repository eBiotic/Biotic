# Libraries
library(sf)
library(sp)
library(tidyverse)
library(magrittr)
library(raster)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticDistribution/GLM/Data/envRasters.RData')
load('./BioticDistribution/GLM/Data/speciesRasters.RData')

env[['HAB_C_E']] <- as.factor(env[['HAB_C_E']])
env[['Megahabita']] <- as.factor(env[['Megahabita']])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data formatting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data.frames
spDat <- values(species) %>% as.data.frame(stringsAsFactors = F)
envDat <- values(env) %>% as.data.frame(stringsAsFactors = F)

# Change variables to factor
envDat[, 'HAB_C_E'] <- as.factor(envDat[, 'HAB_C_E'])
envDat[, 'Megahabita'] <- as.factor(envDat[, 'Megahabita'])

# Scale numeric environmental data
uid <- unlist(lapply(envDat, is.numeric))
envDat[,uid] <- scale(envDat[,uid])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Environmental variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Environmental variables
envCov <- colnames(envDat)

# Variables for which we want quadratic terms
envQ <- c('Bathy_Mean','Y','X')
envQ <- c('Bathy_Mean')

# Other variables
envO <- envCov[!envCov %in% envQ]
rem <- c('HAB_C_E','Megahabita','X','Y')
envM <- envO[!envO %in% rem]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run for all species
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nSp <- ncol(spDat)
R2 <- spDist <- spPred <- vector('list', nSp)

for(i in 1:nSp){
  cat(i, ' of ', nSp, '\r')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Formula
f <- as.formula(paste0('spDat[,', i, '] ~ ',
                paste(envQ, paste0('I(', envQ, '^2)'), sep = ' + ', collapse = ' + '),
                ' + ',
                paste(envM, collapse = ' + ')))
# f <- as.formula(paste0('spDat[,', i, '] ~ ', paste(envCov[!envCov %in% c('HAB_C_E','Megahabita')], collapse = ' + ')))

# Model
spDist[[i]] <- glm(f, data = envDat,
                   family = binomial(link = "logit"),
                   control=glm.control(maxit=100))

# summary(spDist)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# McFadden's R^2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Null model
fN <- as.formula(paste0('spDat[,', i, '] ~ 1'))
nullmod <- glm(fN, family="binomial")

# R^2
# 1 - (Log-likelihood(model) / Log-likelihood(null model))
llMod <- logLik(spDist[[i]])
llNull <-  logLik(nullmod)
R2[[i]] <- as.numeric(1-(llMod / llNull))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. PREDICTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Remove unused levels
# modLevel <- spDist[[i]]$xlevels
# varName <- names(modLevel)
# datLevel <- lapply(envDat[, varName], levels)
# diffLevel <- list(which(!datLevel[[1]] %in% modLevel[[1]]),
#                   which(!datLevel[[2]] %in% modLevel[[2]]))
# names(diffLevel) <- names(datLevel)

# Subset data
newDat <- envDat

# # Megahabita
# uid <- newDat$Megahabita %in% diffLevel$Megahabita
# newDat$Megahabita[uid] <- NA
#
# # HAB_C_E
# uid <- newDat$HAB_C_E %in% diffLevel$HAB_C_E
# newDat$HAB_C_E[uid] <- NA
#
# Prediction
pred <- predict(spDist[[i]], newdata = newDat, type = "response")

# Build raster
spPred[[i]] <- raster(env) %>%
               setValues(pred)

# Plot
# plot(spPred[[i]], zlim = c(0,1))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# END PREDICTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load species list
load('BioticData/SpeciesList/Data/SpeciesList.RData')

# Minimum record number accepted (subjective)
minRec <- 30

# Filter
sp <- sp %>%
      filter(Count >= minRec)

# Prediction stack
spPred <- stack(spPred)
names(spPred) <- sp$species
# plot(spPred, zlim = c(0,1))


# McFadden's R2
R2 <- sp %>%
      mutate(McFaddenR2 = unlist(R2))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Binaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
th <- 0.5
rec <- matrix(c(th, 1, 1, 0, th, 0), ncol = 3, byrow = T)
bin <- reclassify(spPred, rec, right = F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Richness
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rich <- calc(bin, sum, na.rm = T)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save(spDist, file = './BioticDistribution/GLM/Data/modelGLM.RData')
save(spPred, file = './BioticDistribution/GLM/Data/predictionGLM.RData')
save(R2, file = './BioticDistribution/GLM/Data/R2GLM.RData')
save(bin, file = './BioticDistribution/GLM/Data/binariesGLM.RData')
save(rich, file = './BioticDistribution/GLM/Data/richnessGLM.RData')

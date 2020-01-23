# Libraries
library(sf)
library(tidyverse)
library(magrittr)
library(raster)
library(randomForest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Environment
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
load('./Environment/Data/EnvironmentRasters.RData')

# Select environmental variables
envCov <- c('Bathy_Mean',
            'SSAL_MEAN','SalMoyMoy',
            'sst','sbt',
            'ARAG','Present.Surface.pH.tif',
            'sat','Present.Surface.Dissolved.oxygen.Mean',
            'Present.Benthic.Mean.Depth.Primary.productivity.Mean','Present.Surface.Primary.productivity.Mean')

# Environmental data
env <- env[[envCov]]

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Occurrences
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
load('./BioticData/SpeciesOccurrence/Data/SpeciesRastersPAbs.RData')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#         Species names
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
sp <- names(species)
nSp <- length(sp)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data.frames
spDat <- values(species) %>% as.data.frame(stringsAsFactors = F)
envDat <- values(env) %>% as.data.frame(stringsAsFactors = F)

# Scale numeric environmental data
uid <- unlist(lapply(envDat, is.numeric))
envDat[,uid] <- scale(envDat[,uid])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random forest - regression
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          	 Model
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
spDist <- vector('list', nSp)
names(spDist) <- sp

for(i in 1:nSp) {
	cat(i, ' of ', nSp, '\r')
	suppressWarnings({
		spDist[[i]] <- randomForest(formula = spDat[,i] ~ .,
																data = envDat,
																importance = TRUE,
																keep.forest = TRUE,
																na.action = na.omit,
																ntree = 500,
																nodesize = 5)
	})
}

# Variation explained by model
varExpl <- lapply(spDist, function (x) last(x$rsq)) %>%
					 unlist() %>%
					 data.frame(species = names(.), rsq = ., stringsAsFactors = F, row.names = NULL)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Predictions
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
spPred <- vector('list', nSp)

for(i in 1:nSp) {
	cat(i, ' of ', nSp, '\r')
	spPred[[i]] <- predict(spDist[[i]], envDat) %>%
								 raster(crs = projection(env),
							 					resolution = res(env),
												ext = extent(env),
												vals = .)
}

# Create raster stack
spPred <- stack(spPred)
names(spPred) <- sp

# Round values
values(spPred) <- round(values(spPred), 6)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          	 Export
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
RF_regression <- spPred
Var_regression <- varExpl
save(RF_regression, file = './BioticDistribution/RandomForest/Data/RandForest_Regression.RData')
save(Var_regression, file = './BioticDistribution/RandomForest/Data/RandForest_Regression_Variance.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random forest - classification
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          	 Model
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
spDist <- vector('list', nSp)
names(spDist) <- sp

for(i in 1:nSp) {
	cat(i, ' of ', nSp, '\r')
	suppressWarnings({
		spDist[[i]] <- randomForest(formula = as.factor(spDat[,i]) ~ .,
																data = envDat,
																importance = TRUE,
																keep.forest = TRUE,
																na.action = na.omit,
																ntree = 500,
																nodesize = 5)
	})
}

# Confusion matrices
confMat <- lapply(spDist, function(x) x$confusion)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          Predictions
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
spPred <- vector('list', nSp)

for(i in 1:nSp) {
	cat(i, ' of ', nSp, '\r')
	spPred[[i]] <- predict(spDist[[i]], envDat) %>%
								 raster(crs = projection(env),
							 					resolution = res(env),
												ext = extent(env),
												vals = .)
}

# Create raster stack
spPred <- stack(spPred)
names(spPred) <- sp

# Round values
values(spPred) <- values(spPred) - 1

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#          	 Export
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
RF_classification <- spPred
ConfMat_classification <- confMat
save(RF_classification, file = './BioticDistribution/RandomForest/Data/RandForest_Classification.RData')
save(ConfMat_classification, file = './BioticDistribution/RandomForest/Data/RandForest_Classification_ConfMat.RData')

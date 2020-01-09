# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script runs diagnostics on the HMSC model

# Steps:
#    1. HMSC data & model
#    2. Producing MCMC trace and density plots
#    3. Producing posterior summaries
#    4. Variance partitioning
#    5. Computing the explanatory power of the model
#    6. Cross-valitdation

# source('BioticDistribution/Combine_RelevePluriSp_MPO/Code/01-PluriDistribution.R')

# Libraries
# library(magrittr)
# library(tidyverse)
library(HMSC)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. HMSC DATA & MODEL
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticDistribution/Combine_RelevePluriSp_MPO/Data/dataHMSC.RData')
load('./BioticDistribution/Combine_RelevePluriSp_MPO/Data/modelHMSC.RData')

# Parameters
sp <- colnames(biotic$Y)
nSp <- length(sp)
envCov <- colnames(biotic$X)
nParam <- length(envCov)

# Groups of environmental covariables, for variance partitioning
# WARNING: Not reproducible
envGroup <- c('Intercept','Bathymetry','Salinity','Salinity','Temperature',
              'Temperature','Temperature', 'Oxygen', 'Spatial', 'Spatial')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. PRODUCING THE MCMC TRACE AND DENSITY PLOTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mixing objects
mixingParamX <- coda::as.mcmc(model, parameters = "paramX")
mixingMeansParamX <- coda::as.mcmc(model, parameters = "meansParamX")
mixingMeansVarX <- coda::as.mcmc(model, parameters = "varX")
mixingParamLatent <- coda::as.mcmc(model, parameters = "paramLatent")

# Save meanParamX for trace and density plots
# saveRDS(mixingMeansParamX, file = './RData/mixingMeansParamX.rds')

# Trace and density plots to visually diagnose mcmc chains
# Another way to check for convergence is to use diagnostic tests such as Geweke's convergence diagnostic (geweke.diag function in coda) and the Gelman and Rubin's convergence diagnostic (gelman.diag function in coda).
paramModel <- colnames(mixingMeansParamX)
nParam <- length(paramModel)

# Plot
# jpeg(paste(fig,'MCMCTracePlot.jpeg',sep=''), width = 6, height = (1.5*nParam), res = 150, units = 'in')
par(mfrow = c(nParam, 2), mar = rep(2, 4))
for(i in 1:ncol(mixingMeansParamX)) {
  coda::traceplot(mixingMeansParamX[,i], col = "blue", main = paste('Trace of ', paramModel[i]))
  coda::densplot(mixingMeansParamX[,i], col = "orange", main = paste('Density of ', paramModel[i]))
}
# dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. PRODUCING POSTERIOR SUMMARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Average
average <- apply(model$results$estimation$paramX, 1:2, mean)

# 95% confidence intervals
CI.025 <- apply(model$results$estimation$paramX, 1:2, quantile, probs = 0.025)
CI.975 <- apply(model$results$estimation$paramX, 1:2, quantile, probs = 0.975)

# Plot
par(mfrow = c(ncol(average), 1))
for(i in 1:ncol(average)) {
  # Parameters
  dat <- c(average[,i], CI.025[,i], CI.975[,i])
  yR <- c(min(dat), max(dat))
  cols <- (CI.025[,i] < 0 & CI.975[,i] < 0) | (CI.025[,i] > 0 & CI.975[,i] > 0)
  cols <- ifelse(cols, '#486639','#8a3636')

  # Parameter value
  plot(average[,i], pch = 15, ylim = yR, main = colnames(average)[i], col = cols)
  abline(h = 0)

  # Credible intervals
  arrows(x0 = 1:nrow(average), x1 = 1:nrow(average), y0 = CI.025[,i], y1 = CI.975[,i], code = 3, angle = 90, length = 0.05, col = cols)
}
# mtext(text = sp[, 'species'], side = 1, line = 1, outer = FALSE, at = 1:nSp, col = 1, las = 2, cex = 0.4)
# dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. VARIANCE PARTITIONING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for parameter names: colnames(mixingMeansParamX)
nGroup <- length(unique(envGroup)) + 2
variationPart <- HMSC::variPart(model, envGroup)
# saveRDS(variationPart, file = './RData/variPart.rds')

# Colors
Colour <- rainbow(n = nGroup, s = 1, v = 1, start = 0, end = max(1, nGroup - 1)/nGroup, alpha = 1)

# jpeg(paste(fig,'variancePartitioning.jpeg',sep=''), width = 6, height = 4, res = 150, units = 'in')
par(mfrow = c(1,1), mar = c(6,3,1,1))
barplot(t(variationPart), col=Colour, names.arg = sp, las = 2, cex.names = 0.4, cex.axis = 0.6)

# Create legend elements
legendVector <- character(nGroup)
variPartLabel <- c(unique(envGroup), 'Random site', 'Random plot')

for(i in 1:nGroup) {
  legendVector[i] <- paste(variPartLabel[i], ' (mean = ',
                           round(mean(variationPart[, i]), 4)*100, "%)",
                           sep="")
}

legend('bottomleft', legend = legendVector, fill = Colour, bg = 'white', cex = 0.5)
# dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. COMPUTING THE EXPLANATORY POWER OF THE MODEL
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prevalence
prevSp <- colSums(biotic$Y)

# Coefficient of multiple determination
R2 <- HMSC::Rsquared(model, averageSp = FALSE)
R2comm <- HMSC::Rsquared(model, averageSp = TRUE)

# # Save R^2 calculation for individual summaries
# saveRDS(R2, file = './RData/modelR2.rds')
# saveRDS(R2comm, file = './RData/modelR2comm.rds')

# Draw figure
# jpeg(paste(fig,'r2summaries.jpeg',sep=''), width = 6, height = 5, res = 150, units = 'in')
par(mfrow = c(1,1))
plot(prevSp, R2, xlab = "Prevalence", ylab = expression(R^2), cex = 0.8, pch=19,
     las=1, cex.lab = 1, main = 'Explanatory power of the model')
abline(h = R2comm, col = "blue", lwd = 2)
# dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Cross-validation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('BioticDistribution/Functions/crossValid.R')
modelAUC <- crossValidation(data = biotic,  nCV = 20, validPct = 0.2)
save(modelAUC, file = 'BioticDistribution/Combine_RelevePluriSp_MPO/Data/modelAUC.RData')

meanAUC <- colMeans(modelAUC)
sdAUC <- apply(modelAUC, MARGIN = 2, FUN = sd)

# png('./figures/crossValidation.png', width = 6, height = 5, res = 150, units = 'in')
  plot(prevSp, meanAUC, ylim = c(0,1), xlab = "Prevalence", ylab = 'AUC', cex = 0.8, pch=19, las=1, cex.lab = 1, main = 'Monte Carlo cross-validation with AUC of ROC curves')
  abline(h = 0.5, col = 'grey')
  arrows(x0 = prevSp, x1 = prevSp, y0 = (meanAUC - sdAUC), y1 = (meanAUC + sdAUC), code = 3, angle = 90, length = 0.05)
  points(prevSp, meanAUC, pch = 19, cex = 0.8)
# dev.off()

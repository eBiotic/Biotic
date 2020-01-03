#' Intersects occurrence data with environmental data
#'
#' @export
#'
#' @param biotic sf object with species occurrences
#' @param environment sf object with environmental data
#'
#' @keywords biotic; environment
#'
#' @examples
#' # Example 1:
#' load('./Combine_RelevePluriSp_MPO/Data/CombinePluriSP.RData')
#' load('./Environment/environment.RData')
#' load('./eDriversGrids/Data/HexaGrid-1000m2.RData')
#' env <- dplyr::left_join(egslGrid, environment, by = 'ID')
#' intersectEnvironment(pluri, env)
#'
#' # Example 2:
#' plot0(c(0,1),c(0,1))
#' arrows2(runif(2), runif(2), x1=runif(2), y1=runif(2))
#' arrows2(runif(2), runif(2), x1=runif(2), y1=runif(2), prophead=FALSE, lty=3)



intersectEnvironment <- function(biotic, environment, envCov, envGroup, minRec = 50) {

  # ---
  library(magrittr)
  library(sf)
  library(tidyverse)
  load('./Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')
  load('./Environment/environment.RData')
  load('./eDriversGrids/Data/HexaGrid-1000m2.RData')
  env <- dplyr::left_join(egslGrid, environment, by = 'ID')
  minRec = 50
  biotic <- pluri
  environment <- env

  # To verify in the scripts of raw data directly...
  biotic <- biotic[!duplicated(biotic), ]
  biotic$species <- gsub(' ', '_', biotic$species)
  environment$Bathy_Mean <- abs(round(environment$Bathy_Mean,0))
  # ---

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ========================================
  # 1. Parameters
  # ========================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Environmental variables
  envCov <- c('Bathy_Mean','SSAL_MEAN','SalMoyMoy','TempMoyMoy','STEMMEAN',
              'BTEMMEAN','O2_Sat_Mea','y','x')

  # Groups of environmental covariables, for variance partitioning
  envGroup <- c('Intercept','Bathymetry','Salinity','Salinity','Temperature',
                'Temperature','Temperature','Oxygen', 'Spatial', 'Spatial')


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ========================================
  # 2. Intersect with environmental data
  # ========================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Note that the process will be much faster if the biotic data is already
  # in wide format, i.e. each row is a fishing event. However, for the sake
  # script clarity and because the analysis is not that long, I do it woth
  # long format table.
  # Intersection
  biotic <- st_intersection(environment, biotic)

  # Identify NAs
  NAs <- biotic[, envCov] %>%
         lapply(X = ., FUN = function(x) which(is.na(x))) %>%
         unlist(.) %>%
         unique(.)

  # Remove NAs if any
  if (length(NAs > 0)) biotic <- biotic[-NAs, ]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ========================================
  # 3. Remove species w/o minimum count
  # ========================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---------------------------------
  # Species list and count
  # ---------------------------------
  sp <- biotic[,,drop = T] %>%
        group_by(species) %>%
        summarize(Count = n()) %>%
        filter(Count >= minRec) %>%
        select(species) %>%
        mutate(ID = paste0('sp',1:n())) %>%
        as.matrix()

  # ---------------------------------
  # Number of species
  # ---------------------------------
  nSp <- length(sp)

  # ---------------------------------
  # Remove species from dataset
  # ---------------------------------
  uid <- biotic$species %in% sp
  biotic <- biotic[uid, ]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ========================================
  # 4. Format biotic data for HMSC analysis
  # ========================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---------------------------------
  # Y: sample units by species matrix
  # ---------------------------------
  # The data has to be formatted so that lines are trawl sessions and columns are species
  # Creating wide version of dataset to have stations as rows and species captured as columns
  # Only for presence absence
  # Other fields could also be used for count or weight

  # Long to wide
  biotic <- biotic %>%
            tidyr::spread(species, presence, fill = 0)
  # x <- biotic
  # biotic <- x
  # biotic <- biotic[1:200, ]

  # Extracting only presence/absence data
  # This corresponds to the Y matrix for the HMSC package
  # A unique ID for each station is used as rownames
  # The biotic data has, in theory, already a column name called `surveyID`
  Y <- biotic[, sp[,'species'], drop = T]
  colnames(Y) <- sp[,'ID']

  # Make sure all columns are numeric
  Y <- apply(Y, 2, as.numeric)

  # Add unique ID as column name
  Station <- biotic$surveyID
  rownames(Y) <- Station
  rownames(Y) <- paste0('site', 1:length(Station))



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
  Pi <- data.frame(sampling_unit = as.factor(Station),
                   survey_number = as.factor(Releve))


  # ----------------------------------------------------
  # X: sampling units by environmental covariates matrix
  # ----------------------------------------------------
  # Create a matrix for the values of environmental covariates at each sampling unit location
  # The values have to be numeric
  X <- biotic[, envCov, drop = T]

  # Make sure all columns are numeric
  X <- apply(X, 2, as.numeric)

  # Add unique ID as column name
  rownames(X) <- Station
  rownames(X) <- paste0('site', 1:length(Station))
  colnames(X) <- paste0('env', 1:ncol(X))


  # ----------------------------
  # as.HMSCdata for HMSC package
  # ----------------------------
  # Creating HMSC dataset for analyses
  # biotic <- HMSC::as.HMSCdata(Y = Y, X = X, Random = Pi, interceptX = T, scaleX = T)
  biotic <- HMSC::as.HMSCdata(Y = Y, X = X, Random = Pi, interceptX = TRUE, scaleX = TRUE)

  # Save file
  # saveRDS(biotic, file = './Data/biotic_HMSC.RData')

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ===============================
  # 5. Performing the MCMC sampling
  # ===============================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sampling of posterior distribution
  model <- HMSC::hmsc(biotic,
                      family = "probit",
                      niter = 20000, # 100000,
                      nburn = 1000, # 1000,
                      thin = 10) # 100)

  # save model
  # save(model, file = './Combine_RelevePluriSp_MPO/Distribution/ModelData/modelHSMC.RData')

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # =========================================
  # 5. Producing MCMC trace and density plots
  # =========================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ================================
  # 6. Producing posterior summaries
  # ================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Average
  average <- apply(model$results$estimation$paramX, 1:2, mean)

  # 95% confidence intervals
  CI.025 <- apply(model$results$estimation$paramX, 1:2, quantile, probs = 0.025)
  CI.975 <- apply(model$results$estimation$paramX, 1:2, quantile, probs = 0.975)

  # Summary table
  paramXCITable <- cbind(unlist(as.data.frame(average)),
                       unlist(as.data.frame(CI.025)),
                       unlist(as.data.frame(CI.975)))
  colnames(paramXCITable) <- c("average", "lowerCI", "upperCI")
  rownames(paramXCITable) <- paste(rep(colnames(average), each = nrow(average)), "_", rep(rownames(average), ncol(average)), sep="")

  # Save summary table
  # saveRDS(paramXCITable, file = './RData/modelPostSumm.rds')

  # Credible intervals
  paramXCITable_Full <- paramXCITable
  beg <- seq(1,nrow(paramXCITable_Full), by = nSp)
  end <- seq(nSp,nrow(paramXCITable_Full), by = nSp)
  sign <- abs(as.numeric(paramXCITable_Full[, 'lowerCI'] <= 0 & paramXCITable_Full[, 'upperCI'] >=0) - 3)

  # Export figure
  # jpeg(paste(fig,'credibleInterval.jpeg',sep=''), width = 6, height = (1.5*nParam), res = 150, units = 'in')
  par(mfrow = c((length(beg)+1),1))
  for(i in 1:length(beg)) {
      paramXCITable <- paramXCITable_Full[beg[i]:end[i], ]
      cols <- sign[beg[i]:end[i]]
      par(mar=c(1,2,1,1))
      plot(0, 0, xlim = c(1, nrow(paramXCITable)), ylim = round(range(paramXCITable)), type = "n", xlab = "", ylab = "", main=paste(colnames(mixingMeansParamX)[i]), xaxt="n", bty = 'n')
      axis(1,1:nSp,las=2, labels = rep('',nSp))
      abline(h = 0,col = 'grey')
      arrows(x0 = 1:nrow(paramXCITable), x1 = 1:nrow(paramXCITable), y0 = paramXCITable[, 2], y1 = paramXCITable[, 3], code = 3, angle = 90, length = 0.05, col = cols)
      points(1:nrow(paramXCITable), paramXCITable[,1], pch = 15, cex = 1, col = cols)
  }
  mtext(text = sp[, 'species'], side = 1, line = 1, outer = FALSE, at = 1:nSp, col = 1, las = 2, cex = 0.4)
  # dev.off()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # =========================
  # 7. Variance partitioning
  # =========================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
          legendVector[i] <- paste(variPartLabel[i], ' (mean = ', round(mean(variationPart[, i]), 4)*100, "%)", sep="")
      }

  legend('bottomleft', legend = legendVector, fill = Colour, bg = 'white', cex = 0.5)
  # dev.off()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ========================================================================
  # 8. Variance partitioning for individual parameters (species diagnotics)
  # ========================================================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Extract variance partitioning per parameters for individual taxa diagnostics
  nGroup <- length(envCov) + 2
  variationPart <- HMSC::variPart(model, c('Intercept',envCov))
  # saveRDS(variationPart, file = './RData/variPartInd.rds')


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ===============================================
  # 9. Computing the explanatory power of the model
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ===============================================
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
  plot(prevSp, R2, xlab = "Prevalence", ylab = expression(R^2), cex = 0.8, pch=19, las=1, cex.lab = 1, main = 'Explanatory power of the model')
  abline(h = R2comm, col = "blue", lwd = 2)
  # dev.off()




}

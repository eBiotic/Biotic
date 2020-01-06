# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(sp)
library(magrittr)
library(tidyverse)
library(reshape2)
library(stringr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 CUSTOM FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('./BioticData/NordGolfe_RelevePluriSp_MPO/code/dmsTOdd.R')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize acidification comes from DFO
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './BioticData/NordGolfe_RelevePluriSp_MPO/Data/RawData'

# Will have to upload the data on zenodo and eventually get the data from SLGO.
# For now, I'm using the data downloaded manually from the website.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
np <- read.table(paste0(output, '/te8-12.txt'), sep = ';', header = T, dec = '.')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transforming ddmm.%% to degree decimals
# See dmsTOdd() for more information
np$LaDeTow <- unlist(lapply(X = np$LaDeTow, FUN = dmsTOdd))
np$LoDeTow <- unlist(lapply(X = np$LoDeTow, FUN = dmsTOdd, type = 'long'))
np$LaFiTow <- unlist(lapply(X = np$LaFiTow, FUN = dmsTOdd))
np$LoFiTow <- unlist(lapply(X = np$LoFiTow, FUN = dmsTOdd, type = 'long'))

# Transforming data columns in correct type
np$DatDeTow <- as.Date(np$DatDeTow)
np$DatFiTow <- as.Date(np$DatFiTow)
np$Prof_1 <- as.numeric(paste(np$Prof_1)) # coerces NAs in place of '. ', which is what I want
np$Prof_2 <- as.numeric(paste(np$Prof_2)) # coerces NAs in place of '. ', which is what I want
np$SNb_Capt <- as.numeric(paste(np$SNb_Capt)) # coerces NAs in place of '.', which is what I want
np$N_EspSci <- as.character(np$N_EspSci)
np$N_EspF <- as.character(np$N_EspF)
np$HreDeb <- as.character(np$HreDeb)
np$HreFin <- as.character(np$HreFin)

# Trim scientific names of species
np$N_EspSci <- str_trim(np$N_EspSci, side = 'both')

# Remove duplicated rows
np <- distinct(np)

# Combine duplicated species within a single station
# Targetted stations:
#     'No_Rel' = 12, 'No_Stn' = 20, 'EspGen' = '351', duplicated species, differing 'WCapOri'
#     'No_Rel' = 12, 'No_Stn' = 21, 'EspGen' = '351', duplicated species, differing 'WCapOri'
#     'No_Rel' = 12, 'No_Stn' = 50, 'EspGen' = '2035', duplicated species, differing 'WCapOri'
#     'No_Rel' = 12, 'No_Stn' = 57, 'EspGen' = '2035', duplicated species, differing 'WCapOri'
# Solution: sum 'WCapOri' for duplicated values
np %>% .[which(.[,'No_Rel'] == 12 & .[,'No_Stn'] == 20 & .[, 'EspGen'] == '351'), c('No_Rel','No_Stn','EspGen','N_EspSci','WCapOri')]
np %>% .[which(.[,'No_Rel'] == 12 & .[,'No_Stn'] == 21 & .[, 'EspGen'] == '351'), c('No_Rel','No_Stn','EspGen','N_EspSci','WCapOri')]
np %>% .[which(.[,'No_Rel'] == 12 & .[,'No_Stn'] == 50 & .[, 'EspGen'] == '2035'), c('No_Rel','No_Stn','EspGen','N_EspSci','WCapOri')]
np %>% .[which(.[,'No_Rel'] == 12 & .[,'No_Stn'] == 57 & .[, 'EspGen'] == '2035'), c('No_Rel','No_Stn','EspGen','N_EspSci','WCapOri')]

# This isn't efficient, but I can't figure it out with aggregate() on full dataset and I don't have time to lose on 4 data points
mod <- data.frame(No_Rel = c(12,12,12,12), No_Stn = c(20,21,50,57), EspGen = c('351','351','2035','2035'))
for(i in 1:nrow(mod)) {
  aggData <- np %>% .[which(.[,'No_Rel'] == mod[i,1] & .[,'No_Stn'] == mod[i,2] & .[, 'EspGen'] == mod[i,3]), ]
  names <- rownames(aggData)
  aggData <- aggData %>% aggregate(WCapOri ~ ., data = ., FUN = sum)
  nc <- ncol(aggData)
  aggData <- aggData[,c(1:(nc-2),nc,nc-1)]

  np[names[1], ] <- aggData
  np <- np[rownames(np) != names[2], ]
}

# Fill in blanks for dates
# missDates <- is.na(np[, 'DatFiTow'])
deb <- as.POSIXct(paste(np[, 'DatDeTow'], np[, 'HreDeb']))
fin <- as.POSIXct(paste(np[, 'DatDeTow'], np[, 'HreFin']))

# If hour beg > end, then date changed during trawl activity
condCheck <- deb < fin
np[,'DatFiTow'] <- as.Date(rep('1900-01-01', (nrow(np))))

np[condCheck, 'DatFiTow'] <- np[condCheck, 'DatDeTow']
np[!condCheck, 'DatFiTow'] <- np[!condCheck, 'DatDeTow'] + 1

if(nrow(np[np[,'DatFiTow'] == '1900-01-01', ]) > 0) {
    stop('Dates should all be resolved, check for potential errors')
}

# Aggregate data
np <- aggregate(cbind(WCapOri, SNb_Capt) ~ No_Rel + No_Stn + EspGen + N_EspSci + N_EspF + Source + Nbpc + DatDeTow + DatFiTow + EngGen + Resul + HreDeb + HreFin + LaDeTow + LoDeTow + LaFiTow + LoFiTow + Prof_1 + Prof_2, data = np, FUN = sum, na.action = na.pass)

# Order dataset by year, station and species number
np <- np[order(np[, 'No_Rel'], np[, 'No_Stn'], np[, 'EspGen']), ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 COMBINE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species name list & number of records per year
# Count number of observations per species
countRec <- numeric(nrow(np))
npNames <- aggregate(countRec ~ EspGen + N_EspSci + N_EspF, data = cbind(np, countRec), FUN = length) %>%
           .[order(.[, 'EspGen']), ]

# Which taxa should be combined?
# All information in './SpeciesFormatting/Code/combineTaxa.R'
load('./BioticData/SpeciesFormatting/Data/combineTaxa.RData')

# Transform as a list
combineTaxa <- str_split(combineTaxa, ' \\| ')

# Duplicate taxa names in northPluri to match taxon name chosen for aggregation
for(i in 1:length(combineTaxa)) {
  # Number of taxa
  nTaxa <- length(combineTaxa[[i]])

  # Identify records for taxon used for combination
  taxonCombRec <- np[, 'N_EspSci'] == combineTaxa[[i]][1]

  # if taxon exist in list, replace taxa name that will be aggregated with taxonComb
  if(sum(taxonCombRec) > 0) {
    # Extract variables for taxon used for combination
    taxonComb <- np[taxonCombRec, ][1, c('EspGen','N_EspSci','N_EspF')]

    # Identify which taxa have to be combined with taxonComb
    taxaReplace <- np[, 'N_EspSci'] %in% combineTaxa[[i]][2:nTaxa]

    # Replace names of taxa to combine with taxonComb
    np[taxaReplace, c('EspGen','N_EspSci','N_EspF')] <- taxonComb
  } else {
    # Create new EspGen for taxon and create vector of variables used to describe taxon used for combination
    # !!!!! There may already be an existing number. Need to figure this out at some point !!!!!!
    newEspGen <- max(as.numeric(paste(np[,'EspGen']))) + 1
    taxonComb <- data.frame(EspGen = newEspGen, N_EspSci = combineTaxa[[i]][1], N_EspF = combineTaxa[[i]][1], stringsAsFactors = FALSE)

    # Identify which taxa have to be combined with taxonComb
    taxaReplace <- np[, 'N_EspSci'] %in% combineTaxa[[i]][2:nTaxa]

    # Replace names of taxa to combine with taxonComb
    np[taxaReplace, c('EspGen','N_EspSci','N_EspF')] <- taxonComb
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
load('./BioticData/SpeciesFormatting/Data/removeTaxa.RData')

# npNames[!npNames[, 'N_EspSci'] %in% removeTaxa, ] # To visualize data removed
# Remove taxa from northPluri
np <- np[!np[, 'N_EspSci'] %in% removeTaxa, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    UPDATE SPECIES LIST & NUMBER OF RECORDS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count number of observations per species
countRec <- numeric(nrow(np))
npNames <- aggregate(countRec ~ EspGen + N_EspSci, data = cbind(np, countRec), FUN = length) %>%
           .[order(.[, 'N_EspSci']), ]




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add mid-way point between beginning and end of trawling activities
np$LoMid <- rowMeans(np[, c('LoDeTow','LoFiTow')])
np$LaMid <- rowMeans(np[, c('LaDeTow','LaFiTow')])

# Point shapefile
np <- st_as_sf(np, coords = c('LoMid','LaMid'), crs = 4326) %>%
      st_transform(crs = 32198)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(np, file = './BioticData/NordGolfe_RelevePluriSp_MPO/Data/Biotic/NorthPluri.RData')
save(npNames, file = './BioticData/NordGolfe_RelevePluriSp_MPO/Data/Biotic/NorthPluriSP.RData')

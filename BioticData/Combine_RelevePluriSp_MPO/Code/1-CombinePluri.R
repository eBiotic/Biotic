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
#                                     SOURCING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source('./NordGolfe_RelevePluriSp_MPO/Code/1-NorthPluri.R')
# source('./SudGolfe_RelevePluriSp_MPO/Code/1-SouthPluri.R')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize acidification comes from DFO
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './BioticData/Combine_RelevePluriSp_MPO/Data/RawData'

# Will have to upload the data on zenodo and eventually get the data from SLGO.
# For now, I'm using the data downloaded manually from the website.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./BioticData/SudGolfe_RelevePluriSp_MPO/Data/Biotic/SouthPluri.RData')
load('./BioticData/SudGolfe_RelevePluriSp_MPO/Data/Biotic/SouthPluriSP.RData')
load('./BioticData/NordGolfe_RelevePluriSp_MPO/Data/Biotic/NorthPluri.RData')
load('./BioticData/NordGolfe_RelevePluriSp_MPO/Data/Biotic/NorthPluriSP.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             COMBINED SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spList <- data.frame(species = unique(c(spNames$N_EspSci, npNames$N_EspSci)),
                     stringsAsFactors = F) %>%
          left_join(npNames, by = c('species' = 'N_EspSci')) %>%
          rename(npID = EspGen, npRec = countRec) %>%
          left_join(spNames, by = c('species' = 'N_EspSci')) %>%
          rename(spID = EspGen,
                 spRec = countRec) %>%
          mutate(combRec = rowSums(.[, c('npRec','spRec')], na.rm = T)) %>%
          mutate(speciesID = 1:nrow(.)) %>%
          arrange(species) %>%
          mutate(check = is.na(npRec) | is.na(spRec))

# spList[, c(1,3,5,8)]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      COMBINED OCCURRENCES (PRESENCE ONLY)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format south pluri data
sp <- sp %>%
      rename(species = N_EspSci,
             presence = Presence) %>%
      mutate(surveyID = paste(cruise, set, sep = '-'),
             year = as.character(year),
             month = as.character(month),
             day = as.character(day),
             month = gsub('\\b9\\b', '09',month)) %>%
      select(-vessel, -cruise, -set, -strat, -time, -duration, -EspGen)

# Format north pluri data
np <- np %>%
      mutate(year = format(DatDeTow,"%Y"),
             month = format(DatDeTow,"%m"),
             day = format(DatDeTow,"%d")) %>%
      mutate(surveyID = paste(No_Rel, No_Stn, sep = '-'),
             presence = 1) %>%
      select(-No_Rel, -No_Stn, -EspGen, -N_EspF, -Source, -Nbpc, -DatFiTow,
             -EngGen, -Resul, -HreDeb, -HreFin, -Prof_1, -Prof_2, -WCapOri,
             -DatDeTow, -SNb_Capt) %>%
      rename(species = N_EspSci,
             latitude_st = LaDeTow,
             latitude_end = LaFiTow,
             longitude_st = LoDeTow,
             longitude_end = LoFiTow) %>%
      .[, colnames(sp)]

# Combined dataset
pluri <- rbind(np, sp)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(pluri, file = './BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluri.RData')
save(spList, file = './BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluriSP.RData')

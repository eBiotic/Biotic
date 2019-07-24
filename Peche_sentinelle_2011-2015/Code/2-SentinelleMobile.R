# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize comes from DFO and cannot be shared
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './Peche_sentinelle_2011-2015/Data/RawData'

# Data will need to be archived to Zenodo with restricted access and downloaded
# using an access token.
# Eventually it would ideally be part of the SLGO web portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip kmz file
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)


# Import data
sentMobQC <- read.table(file = paste0(output, '/PSM_QC_42-50.txt'),
                        header = TRUE, sep = ";", dec = '.',
                        stringsAsFactors = F)

sentMobTN <- read.table(file = paste0(output, '/PSM_TN_41-50.txt', sep = ''),
                        header = TRUE, sep = ";", dec = '.',
                        stringsAsFactors = F)


# -----------------------------------------------------------------------------
# DATA: Sentinel fisheries for mobile gear 2011-2015
#
# INFORMATION:
#
# /*************  Structure du fichier de données pour les pêches sentinelles avec engins mobiles ********************************************************/
#     Source ";"    /* Source de données, Alfred Needler=6, Lady Hammond=8, Teleost=16*/
#     No_Rel ";"    /* Numéro du relevé */
#     DatDeTow ";"  /* Date de début de trait Format aaaa-mm-jj */
#     DatFiTow ";"  /* Date de fin de trait Format aaaa-mm-jj */
#     NoStn ";"     /* Numéro de la station */
#     EngGen ";"    /* Code d'engin de pêche utilisé */
#     Resul ";"     /* Code résultat opération */
#     EspGen ";"    /* Code d'espèce */
#     N_EspSci ";"  /* Nom scientifique de l'espèce*/
#     N_EspF ";"    /* Nom français de l'espèce */
#     HreDeb ";"    /* Heure début du trait, Format HH:MM:SS */
#     HreFin ";"    /* Heure fin de trait, Format HH:MM:SS */
#     LaDeTow ";"   /* Latitude positionnement début du trait, unité=ddmm.%% */
#     LoDeTow ";"   /* Longitude positionnement début du trait, unité=ddmm.%% */
#     LaFiTow ";"   /* Latitude positionnement fin du trait, unité=ddmm.%% */
#     LoFiTow ";"   /* Latitude début du trait, unité=dd.%% */
#     Prof_1 ";"    /* Profondeur début du trait, unité=m */
#     Prof_2 ";"    /* Profondeur fin de tait, unité=m */
#     WCapOri ";"   /* Poids de la capture, unité=kg */
#     SNb_Capt;     /* Nombre individus capturés */
# # -----------------------------------------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Transforming data columns in correct type
sentMobTN$LaFiTow <- as.numeric(paste(sentMobTN$LaFiTow))
sentMobTN$LoFiTow <- as.numeric(paste(sentMobTN$LoFiTow))
sentMobTN$WCapOri <- as.numeric(paste(sentMobTN$WCapOri))
sentMobTN$DatDeTow <- as.Date(sentMobTN$DatDeTow)
sentMobQC$NoStn <- as.numeric(paste(sentMobQC$NoStn))
sentMobQC$WCapOri <- as.numeric(paste(sentMobQC$WCapOri))
sentMobQC$DatDeTow <- as.Date(sentMobQC$DatDeTo)

# Transforming ddmm.%% to degree decimals
# See dmsTOdd() for more information
source('./Peche_sentinelle_2011-2015/Code/dmsTOdd.R')
sentMobQC[, "LaDeTow"] <- unlist(lapply(X = sentMobQC[, "LaDeTow"], FUN = dmsTOdd))
sentMobQC[, "LoDeTow"] <- unlist(lapply(X = sentMobQC[, "LoDeTow"], FUN = dmsTOdd, type = 'long'))
sentMobQC[, "LaFiTow"] <- unlist(lapply(X = sentMobQC[, "LaFiTow"], FUN = dmsTOdd))
sentMobQC[, "LoFiTow"] <- unlist(lapply(X = sentMobQC[, "LoFiTow"], FUN = dmsTOdd, type = 'long'))
sentMobTN[, "LaDeTow"] <- unlist(lapply(X = sentMobTN[, "LaDeTow"], FUN = dmsTOdd))
sentMobTN[, "LoDeTow"] <- unlist(lapply(X = sentMobTN[, "LoDeTow"], FUN = dmsTOdd, type = 'long'))
sentMobTN[, "LaFiTow"] <- unlist(lapply(X = sentMobTN[, "LaFiTow"], FUN = dmsTOdd))
sentMobTN[, "LoFiTow"] <- unlist(lapply(X = sentMobTN[, "LoFiTow"], FUN = dmsTOdd, type = 'long'))

# Single data frame for Mobile gear, TN & QC
sentMob <- rbind(sentMobQC, sentMobTN)

# Change species names
sentMob$N_EspSci <- str_trim(sentMob$N_EspSci, 'both')
sentMob$N_EspSci <- gsub('Pennatula borealis', 'Pennatula grandis', sentMob$N_EspSci)

# Remove empty species
sentMob <- sentMob[!sentMob$N_EspSci == '', ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
# source('./SpeciesFormatting/Code/removeTaxa.R')
load('./SpeciesFormatting/Data/removeTaxa.RData')
sentMob <- sentMob[!sentMob$N_EspSci %in% removeTaxa, ] # Remove from dataset


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 COMBINE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Which taxa should be combined?
# All information in './SpeciesFormatting/Code/combineTaxa.R'
load('./SpeciesFormatting/Data/combineTaxa.RData')

# Transform as list
combineTaxa <- str_split(combineTaxa, ' \\| ')
# for(i in 1:length(combineTaxa)) print(c(paste(combineTaxa[[i]][1]), paste(any(combineTaxa[[i]] %in% obsSp$species))))

# Duplicate taxa names in northPluri to match taxon name chosen for aggregation
for(i in 1:length(combineTaxa)) {
  for(j in 2:length(combineTaxa[[i]])) {
    id0 <- sentMob$N_EspSci == combineTaxa[[i]][j]
    sentMob$N_EspSci[id0] <- combineTaxa[[i]][1]
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species list
sentMobSp <- table(sentMob$N_EspSci) %>%
             as.data.frame(stringsAsFactors = F) %>%
             rename(species = Var1,
                    CountRec = Freq)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add mid-way point between beginning and end of mobile fishing activities
sentMob$longitude <- rowMeans(sentMob[, c('LoDeTow','LoFiTow')])
sentMob$latitude <- rowMeans(sentMob[, c('LaDeTow','LaFiTow')])

# Remove NAs
idNA <- is.na(sentMob$longitude)
sentMob <- sentMob[!idNA, ]

# Point shapefile
sentMob <- st_as_sf(sentMob, coords = c('longitude','latitude'), crs = 4326, remove = F) %>%
           st_transform(crs = 32198)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sentMob <- sentMob %>%
           mutate(year = format(DatDeTow,"%Y"),
                  month = format(DatDeTow,"%m"),
                  day = format(DatDeTow,"%d"),
                  surveyID = paste(No_Rel, NoStn, sep = '-'),
                  presence = 1) %>%
           rename(latitude_st = LaDeTow,
                  latitude_end = LaFiTow,
                  longitude_st = LoDeTow,
                  longitude_end = LoFiTow,
                  species = N_EspSci,
                  gear = EngGen,
                  biomass = WCapOri) %>%
           select(-Source, -No_Rel, -DatDeTow, -DatFiTow, -EspGen, -N_EspF,
                  -SNb_Capt, -Prof_1, -Prof_2, -HreFin, -HreDeb, -Resul,
                  -NoStn) %>%
           filter(presence > 0)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               SELECT OBSERVATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose points located in the St. Lawrence only
# Some are obvious mistakes (inland fishing events)
# Load EGSL outline
load('./eDriversGrids/Data/egslSimple.RData')

# Intersect fisheries events with EGSL
sentMob <- sentMob %>%
           st_intersects(egslSimple, .) %>%
           unlist() %>%
           sentMob[., ]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(sentMob, file = './Peche_sentinelle_2011-2015/Data/Biotic/SentinelleMobile.RData')
save(sentMobSp, file = './Peche_sentinelle_2011-2015/Data/Biotic/SentinelleMobileSP.RData')

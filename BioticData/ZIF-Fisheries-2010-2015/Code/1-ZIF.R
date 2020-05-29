# source('./BioticData/ZIF-Fisheries-2010-2015/Code/1-ZIF.R')
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
output <- './BioticData/ZIF-Fisheries-2010-2015/Data/RawData'

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

# Import data !(could be done in a single line of command)
zif0 <- read.csv(file = paste0(output, '/2010_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif1 <- read.csv(file = paste0(output, '/2011_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif2 <- read.csv(file = paste0(output, '/2012_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif3 <- read.csv(file = paste0(output, '/2013_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif4 <- read.csv(file = paste0(output, '/2014_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif5 <- read.csv(file = paste0(output, '/2015_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)

# Add year column !(could be done in a single line of command)
zif0$year <- 2010
zif1$year <- 2011
zif2$year <- 2012
zif3$year <- 2013
zif4$year <- 2014
zif5$year <- 2015

# Merge dasets
# Check that all column names are the same between datasets
# cNames <- data.frame(colnames(zif0), colnames(zif1), colnames(zif2), colnames(zif3), colnames(zif4), colnames(zif5))
# all(apply(cNames, 1, function(x) length(unique(x)) == 1))
zif <- rbind(zif0, zif1, zif2, zif3, zif4, zif5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------ #
# Change names #
# ------------ #
# Gear names
  zif[, 'Engin'] <- gsub('Plong\x8ee', 'Plongee', zif[, 'Engin'])
  zif[, 'Engin'] <- gsub('Senne danoise ou \x8ecossaise', 'Senne Danoise ou Ecossaise', zif[, 'Engin'])
  zif[, 'Engin'] <- gsub('P\x90che \x88 la ligne', 'Peche a la ligne', zif[, 'Engin'])


# Species list & adjusting species names in dataset
# Note that I only adjust names for scientific names
spList <- data.frame(Common = sort(unique(as.character(zif$Sp_capture))),
                     Scientific = sort(unique(as.character(zif$Sp_capture))),
                     stringsAsFactors = F)

spList$Scientific <- gsub("Oursin",'Strongylocentrotus sp.', spList$Scientific)
spList$Scientific <- gsub("Crabe des neiges",'Chionoecetes opilio', spList$Scientific)
spList$Scientific <- gsub("P\x8etoncle d'Islande",'Chlamys islandica', spList$Scientific)
spList$Scientific <- gsub("P\x8etoncle",'Chlamys islandica', spList$Scientific)
spList$Scientific <- gsub("Mactre atlantique",'Spisula solidissima', spList$Scientific)
spList$Scientific <- gsub("Crevette",'Pandalus borealis', spList$Scientific)
spList$Scientific <- gsub("Buccin",'Buccinum sp.', spList$Scientific)
spList$Scientific <- gsub("Fl\x8etan du Groenland",'Reinhardtius hippoglossoides', spList$Scientific)
spList$Scientific <- gsub("Fl\x8etan atlantique",'Hippoglossus hippoglossus', spList$Scientific)
spList$Scientific <- gsub("Morue franche",'Gadus morhua', spList$Scientific)
spList$Scientific <- gsub("Plie rouge",'Pseudopleuronectes americanus', spList$Scientific)
spList$Scientific <- gsub("Hareng",'Clupea harengus', spList$Scientific)
spList$Scientific <- gsub("Mactre de Stimpson",'Mactromeris polynyma', spList$Scientific)
spList$Scientific <- gsub("Plie canadienne",'Hippoglossoides platessoides', spList$Scientific)
spList$Scientific <- gsub("Concombre de mer",'Cucumaria frondosa', spList$Scientific)
spList$Scientific <- gsub("Limande \x88 queue jaune",'Limanda ferruginea', spList$Scientific)
spList$Scientific <- gsub("Turbot de sable",'Scophthalmus aquosus', spList$Scientific)
spList$Scientific <- gsub("S\x8ebaste",'Sebastes sp.', spList$Scientific)
spList$Scientific <- gsub("Merluche blanche",'Urophycis tenuis', spList$Scientific)
spList$Scientific <- gsub("Aiglefin",'Melanogrammus aeglefinus', spList$Scientific)
spList$Scientific <- gsub("Couteau de l'Atlantique",'Ensis leei', spList$Scientific)
spList$Scientific <- gsub("Chaboisseaux",'Cottidae', spList$Scientific)
spList$Scientific <- gsub("Goberge",'Pollachius virens', spList$Scientific)
spList$Scientific <- gsub("Maquereau",'Scomber scombrus', spList$Scientific)
spList$Scientific <- gsub("Homard",'Homarus americanus', spList$Scientific)
spList$Scientific <- gsub("Raie",'Rajidae', spList$Scientific)
spList$Scientific <- gsub("Lompe",'Cyclopterus lumpus', spList$Scientific)
spList$Scientific <- gsub("Plie grise",'Glyptocephalus cynoglossus', spList$Scientific)
spList$Scientific <- gsub("Baudroie",'Lophius americanus', spList$Scientific)
spList$Scientific <- gsub("Gaspereau",'Alosa pseudoharengus', spList$Scientific)
spList$Scientific <- gsub("Capelan",'Mallotus villosus', spList$Scientific)
spList$Scientific <- gsub("Esturgeon",'Acipenser oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Mara\x94che",'Lamna nasus', spList$Scientific)
spList$Scientific <- gsub("Requins",'Selachii', spList$Scientific)
spList$Scientific <- gsub("Crabe commun",'Cancer irroratus', spList$Scientific)
spList$Scientific <- gsub("Brosme",'Brosme brosme', spList$Scientific)
spList$Scientific <- gsub("Requin bleu",'Prionace glauca', spList$Scientific)
spList$Scientific <- gsub("Loup atlantique",'Anarhichas lupus', spList$Scientific)
spList$Scientific <- gsub("Loup de mer",'Anarhichadidae', spList$Scientific)
spList$Scientific <- gsub("Crabe araign\x8ee",'Hyas araneus', spList$Scientific)
spList$Scientific <- gsub("Poulamon",'Microgadus tomcod', spList$Scientific)
spList$Scientific <- gsub("Thon rouge",'Thunnus thynnus', spList$Scientific)
spList$Scientific <- gsub("Morue de roche",'Gadus ogac', spList$Scientific)
spList$Scientific <- gsub("Clovisse arctique",'Mesodesma sp.', spList$Scientific)
spList$Scientific <- gsub("Mako \x88 nageoires courtes",'Isurus oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Gaspareau",'Alosa pseudoharengus', spList$Scientific)
spList$Scientific <- gsub("Loquette d'Am\x8erique",'Zoarces americanus', spList$Scientific)
spList$Scientific <- gsub("Mako \x88 courtes nageoires",'Isurus oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Anguille",'Anguilla rostrata', spList$Scientific)
spList$Scientific <- gsub("Aiguillat commun",'Squalus acanthias', spList$Scientific)
spList$Scientific <- gsub("Merluche \x8ecureuil",'Urophycis chuss', spList$Scientific)
spList$Scientific <- gsub("Argentine",'Argentina silus', spList$Scientific)
spList$Scientific <- gsub("Thon ventru",'Thunnus obesus', spList$Scientific)
spList$Scientific <- gsub("Germon",'Thunnus alalunga', spList$Scientific)
spList$Scientific <- gsub("Espadon",'Xiphias gladius', spList$Scientific)

spList <- spList[order(spList$Common, decreasing = T), ]

# Adjust in dataset
for(i in 1:nrow(spList)) zif$Sp_capture <-  gsub(spList[i,1], spList[i,2], zif$Sp_capture)
for(i in 1:nrow(spList)) zif$Sp_vise <-  gsub(spList[i,1], spList[i,2], zif$Sp_vise)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
# source('./SpeciesFormatting/Code/removeTaxa.R')
load('./BioticData/SpeciesFormatting/Data/removeTaxa.RData')
# zifSp <- zifSp[!zifSp$species %in% removeTaxa, ] # Remove from species list
zif <- zif[!zif$Sp_capture %in% removeTaxa, ] # Remove from dataset


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  SPECIES LIST
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of records
zifSp <- table(zif$Sp_capture) %>%
         as.data.frame(stringsAsFactors = F) %>%
         rename(species = Var1,
                CountRec = Freq)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  Transform Coordinates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('./BioticData/ZIF-Fisheries-2010-2015/Code/dmsTOdd.R')
zif[, "Latitude"] <- unlist(lapply(X = zif[, "Latitude"], FUN = dmsTOdd))
zif[, "Longitude"] <- unlist(lapply(X = zif[, "Longitude"], FUN = dmsTOdd, type = 'long'))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zif <- zif %>%
       mutate(month = format(as.Date(Date),"%m"),
              day = format(as.Date(Date),"%d"),
              presence = 1,
              surveyID = paste0('zif-', group_indices(., Latitude, Longitude))) %>%
       rename(gear = Engin,
              species = Sp_capture,
              longitude = Longitude,
              latitude = Latitude) %>%
       select(-Date, -Sp_vise, -Pds_capture_kg)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial object and transform projection
zif <- st_as_sf(x = zif, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
       st_transform(crs = 32198)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               SELECT OBSERVATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose points located in the St. Lawrence only
# Some are obvious mistakes (inland fishing events)
# Load EGSL outline
load('./eDriversGrids/Data/egslSimple.RData')

# Intersect fisheries events with EGSL
zif <- zif %>%
       st_intersects(egslSimple, .) %>%
       unlist() %>%
       zif[., ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(zif, file = './BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zif.RData')
save(zifSp, file = './BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zifSp.RData')

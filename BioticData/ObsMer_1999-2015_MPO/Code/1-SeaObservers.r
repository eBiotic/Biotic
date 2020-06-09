# source('./BioticData/ObsMer_1999-2015_MPO/Code/1-SeaObservers.r')
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
source('./BioticData/ObsMer_1999-2015_MPO/code/dmsTOdd.R')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize acidification comes from DFO
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './BioticData/ObsMer_1999-2015_MPO/Data/RawData'

# Will have to upload the data on zenodo and eventually get the data from SLGO.
# For now, I'm using the data downloaded manually from the website.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data
obs <- read.csv(file = paste0(output, '/Table_capturesOBS_19992015_data.csv'),
                header = TRUE, sep = ",", dec = '.')

# Gear types
obsGear <- read.csv(file = paste0(output, '/Table_capturesOBS_19992015_engins.csv'),
           header = TRUE, sep = ",", dec = '.')

# Species
obsSp <- read.csv(file = paste0(output, '/Table_capturesOBS_19992015_especes.csv'),
         header = TRUE, sep = ",", dec = '.')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modify species names
# obsSp$NOM_SCIENT_ESP <- gsub(' ', '_', obsSp$NOM_SCIENT_ESP)
# obsSp$NOM_SCIENT_ESP <- gsub('\\.','', obsSp$NOM_SCIENT_ESP)

# Remove data prior to 2010
obs <- filter(obs, An > 2009)

# Species that are not in species list (for simplicity's sake, remove them for now)
obs <- obs[obs$Sp_capture %in% obsSp$COD_OBS, ]

# Transform coordinates in degrees dedimal
obs[, "Latitude_deb"] <- unlist(lapply(X = obs[, "Latitude_deb"], FUN = dmsTOdd))
obs[, "Longitude_deb"] <- unlist(lapply(X = obs[, "Longitude_deb"], FUN = dmsTOdd, type = 'long'))
obs[, "Latitude_fin"] <- unlist(lapply(X = obs[, "Latitude_fin"], FUN = dmsTOdd))
obs[, "Longitude_fin"] <- unlist(lapply(X = obs[, "Longitude_fin"], FUN = dmsTOdd, type = 'long'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               NUMBER OF RECORDS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count number of observations per species
obsSp <- table(obs$Sp_capture) %>%
         as.data.frame(stringsAsFactors = F) %>%
         rename(species = Var1,
                CountRec = Freq) %>%
         mutate(species = as.numeric(species)) %>%
         left_join(obsSp, ., by = c('COD_OBS' = 'species')) %>%
         na.omit() %>%
         select(-NOM_COMMUN_ESP_F) %>%
         rename(species = NOM_SCIENT_ESP,
                obsID = COD_OBS) %>%
         arrange(species) %>%
         mutate(species = as.character(paste(species)))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             FORMAT SPECIES NAMES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Correct mistakes
obsSp$species <- tolower(obsSp$species) # lower letters
obsSp$species <- paste0(obsSp$species, '  ') # add two spaces at the end
obsSp$species <- gsub('sp  ','sp.', obsSp$species) # add point at the end of genus
obsSp$species <- gsub('ascidia sp. adult','Ascidia sp.', obsSp$species) # add point at the end of genus
obsSp$species <- gsub('triglops  ','triglops sp.', obsSp$species) # add point at the end of genus
obsSp <- obsSp[-grep('egg', obsSp$species), ] # Remove eggs
obsSp$species <- str_trim(obsSp$species, side = 'both') # Remove spaces
obsSp$species <- paste0(toupper(substring(obsSp$species, 1, 1)), substring(obsSp$species, 2, nchar(obsSp$species))) # First letter capital

# Remove irrelevant entries from species list and dataset
  # ID
  rmEntries <- c("Stones and rocks", # Not relevant
                 "Poisson non-identifie", # Not relevant
                 "Shark (ns)") # Group is too coarse
  rmEntries <- obsSp$obsID[obsSp$species %in% rmEntries]

  # Remove
  obsSp <- obsSp[!obsSp$obsID %in% rmEntries, ]
  obs <- obs[!obs$Sp_capture %in% rmEntries, ]

# Taxonomic names
# All species checked on WoRMS for taxonomic names using taxize
# library(taxize)
# sp <- obsSp$species[is.na(obsSp$check)]
# sp <- gsub(' sp.', '', sp)
# uids <- tnrs(sp)
obsSp$species <- gsub('Aluterus schoepfi','Aluterus schoepfii', obsSp$species)
obsSp$species <- gsub('Bathophilus metallicus','Bathophilus vaillanti', obsSp$species)
obsSp$species <- gsub('Gonostoma elongatum','Sigmops elongatus', obsSp$species)
obsSp$species <- gsub('Hyperiidae','Hyperiidea', obsSp$species)
obsSp$species <- gsub('Solaster sp.','Crossaster sp.', obsSp$species)
obsSp$species <- gsub('Cryptodonta; pterimorphia','Cryptodonta', obsSp$species)
obsSp$species <- gsub('Loliginidae; ommastrephidae','Loliginidae', obsSp$species)
obsSp$species <- gsub('Notoscopelus elongatus kroyeri','Notoscopelus kroyeri', obsSp$species)
obsSp$species <- gsub('Clinocardium ciliatum','Clinocardium ciliatum', obsSp$species)
obsSp$species <- gsub('Liparis liparis','Liparis liparis liparis', obsSp$species)
obsSp$species <- gsub('Omosudis lowei','Omosudis lowii', obsSp$species)
obsSp$species <- gsub('Paralepis atlantica','Magnisudis atlantica', obsSp$species)
obsSp$species <- gsub('Paralichthys oblongus','Hippoglossina oblonga', obsSp$species)
obsSp$species <- gsub('Pennatula borealis','Pennatula grandis', obsSp$species)
obsSp$species <- gsub('Polinices heros','Euspira heros', obsSp$species)
obsSp$species <- gsub('Staurophora mertensii','Staurostoma mertensii', obsSp$species)
obsSp$species <- gsub('Tealia felina','Urticina felina', obsSp$species)
obsSp$species <- gsub('Tetrapturus albidus','Kajikia albida', obsSp$species)
obsSp$species <- gsub('Ulcina olrikii','Aspidophoroides olrikii', obsSp$species)
obsSp$species <- gsub('Gasterosteus aculeatus','Gasterosteus aculeatus aculeatus', obsSp$species)
obsSp$species <- gsub('Urophycis chesteri','Phycis chesteri', obsSp$species)
obsSp$species <- gsub('Sergestes arcticus','Eusergestes arcticus', obsSp$species)

# Add species names to obs dataset
obs <- left_join(obs, obsSp[, 1:2], by = c('Sp_visee' = 'obsID')) %>%
       select(-Sp_visee) %>%
       rename(Sp_visee = species) %>%
       left_join(., obsSp[, 1:2], by = c('Sp_capture' = 'obsID')) %>%
       select(-Sp_capture) %>%
       rename(Sp_capture = species)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
load('./BioticData/SpeciesFormatting/Data/removeTaxa.RData')
obs <- obs[!obs$Sp_capture %in% removeTaxa, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 COMBINE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Which taxa should be combined?
# All information in './SpeciesFormatting/Code/combineTaxa.R'
load('./BioticData/SpeciesFormatting/Data/combineTaxa.RData')

# Transform as list
combineTaxa <- str_split(combineTaxa, ' \\| ')
# for(i in 1:length(combineTaxa)) print(c(paste(combineTaxa[[i]][1]), paste(any(combineTaxa[[i]] %in% obsSp$species))))

# Duplicate taxa names in northPluri to match taxon name chosen for aggregation
for(i in 1:length(combineTaxa)) {
  for(j in 2:length(combineTaxa[[i]])) {
    id0 <- obs$Sp_capture == combineTaxa[[i]][j]
    obs$Sp_capture[id0] <- combineTaxa[[i]][1]
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                              UPDATED OBSERVATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update species list
obsSp <- table(obs$Sp_capture) %>%
         as.data.frame(stringsAsFactors = F) %>%
         rename(species = Var1,
                CountRec = Freq) %>%
         arrange(species)

# Select only observations for species in species list
obs <- obs[obs$Sp_capture %in% obsSp$species, ]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add mid-way point between beginning and end of mobile fishing activities
idNA <- !is.na(obs$Longitude_fin)
obs$longitude[idNA] <- rowMeans(obs[idNA, c('Longitude_deb','Longitude_fin')])
obs$latitude[idNA] <- rowMeans(obs[idNA, c('Latitude_deb','Latitude_fin')])

# Static fisheries
obs$longitude[!idNA] <- obs$Longitude_deb[!idNA]
obs$latitude[!idNA] <- obs$Latitude_deb[!idNA]

# Point shapefile
obs <- st_as_sf(obs, coords = c('longitude','latitude'), crs = 4326) %>%
       st_transform(crs = 32198)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
obs <- obs %>%
       rename(year = An,
              month = Mois,
              day = Jour,
              latitude_st = Latitude_deb,
              latitude_end = Latitude_fin,
              longitude_st = Longitude_deb,
              longitude_end = Longitude_fin,
              species = Sp_capture,
              gear = Engin) %>%
        mutate(presence = as.numeric(obs$Pds_capt_kg > 0),
               surveyID = paste(No_voy, No_affec, No_sortie, No_trait, sep = '-')) %>%
        select(-No_voy, -No_affec, -No_sortie, -No_trait, -Div_OPANO, -Prof_m,
               -Duree_act_h, -Pds_capt_kg, -Sp_visee) %>%
        filter(presence > 0)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               SELECT OBSERVATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose points located in the St. Lawrence only
# Some are obvious mistakes (inland fishing events)
# Load EGSL outline
load('./eDriversGrids/Data/egslSimple.RData')

# Intersect fisheries events with EGSL
obs <- obs %>%
       st_intersects(egslSimple, .) %>%
       unlist() %>%
       obs[., ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(obs, file = './BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserver.RData')
save(obsSp, file = './BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserverSP.RData')

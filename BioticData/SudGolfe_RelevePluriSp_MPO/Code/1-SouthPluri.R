# source('./BioticData/SudGolfe_RelevePluriSp_MPO/Code/1-SouthPluri.R')
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
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize acidification comes from DFO
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './BioticData/SudGolfe_RelevePluriSp_MPO/Data/RawData'

# Will have to upload the data on zenodo and eventually get the data from SLGO.
# For now, I'm using the data downloaded manually from the website.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sp <- read.csv(file = paste0(output, '/DBeauchesne_MAR2016.csv'), stringsAsFactor = F) # data file
spCodes <- read.csv(file = paste0(output, '/Gulf_Maritimes_SpCode.csv'), stringsAsFactor = F) # species code list


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Change column names
colnames(sp) <- gsub('X_', '', colnames(sp))

# Remove errors from database (see mail with S. Hurtubise)
errors <- c('963','4348','8332','8364','8365')
sp <- sp[, !colnames(sp) %in% errors]

# Change sp number in dataset (see mail with S. Hurtubise)
sp[, '6000'] <- rowSums(sp[, c('6000','6118')])
sp <- sp[, -which(colnames(sp) == '6118')]

# Add missing entries in spCodes (see mail with S. Hurbutise)
species <- c(2804,2808,3701,4220,6114,8327,8343,8346,8351,8352,8358,8361,8366,8611,8612,8613,8614,8616,8617,8620,8623,8624)
latin_name <- c("Neohela monstrosa","Maera loveni","Golfingia margaritacea","","Leptasterias sp.","Capnella sp.","Nemertea","Pseudoarchaster parelii","Stephanoauge nexilis","Syscenus infelix","Eusirus cuspidatus","Anthoptilum grandiflorum","Phakelia sp.","","Weberella bursa","Suberites ficus","","Mycale lingua","","","Halichondia panicea","")
name <- c("","","","pétoncle non identifié","","","","","","","","","sponge","sponge","NIPPLE sponge","FIG sponge","sponge","BED sponge","sponge","sponge","BREAD sponge","sponge")
addSpCodes <- data.frame(species = species, latin_name = latin_name, name = name)
spCodes <- rbind(spCodes, addSpCodes)

# Remove species for which we have no data
noName <- spCodes$species[which(spCodes$latin_name == "")]
spCodes <- spCodes[!spCodes$species %in% noName,]
sp <- sp[, !colnames(sp) %in%  noName]

# Modify species codes
spCodes$name <- gsub(';', '; ', spCodes$name)
spCodes$name <- tolower(spCodes$name)
spCodes$latin_name <- tolower(spCodes$latin_name)
spCodes <- spCodes[-grep('eggs', spCodes$latin_name), ] # Remove eggs
spCodes <- spCodes[-grep('unid\\.', spCodes$latin_name), ] # Remove unidentified
spCodes <- spCodes[-grep('\\(ns\\)', spCodes$latin_name), ] #
spCodes$latin_name <- gsub(' o\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' c\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' p\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' f\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' s\\.f\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' s\\.c\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' s\\.p\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' s\\.o\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' so\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' s\\.', '', spCodes$latin_name)
spCodes$latin_name <- gsub(' - obsolete', '', spCodes$latin_name)
spCodes$latin_name <- gsub('lithodes/neolithodes', 'Lithodes sp\\.', spCodes$latin_name)
spCodes$latin_name <- paste0(toupper(substring(spCodes$latin_name, 1, 1)), substring(spCodes$latin_name, 2, nchar(spCodes$latin_name)))

# Remove species that are no longer in spCodes
id0 <- colnames(sp) %in% spCodes$species
id0[1:13] <- TRUE
sp <- sp[, id0]

# Species list
spNames <- colnames(sp) %>%
           as.numeric() %>%
           .[!is.na(.)] %>%
           match(spCodes$species, .) %>%
           is.na()

# Only species in sp dataset
spNames <- spCodes[!spNames, ]

# Change column names
spNames <- spNames %>%
            rename(EspGen = species) %>%
            rename(N_EspSci = latin_name) %>%
            rename(N_EspF = name)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           REMOVE/CHANGE TAXA (INITIAL)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove
spRm <- c('Ctenophora;coelenterata;porifera', 'Prionodesmata;teleodesmata',
          'Parasites;round worms', 'Polychaeta;large')
id0 <- spNames$N_EspSci %in% spRm
sp <- sp[, !colnames(sp) %in% spNames$EspGen[id0]]
spNames <- spNames[!id0, ]

# Adjust name
spNames$N_EspSci <- gsub('Gorgonocephalidae;asteronychidae', 'Gorgonocephalus sp.', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Actinaria', 'Actiniaria', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Aphrodita hastata', 'Aphroditella hastata', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Aphrodita sp\\.', 'Aphroditella sp\\.', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Astrotecten duplicatus', 'Astropecten duplicatus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Careproctus reinhardi', 'Careproctus reinhardti', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Chlamys islandicus', 'Chlamys islandica', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Gymnelis viridis', 'Gymnelus viridis', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Halichondia panicea', 'Halichondria panicea', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Lumpenus lumpretaeformis', 'Lumpenus lampretaeformis', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Lumpenus medius', 'Anisarchus medius', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Lunatia heros', 'Euspira heros', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Lycenchelys verrilli', 'Lycenchelys verrillii', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Lycodes terraenova', 'Lycodes terraenovae', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Margarites groenlandica', 'Margarites groenlandicus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Myoxocephalus aeneus', 'Myoxocephalus aenaeus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Notolepis rissoi kroyeri', 'Arctozenus risso kroyeri', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Ophiura sarsi', 'Ophiura sarsii', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Pennatula borealis', 'Pennatula grandis', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Pseudoarchaster parelii', 'Pseudarchaster parelii', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Pycnogonum littorale', 'Pycnogonum litorale', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Scomberesox saurus', 'Scomberesox saurus saurus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Sepiolodae', 'Sepioloidea sp.', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Solaster papposus', 'Crossaster papposus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Solaster sp.', 'Crossaster sp.', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Spisula polynyma', 'Mactromeris polynyma', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Stephanoauge nexilis', 'Stephanauge nexilis', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Stichaeus punctatus', 'Stichaeus punctatus punctatus', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Uleina olrikii', 'Aspidophoroides olrikii', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Venericardia borealis', 'Cyclocardia borealis', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Capnella sp.', 'Duva florida', spNames$N_EspSci) # Only Duva species in the St. Lawrence
spNames$N_EspSci <- gsub('Clinocardium ciliatum', 'Ciliatocardium ciliatum', spNames$N_EspSci)
spNames$N_EspSci <- gsub('Ilyanassa obsoleta', 'Tritia obsoleta', spNames$N_EspSci)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 WIDE TO LONG
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
id <- colnames(sp) %in% spNames[,1]

sp <- sp %>%
      gather(key = EspGen,
             value = Presence,
             -vessel, -cruise, -set, -strat, -year, -month, -day, -time,
             -duration, -latitude_st, -latitude_end, -longitude_st,
             -longitude_end)

sp$EspGen <- as.numeric(sp$EspGen)
sp <- left_join(sp, spNames[, c('EspGen', 'N_EspSci')], by = 'EspGen')

# Remove absences
# They will be created again when I generate the community matrix for the analyses.
# For now they are only taking up memory
id0 <- sp$Presence == 0
sp <- sp[!id0, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 REMOVE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All information in './SpeciesFormatting/Code/removeTaxa.R'
load('./BioticData/SpeciesFormatting/Data/removeTaxa.RData')


# spNames[!spNames[, 'N_EspSci'] %in% removeTaxa, ] # To visualize data removed

# Remove species from sp dataset
id0 <- sp$N_EspSci %in% removeTaxa
sp <- sp[!id0, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 COMBINE TAXA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  taxonCombRec <- sp[, 'N_EspSci'] == combineTaxa[[i]][1]

  # if taxon exist in list, replace taxa name that will be aggregated with taxonComb
  if(sum(taxonCombRec) > 0) {
    # Extract variables for taxon used for combination
    taxonComb <- sp[taxonCombRec, ][1, c('EspGen','N_EspSci')]

    # Identify which taxa have to be combined with taxonComb
    taxaReplace <- sp[, 'N_EspSci'] %in% combineTaxa[[i]][2:nTaxa]

    # Replace names of taxa to combine with taxonComb
    sp[taxaReplace, c('EspGen','N_EspSci')] <- taxonComb
  } else {
    # Create new EspGen for taxon and create vector of variables used to describe taxon used for combination
    # !!!!! There may already be an existing number. Need to figure this out at some point !!!!!!
    newEspGen <- max(as.numeric(paste(sp[,'EspGen']))) + 1
    taxonComb <- data.frame(EspGen = newEspGen, N_EspSci = combineTaxa[[i]][1], stringsAsFactors = FALSE)

    # Identify which taxa have to be combined with taxonComb
    taxaReplace <- sp[, 'N_EspSci'] %in% combineTaxa[[i]][2:nTaxa]

    # Replace names of taxa to combine with taxonComb
    sp[taxaReplace, c('EspGen','N_EspSci')] <- taxonComb
  }
}

# Update species names
spNames <- sp[, c('EspGen','N_EspSci')]
spNames <- distinct(spNames)
spNames <- spNames[order(spNames[, 2]), ]




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               NUMBER OF RECORDS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count number of observations per species
spNames$countRec <- table(sp$N_EspSci)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add mid-way point between beginning and end of trawling activities
sp$LoMid <- rowMeans(sp[, c('longitude_st','longitude_end')])
sp$LaMid <- rowMeans(sp[, c('latitude_st','latitude_end')])

# Point shapefile
sp <- st_as_sf(sp, coords = c('LoMid','LaMid'), crs = 4326) %>%
      st_transform(crs = 32198)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(sp, file = './BioticData/SudGolfe_RelevePluriSp_MPO/Data/Biotic/SouthPluri.RData')
save(spNames, file = './BioticData/SudGolfe_RelevePluriSp_MPO/Data/Biotic/SouthPluriSP.RData')

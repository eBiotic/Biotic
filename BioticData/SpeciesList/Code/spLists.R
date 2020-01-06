load('./BioticData/Combine_RelevePluriSp_MPO/Data/CombinePluriSP.RData')
load('./BioticData/ZIF-Fisheries-2010-2015/Data/Biotic/zifSp.RData')
load('./BioticData/ObsMer_1999-2015_MPO/Data/Biotic/SeaObserverSP.RData')
spList$CountRec <- spList$combRec
spList <- spList[, c('species','CountRec')]

library(magrittr)
library(tidyverse)
sp <- rbind(spList, zifSp, obsSp) %>%
      group_by(species) %>%
      summarise(Count = sum(CountRec)) %>%
      as.data.frame()

save(sp, file = './BioticData/SpeciesList/Data/SpeciesList.RData')

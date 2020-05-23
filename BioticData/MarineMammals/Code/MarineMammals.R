# Marine mammals list
mm <- c("Balaenoptera musculus",
        "Balaenoptera physalus",
        "Megaptera novaeangliae",
        "Balaenoptera acutorostrata",
        "Eubalaena glacialis",
        "Balaenoptera borealis",
        "Balaena mysticetus",
        "Stenella frontalis",
        "Lagenorhynchus acutus",
        "Delphinapterus leucas",
        "Tursiops truncatus",
        "Ziphius cavirostris",
        "Phocoena phocoena",
        "Orcinus orca",
        "Globicephala melas",
        "Monodon monoceros",
        "Hyperoodon ampullatus",
        "Kogia breviceps",
        "Grampus griseus",
        "Delphinus delphis",
        "Physeter macrocephalus",
        "Stenella coeruleoalba",
        "Lagenorhynchus albirostris",
        "Erignathus barbatus",
        "Halichoerus grypus",
        "Phoca vitulina",
        "Pagophilus groenlandicus",
        "Cystophora cristata",
        "Pusa hispida",
        "Odobenus rosmarus")


# Species list
mmSp <- data.frame(species = mm, CountRec = 0)

# Export list
save(mmSp, file = './BioticData/MarineMammals/Data/Biotic/MarineMammalsSP.RData')

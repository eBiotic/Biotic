# List of taxa to remove
removeTaxa <- c(
  'Acanthephyra pelagica', # Surveys not designed to capture these taxa, pelagic species
  'Acanthephyra sp.', # Surveys not designed to capture these taxa, pelagic species
  "Acipenseridae", # Group is too coarse
  "Acipenseriformes", # Group is too coarse
  'Actiniaria',  # Group is too coarse# Group is too coarse
  'Agarum clathratum', # Surveys not designed to capture these taxa
  'Agarum sp.', # Surveys not designed to capture these taxa
  "Agonidae", # Group is too coarse
  "Alcidae", # Group is too coarse
  'Alcyonacea', # Group is too coarse
  'Alosa sp.', # Group is too coarse
  "Ammodytidae", # Group is too coarse
  'Amphipoda', # Group is too coarse
  "Anarhichadidae", # Group is too coarse
  "Anguilliformes", # Group is too coarse
  "Anguilloidei", # Group is too coarse
  'Annelida',  # Group is too coarse
  'Anthozoa', # Group is too coarse
  "Aphroditidae", # Group is too coarse
  'Archaeogastropoda', # Group is too coarse
  "Argentinidae", # Group is too coarse
  "Argis sp.", # Group is too coarse
  'Asteriidae', # Group is too coarse
  'Asteroidea', # Group is tåoo coarse
  'Bivalvia', # Group is too coarse
  "Brachyura", # Group is too coarse
  'Brada inhabilis', # Identification difficult, likely grouped as 'Polychaeta'
  'Buccinidae', # Group is too coarse
  'Cadlina laevis', # coastal species # Surveys not designed to capture these taxa
  "Cancridae", # Group is too coarse
  'Cardiidae', # Group is too coarse
  'Cephalaspidea', # Group is too coarse
  'Cephalopoda', # Group is too coarse
  "Ceratiidae", # Group is too coarse
  "Chauliodontinae", # Group is too coarse
  "Chondrichthyes", # Group is too coarse
  'Chondrus crispus', # Surveys not designed to capture these taxa
  'Cirripedia', # Group is too coarse
  "Clupeidae", # Group is too coarse
  'Clypeasteroida', # Group is too coarse
  'Cnidaria', # Group is too coarse
  'Coelenterata', # Group is too coarse
  "Copepoda", # Group is too coarse
  'Cottidae', # Group is too coarse
  "Crangonidae", # Group is too coarse
  'Crustacea', # Group is too coarse
  "Cryptodonta", # Group is too coarse
  'Ctenophora', # Group is too coarse
  'Cumacea', # Group is too coarse
  "Cyclopteridae", # Group is too coarse
  'Decapoda', # Group is too coarse
  'Dentaliidae', # Group is too coarse
  "Diodontidae", # Group is too coarse
  'Echinodermata', # Group is too coarse
  "Echinoidea", # Group is too coarse
  'Echiura', # Group is too coarse
  'Epitonium sp.', # Likely mistaken for Boreotrophon
  'Eualus sp.', # species readily identified
  'Euphausiacea', # Group is too coarse & survey not designed to capture this taxon
  'Fecampiidae', # Group is too coarse & survey not designed to capture this taxon
  'Flabelligeridae', # Group is too coarse
  'Gadidae', # Group is too coarse
  'Gadus sp.', # Component species differ in spatial distribution
  'Gammaridea', # Group is too coarse
  'Gammarus sp.', # Coastal species # Surveys not designed to capture these taxa
  'Gasterosteidae', # alternatively, could be combined with Gasterosteus aculeatus aculeatus
  'Gastropoda', # Group is too coarse
  'Golfingia (golfingia) margaritacea', # Hard to identify in the field
  'Golfingia margaritacea', # Hard to identify in the field
  'Holothuroidea', # Group is too coarse
  'Hyas sp.', # Species easily identifiable and mostly identified in the dataset
  'Hydrozoa',  # probably composed of pelagic hydrozoa P. lactea and S. martensii which can be confounded with scyphozoa A. aurita
  "Hyperiidea", # Group is too coarse
  "Incirrina", # Group is too coarse
  "Invertebrata", # Group is too coarse
  'Isididae', # Group is too coarse
  'Isopoda', # Group is too coarse
  'Laminaria sp.', # Surveys not designed to capture these taxa
  'Lamnidae', # Group is too coarse
  'Lamniformes', # Group is too coarse
  'Lebbeus sp.', # species readily identified
  'Lepidoteuthidae', # Group is too coarse
  'Liparidae', # Group is too coarse
  'Liparis sp.', # species distinguishable
  "Lithodidae", # Group is too coarse
  "Littorinidae", # Group is too coarse
  "Loliginidae", # Group is too coarse
  'Loligo pealei', # Species from the East American coast, wrongly identified in the St. Lawrence in the past. See C. Nozères catalog for more information
  'Lumpenus sp.', # too coarse
  'Lycenchelys sp.', # species distinguishable
  'Lycodes sp.', # species distinguishable
  "Macrouridae", # Group is too coarse
  'Macrura', # Group is too coarse
  'Maldanidae', # Group is too coarse & survey not designed to capture this taxon
  'Meganyctiphanes norvegica', # pelagic species # Surveys not designed to capture these taxa
  'Melinna cristata', # Identification difficult, likely grouped as 'Polychaeta'
  "Melanostomiinae", # Group is too coarse
  "Merlucciidae", # Too coarse
  "Merluccius sp.", # too coarse
  'Mollusca', # Group is too coarse
  'Myctophiformes', # Group is too coarse
  "Myliobatiformes", # Group is too coarse
  'Mysida', # Group is too coarse & survey not designed to capture this taxon
  'Mytilidae', # Group is too coarse
  'Nematoda', # Group is too coarse & survey not designed to capture this taxon
  'Nemertea', # Group is too coarse & survey not designed to capture this taxon
  'Nephtheidae', # could be grouped in a single group if species have similar distributions (Duva florida, Gersemia rubiformis, Drifa glomerata)
  'Nephtys sp.', # Identification difficult, likely grouped as 'Polychaeta'
  "Nereididae", # Group is too coarse
  'Nereis pelagica', # Identification difficult, likely grouped as 'Polychaeta'
  'Nudibranchia', # Group is too coarse
  'Octopodidae', # Group is too coarse
  "Ocythoidae", # Group is too coarse
  "Ommastrephidae", # Group is too coarse
  'Onychoteuthidae', # species are identified at the species level
  "Ophidiidae", # Group is too coarse
  'Ophiuroidea', # Group is too coarse
  "Oregoniidae", # Group is too coarse
  "Pandalidae", # Group is too coarse
  'Pandalus sp.', # Group is too coarse
  'Paralepididae', # Group is too coarse
  'Paraliparis sp.', # species distinguishable
  "Pectinidae", # Group is too coarse
  "Pennatulacea", # Group is too coarse
  "Perciformes", # Group is too coarse
  'Phaeophyceae', # Surveys not designed to capture these taxa
  "Phalacrocoracidae", # Group is too coarse
  'Phoca sp.', # Group is too coarse
  'Phocoenidae', # Group is too coarse
  "Pinnipedia", # Group is too coarse
  'Pleuromamma sp.', # Survey not designed for it
  'Placopecten magellanicus', # coastal & shallow water species # Surveys not designed to capture these taxa
  "Pleuronectidae", # Group is too coarse
  'Pleuronectiformes', # Group is too coarse
  'Polychaeta', # Group is too coarse
  'Polyphysia crassa', # Identification difficult, likely grouped as 'Polychaeta'
  'Polyplacophora', # Group is too coarse
  'Priapulus caudatus', # Surveys not designed to capture these taxa
  'Pteraster sp.', # species easily distinguishable
  'Pycnogonida', # Group is too coarse
  "Raja sp.", # Group is too coarse
  "Rajidae", # Group is too coarse
  "Rajiformes", # Group is too coarse
  'Rhodophyta', # Surveys not designed to capture these taxa
  'Sabinea sp.', # species readily identified
  'Salmo sp.', # species readily identified
  'Scaphopoda', # Group is too coarse
  "Scorpaenidae", # Group is too coarse
  'Scyliorhinidae', # Group is too coarse
  'Scyphozoa',  # probably composed of pelagic scyphozoa A. aurita which can be confounded with hydrozoa P. lactea and S. martensii
  'Selachii', # Groups too coarse
  'Sepiolodae', # Group is too coarse
  "Sepiolidae", # Group is too coarse
  'Sipuncula', # Group is too coarse
  'Sipunculus sp.', # Hard to identify in the field
  'Spirontocaris sp.', # species readily identified
  "Squalidae", # Group is too coarse
  "Squaliformes", # Group is too coarse
  'Sternoptychidae', # Group is too coarse
  'Stichaeidae', # Group is too coarse
  "Teuthida", # Group is too coarse
  "Thalassinidea", # Group is too coarse
  'Thaliacea', # Group is too coarse
  'Themisto compressa', # pelagic species # Surveys not designed to capture these taxa
  'Themisto libellula', # pelagic species # Surveys not designed to capture these taxa
  'Trematoda', # Group is too coarse
  'Trochidae', # Group is too coarse
  'Turbellaria', # Group is too coarse & survey not designed to capture this taxon
  'Uria sp.', # Group is too coarse
  'Urophycis sp.', # Group is too coarse
  'Zoarcidae' # Group is too coarse
)


save(removeTaxa, file = './BioticData/SpeciesFormatting/Data/removeTaxa.RData')

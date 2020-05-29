# source('./BioticData/SpeciesFormatting/Code/combineTaxa.R')
# Form groups of taxa as a vector, with taxa to combine seperated with ' | '
# Main taxa that will be used for aggregation should be place first

# Taxa are kept, to the extent possible, at the species level. Groups that are
# too coarse are generally removed from the dataset. Combined taxa are generally
# species that are closely related with similar functional roles in ecosystems
# and that are hard to distinguish in the field. For example, species of the
# genus Buccinum are aggregated as they have similar functional roles and can
# be very hard to differentiate. Certain large groups like Porifera were also
# retained, as they represent a widely distributed group and their
# identification in the field is hard to accomplish. Removing Porifera in
# favour of species would likely result in a gross underestimates of this group
# in the St. Lawrence and likely include misidentifications.

# Every removal or aggregation is documented in the following code to allow for
# increased transparency and reproducibility.

# Additionnal notes:
  # 'Bryozoa | Alcyonidium sp. | Alcyonidium pachydermatum | Reteporella grimaldii | Securiflustra securifrons | Caberea ellisii'
  # Bryozoa retained, coarse but represent a widely distributed group that would
  # be vastly underestimated if left out. Other 4 species and 1 genus are
  # larger, errected species more easily recognized, hence they are kept
  # individually. Since there is not a lot of data to describe them, however,
  # it may be wise to group them all under the bryozoa umbrella for the analysis.

# To verify in particular:
    # - Myctophidae
    # - Gaidropsarus sp.
    # - Artediellus sp.
    # - Liparidae (stays at the species level)
    # - Lycodes (stays at the species level) + Lycodes terraenovae & Lycodes esmarkii
    # - Lycenchelys (stays at the species level)
    # - Adjust Eumicrotremus spinosus

combineTaxa <- c(
  # Identification: hard to distinguish:
  'Strongylocentrotus sp. | Strongylocentrotus droebachiensis',
  'Sebastes sp. | Sebastes norvegicus | Sebastes mentella',
  'Henricia sp. | Henricia sanguinolenta',


  # Combine, hard to distinguish in the field. Porifera is coarse, but represent
  # a widely distributed group that would be vastly underestimated if left out.
  # We therefore combine them at the 'Porifera' level, only leaving
  # 'Stylocordyla borealis' as a species, as it is easily identified
  'Porifera | Tentorium semisuberites | Radiella hemisphaerica | Polymastia sp. | Halichondia panicea | Haliclona oculata | Haliclona sp. | Mycale lingua | Phakelia sp. | Suberites ficus | Weberella bursa',


  # Very likely the same species:
  'Atolla wyvillei | Atolla sp.',
  'Lithodes maja | Lithodes sp.',
  'Hormathia nodosa | Hormathia sp. | Hormathia tuberculosa',
  'Illex illecebrosus | Illex sp.',

  # Identification: can be hard to distinguish in the field and they are species
  # that generally have similar functional roles and habitat requirements. Only
  # 'Aphroditella hastata' is left as a species, as it is frequently caught and
  # easily identifiable due to its size
  'Polynoidae | Euphrosine borealis | Harmothoe sp. | Eunoe nodosa | Laetmonice filicornis',
  'Aphroditella hastata | Aphroditella sp.',

  # Combine, hard to distinguish in the field. Ascidiacea is coarse, but
  # represent a widely distributed group that would be vastly underestimated if
  # left out. We therefore combine them at the 'Ascidiacea' level, only leaving
  # 'Boltenia ovifera' and 'Eudistoma vitreum' as species, as they are easily
  # identified
  'Ascidiacea | Pelonaia corrugata | Botrylloides sp. | Ascidia sp. | Cnemidocarpa finmarkiensis | Synoicum pulmonaria | Polycarpa fibrosa | Halocynthia pyriformis | Boltenia echinata | Boltenia sp. | Tunicata',

  # Only consider genus level
  'Tritia sp. | Tritia obsoleta',

  # species easily distinguishable, but often found in similar locations and
  # O. robusta is smaller and often hard to distinguish in trawl among important
  # biomass of O. sarsii
  'Ophiura sp. | Ophiura sarsii | Ophiura robusta',

  # Very likely the same species, in any case will represent the same group.
  # Combine at the genus level, as other less frequent species might be included.
  # Keep in mind that the most likely species is the one being aggregated
  'Actinauge sp. | Actinauge cristata | Actinauge verrillii',
  'Actinostola sp. | Actinostola callosa',
  'Bolocera sp. | Bolocera tuediae',
  'Stephanauge sp. | Stephanauge nexilis',

  # 'Bryozoa | Alcyonidium sp. | Alcyonidium pachydermatum | Reteporella grimaldii | Securiflustra securifrons | Caberea ellisii'
  # Bryozoa retained, coarse but represent a widely distributed group that would
  # be vastly underestimated if left out. Other 4 species and 1 genus are
  # larger, errected species more easily recognized, hence they are kept
  # individually. Since there is not a lot of data to describe them, however,
  # it may be wise to group them all under the bryozoa umbrella for the analysis.
  'Bryozoa | Bryozoans | Bryozoans brachiopoda | Bryozoans ectoprocta',

  # Identification: can be hard to distinguish species from this group in the
  # field and they are species that generally have similar functional roles and
  # habitat requirements. Grouped at the genus level
  'Alcyonidium sp. | Alcyonidium pachydermatum',
  'Nuculana sp. | Nuculana tenuisulcata',
  'Mytilus sp. | Mytilus edulis',
  'Astarte sp. | Astarte subaequilatera | Astarte borealis | Astarte undata',
  'Buccinum sp. | Buccinum scalariforme | Buccinum undatum',
  'Asterias sp. | Asterias vulgaris | Asterias rubens', # Asterias rubens is likely an identification error, as it is mostly coastal
  'Colus sp. | Colus stimpsoni | Colus pubescens | Plicifusus kroeyeri',
  'Neptunea sp. | Neptunea decemcostata | Neptunea despecta',
  'Dendronotus sp. | Dendronotus frondosus',
  'Tonicella sp. | Tonicella rubra',
  'Nymphon sp. | Nymphon hirtipes',
  'Pagurus sp. | Pagurus pubescens | Paguroidea',
  'Molpadia sp. | Molpadia oolitica | Molpadia',
  'Gorgonocephalus sp. | Gorgonocephalus arcticus',
  'Flabellum sp. | Flabellum alabastrum',
  'Margarites sp. | Margarites groenlandicus | Margarites costalis',
  'Boreotrophon sp. | Boreotrophon clathratus | Scabrotrophon fabricii',
  'Poraniomorpha sp. | Poraniomorpha hispida',
  'Musculus sp. | Musculus niger',
  'Cuspidaria sp. | Cuspidaria glacialis',
  'Icelus sp. | Icelus bicornis | Icelus spatula',
  'Triglops sp. | Triglops murrayi | Triglops nybelini',
  'Myoxocephalus sp. | Myoxocephalus octodecemspinosus | Myoxocephalus scorpius | Myoxocephalus aenaeus | Myoxocephalus quadricornis | Myoxocephalus scorpioides',
  'Bathypolypus sp. | Bathypolypus bairdii | Bathypolypus arcticus | Octopoda',

  'Macoma sp. | Macoma calcarea',
  'Leptasterias sp. | Leptasterias polaris',
  'Rossia sp. | Rossia megaptera | Rossia palpebrosa | Semirossia tenera',


  # Identification: can be hard to distinguish species from this group in the
  # field and they are species that generally have similar functional roles and
  # habitat requirements.
  'Naticidae | Cryptonatica affinis | Euspira sp. | Euspira pallida | Euspira heros',

  # Very likely the same species, in any case will represent the same group.
  # Combine at the genus level, as other less frequent species might be included.
  'Ampelisca sp. | Ampelisca eschrichtii',

  # Keep even though coarse. The family represents the same type of species
  # functionally and are hardly distinguishable in the field. Combined with
  # Balanus balanus, which is often grouped in the Balanidae family during
  # identification.
  'Balanidae | Balanus balanus',

  # Subspecies too detailed for my analyses
  'Eualus gaimardii | Eualus gaimardii belcheri | Eualus gaimardii gaimardii',
  'Eumicrotremus spinosus | Eumicrotremus spinosus variabilis',

  # Grouped at the species level. I is unlikely that there are other species in
  # the St. Lawrence with the same physiology. If there are, it could be
  # 'Poliometra proliza', which is found in the Arctic, but not recorded in
  # the St. Lawrence. In any event, they share similar functions and are often
  # found together in the Arctic.
  'Heliometra glacialis | Crinoidea',

  # Combine, hard to distinguish in the field and likely composed of functionally
  # similar species limited to the two listed here.
  'Velutinidae | Velutina velutina | Limneria undata',

  # Combine at the species level, likely the only species of that group in the
  # St. Lawrence
  'Arrhoges occidentalis | Aporrhais sp.',
  'Lithodes maja | Lithodes sp.',
  'Crossaster papposus | Crossaster sp.',
  'Terebratulina septentrionalis | Terebratulina sp.',
  'Chionoecetes opilio | Chionoecetes sp.',


  # Readily identified
  'Epizoanthus erdmanni | Epizoanthus sp.',

  # Combine at the family level. The only other species found in the St.
  # Lawrence from this group is 'Cyclothone braueri', which has similar feeding
  # strategies, habitat requirements and functional role.
  'Gonostomatidae | Cyclothone microdon',

  # Diverse group that can be hard to distinguish in the field and that are
  # found in similar types of habitats. Most specimen are identified at the
  # family level. Identifications at the genus or species level should be
  # correct, but since there are vastly more speciments attributed to the
  # family, all specimens from this family will be aggregated to the family
  # level 'Myctophidae' and used for analyses. Maybe consider adding Neoscopelus
  # macrolepidotus, which is also species found at great depths, but member of
  # the family 'Neoscopelidae'. For now, consider individually.
  'Myctophidae | Benthosema glaciale | Lampadena speculigera | Notoscopelus sp. | Notoscopelus elongatus | Myctophum punctatum | Myctophum sp.',


  # Identification: can be hard to distinguish species from this group and they
  # are species that generally have similar functional roles and habitat
  # requirements. G. ensis is generally located in the St. Lawrence, while
  # G. argentatus is generally in the north-eastern Atlantic. Grouped at the
  # genus level
  'Gaidropsarus sp. | Gaidropsarus argentatus | Gaidropsarus ensis',

  # Identification: can be hard to distinguish species from this group in the
  # field, except for mature males, and they are species that generally have
  # similar functional roles and habitat requirements. Grouped at the genus level
  'Artediellus sp. | Artediellus atlanticus | Artediellus uncinatus',

  # Often confounded and might in fact be a single species. Grouped under the
  # 'Lycodes terraenovae' umbrella, but keep in mind that it includes both
  # species
  'Lycodes terraenovae | Lycodes esmarkii',

  # Distribution and physiological overlap between the two species, record at
  # the genus level.
  'Ammodytes sp. | Ammodytes americanus | Ammodytes dubius',

  # Subspecies too detailed for my analyses
  'Arctozenus risso | Arctozenus risso kroyeri',

  # Change to species, assuming that it is Stomias boa ferox
  'Stomias boa ferox | Stomias boa boa | Stomias boa',

  # Marine birds, to verify at some point but likely unimportant for the datasets I currently have in hand
  'Larus sp. | Larus marinus',

  # For fish, might need to be revised as well
  'Lampanyctus sp. | Lampanyctus macdonaldi',

  # Genus scale sufficient
  'Mesodesma sp. | Mesodesma arctatum',

  # All same species, keep to species level
  'Ciliatocardium ciliatum | Clinocardium sp. | Clinocardium ciliatum',

  # Same species
  'Osmerus mordax mordax | Osmerus mordax'

)

save(combineTaxa, file = './BioticData/SpeciesFormatting/Data/combineTaxa.RData')

# MAREANO_Video

The goal of the repository is to compile scripts that work on the Mareano video data in an organized way.

The first stage (i.e. 01_) deals with data preparation. It currently treats the raw mareano video data by applying filters to remove certain species and also by merging some taxonomic units. and produces: otu, env, taxonomic trees
(We could adapt the script to easily apply different treatments depending on the needs of the analyses planned)

# The current filters to species data are:
  - At least 20 sightings
  - Size class is either "OK" or "Detailed"
  - Specificity of otu <4
  - Select only "Organisms"
  - Exclude pelagic species
  - Exclude Grouping_taxon_biotope "Algae", "Bacteria", "Biota" and "Foraminifera"

# Stations are also excluded if:
  - Lengths of transect is not between 190 and 300 meters
  - (no_vision + poor_vision) make up 50% or more of the transect
  - Stations correspond to 2006612 cruise
  - A few additional stations are excluded due to strong variability between neighboring sites intering with the autocorrelation analyses (THIS SHOULD BE REVISITED!)
  - Some stations might also be excluded if any of the key environmental parameters used in the analyses after are NA




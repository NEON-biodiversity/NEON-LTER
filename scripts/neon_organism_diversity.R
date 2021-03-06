## TITLE:         NEON Organismal Data: read in raw data (fr API script), compute diversity measures.
## AUTHOR:        Phoebe Zarnetske, Quentin Read 
## COLLABORATORS: Sydne Record (Bryn Mawr), Ben Baiser (UFL), Angela Strecker (PSU), 
##                John M. Grady (MSU/Bryn Mawr), Jonathan Belmaker (Tel Aviv U), Mao-Ning Tuanmu (Academia Sinica),
##                Lydia Beaudrot (Rice U), Kate Thibault 
## DATA:          NEON organismal data: all species, all years, all sites
## PROJECT:       "NEON's continental-scale biodiversity"
## DATE:          initiated: June 18, 2018; last run:

## This script reads in NEON's organismal raw data across all available sites, 
# computes diversity measures per site and year, and cumulatively,
# and exports those data. 

# Modified 26 June: add some options to richness_cumulative() function
# Modified 24 June: moved all plotting code to a different script that can be run locally with data on google drive

## On HPCC; add this command to load recent R version
#module swap GNU GNU/4.9
#module load OpenMPI 1.10.0
#module load R/3.3.2

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

# Set file paths
data_path <- '/mnt/research/neon/raw_data/organismal_data_june2018'
tax_path <- '/mnt/research/neon/raw_data/taxonomy_lists_june2018'
fig_path <- '/mnt/research/neon/MS4_NEONOrganisms/figs'
export_path <- '/mnt/research/neon/final_data/richness'

#Install/load packages
for (package in c("lme4", "dplyr", "purrr", "reshape2", "lubridate", "iNEXT")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

## Code below from https://github.com/NEON-biodiversity/teaching/tree/master/grad_lab/neon_api_grad_lab_rawcode.R

# -------------------------------------------------------------------------------------------------------------------------

# ----------------------------------------
# Load all organismal data and do QC on it
# ----------------------------------------

# Added by QDR, 19 June 2018
# Modified by QDR, 20 June 2018: add other taxa besides mammals

# Read in data from each taxon
# Note: data are pulled with the R script on github/sydnerecord/NEON/data_extraction/datapull_neonapi.r
# The data were all pulled on 20 June 2018

mammal_data <- read.csv(file.path(data_path,"mammal_data.csv"), stringsAsFactors = FALSE)
bird_data <- read.csv(file.path(data_path,"bird_pointcount.csv"), stringsAsFactors = FALSE)
aq_plant_data <- read.csv(file.path(data_path,"aquatic_plants.csv"), stringsAsFactors = FALSE)
macroinvert_data <- read.csv(file.path(data_path,"benthic_macroinvertebrates.csv"), stringsAsFactors = FALSE)
fish_data <- read.csv(file.path(data_path,"fish_trapping.csv"), stringsAsFactors = FALSE)
mosq_data <- read.csv(file.path(data_path,"mosquito_trapping_IDs.csv"), stringsAsFactors = FALSE)
phytop_data <- read.csv(file.path(data_path,"phytoplankton.csv"), stringsAsFactors = FALSE)
plant_data <- read.csv(file.path(data_path,"plant_percentcover_100m2.csv"), stringsAsFactors = FALSE)
tick_data <- read.csv(file.path(data_path,"tick_samples.csv"), stringsAsFactors = FALSE)
tree_data <- read.csv(file.path(data_path,"woody_plant_survey.csv"), stringsAsFactors = FALSE)
zoop_data <- read.csv(file.path(data_path,"zooplankton.csv"), stringsAsFactors = FALSE)
beetle_data <- read.csv(file.path(data_path,"ground_beetles.csv"), stringsAsFactors = FALSE)

# Read in taxonomy for any groups it's needed to exclude species
mammal_tax <- read.csv(file.path(tax_path, 'small_mammal_taxonomy.csv'), stringsAsFactors = FALSE)  
beetle_tax <- read.csv(file.path(tax_path, 'beetle_taxonomy.csv'), stringsAsFactors = FALSE)  
tick_tax <- read.csv(file.path(tax_path, 'tick_taxonomy.csv'), stringsAsFactors = FALSE)  

# QC for all taxa
# ---------------------

# Get rid of anything not listed as target taxa (some taxa do not have this distinction). 
# For example for mammals that would be bycatch that are not nocturnal rodents
# For now just get rid of anything not identified to species
# Use the following rule: if there is no species-level identification within a given genus, keep those individuals because they can all be treated as a single species
# But if there are any individuals in a genus that are identified to species, we have to get rid of all the un-ID'd ones because they could be part of that species
# If anything is identified to a level even coarser than genus, get rid of it

# Edit 24 June: make a separate tree data frame with only the trees in Little's list.

# QC function to keep only taxa we want.
keep_taxa <- function(dat, column = 'taxonID') {
  not_to_sp <- grepl('sp.', dat$scientificName, fixed = TRUE) | grepl('spp.', dat$scientificName, fixed = TRUE) | dat$scientificName == ''
  if (with(dat, exists('specificEpithet'))) {
    not_to_sp[is.na(dat$specificEpithet) | dat$specificEpithet == ''] <- TRUE
  }
  genera_with_species <- unique(dat$genus[!not_to_sp])
  na.omit(unique(dat[!(((dat$genus %in% genera_with_species) & not_to_sp) | dat$taxonRank %in% c('family','order','phylum','class','kingdom','superorder')), column]))
}

mammal_data <- mammal_data %>%
  filter(grepl('4|5', trapStatus), 
         recapture %in% 'N', 
         !taxonID %in% mammal_tax$taxonID[mammal_tax$dwc.specificEpithet %in% 'sp.' | !mammal_tax$taxonProtocolCategory %in% 'target'],
         nchar(taxonID) > 0) %>% # Also get rid of empty traps and recaptured individuals
  mutate(year = year(collectDate))	
bird_data <- bird_data %>%
  filter(taxonRank %in% c('species', 'subspecies')) %>%
  mutate(year = year(startDate))
aq_plant_data <- aq_plant_data %>%
  mutate(taxonRank = 'none') %>%
  filter(scientificName %in% (keep_taxa(.,'scientificName'))) %>%
  mutate(year = year(collectDate))
macroinvert_data <- macroinvert_data %>%
  filter(acceptedTaxonID %in% keep_taxa(.,'acceptedTaxonID')) %>%
  mutate(year = year(collectDate))
fish_data <- fish_data %>%
  filter(taxonRank %in% c('species', 'subspecies')) %>%
  mutate(year = year(passStartTime))
mosq_data <- mosq_data %>%
  filter(scientificName %in% keep_taxa(.,'scientificName')) %>%
  mutate(year = year(collectDate))
plant_data <- plant_data %>%
  filter(taxonRank %in% c('species', 'speciesGroup', 'subspecies', 'variety')) %>%
  mutate(year = year(endDate))
tree_data <- tree_data %>%
  filter(taxonRank %in% c('species', 'speciesGroup', 'subspecies', 'variety')) %>%
  mutate(year = year(date))
beetle_data <- beetle_data %>%
  mutate(genus = map_chr(strsplit(scientificName, ' '), function(x) if (length(x) > 0) x[1] else '')) %>%
  filter(taxonID %in% beetle_tax$taxonID[beetle_tax$taxonProtocolCategory == 'target']) %>%
  filter(taxonID %in% keep_taxa(.)) %>%
  mutate(year = year(collectDate))
zoop_data <- zoop_data %>%
  mutate(genus = map_chr(strsplit(scientificName, ' '), function(x) if (length(x) > 0) x[1] else '')) %>%
  filter(taxonID %in% keep_taxa(.)) %>%
  mutate(year = year(collectDate))
phytop_data <- phytop_data %>%
  filter(scientificName %in% keep_taxa(., 'scientificName')) %>%
  mutate(year = year(collectDate))
### there are only two species of tick so there is no point in doing ticks.

# Little trees only
little_spp <- read.csv('/mnt/research/neon/final_data/richness/Little_tree_by_NEON_site.csv', stringsAsFactors = FALSE)
little_tree_data <- tree_data %>%
  mutate(binomial = map_chr(strsplit(tree_data$scientificName, ' '), function(x) paste(x[1],x[2]))) %>%
  filter(binomial %in% little_spp$binomial)

# -------------------------------------------------------------------------------------------------------------------------
  
# ----------------------------  
# Richness estimator functions
# ----------------------------

# Added by QDR, 19 June 2018
# Modified by QDR, 20 June 2018: Create functions for cumulative richness by year and for reshaping data to plot.

# Chao1 richness estimator
# Takes as input a vector x of species IDs
# Requires observed abundances.
estimator_chao1 <- function(x) {
  xcomm <- table(x)
  S_obs <- length(xcomm) # Number of species observed
  f1 <- sum(xcomm == 1) # Number of singletons
  f2 <- sum(xcomm == 2) # Number of doubletons
  chao1 <- S_obs + (f1 * (f1 - 1)) / (2 * (f2 + 1)) # Calculate chao1 estimator
  var_chao1 <- f2 * ( ((f1/f2)/4)^4 + (f1/f2)^3 + ((f1/f2)/2)^2 ) # Variance of estimator
  if (!is.finite(var_chao1)) var_chao1 <- 0 # If no doubletons, variance is zero
  return(data.frame(chao1 = chao1, 
					chao1_var = var_chao1,
					chao1_CImin = max(S_obs, chao1 - 1.96 * sqrt(var_chao1)),
					chao1_CImax = chao1 + 1.96 * sqrt(var_chao1)))
}

# Asymptotic richness estimator, using iNEXT package.
# Takes as input a vector x of species IDs
# Requires observed abundances.

# For now, just get the asymptotic estimator for richness, and the bounds of its 95% conf int
# Later we can extract even more output from the iNEXT output object.
estimator_asymp <- function(x) {
  require(iNEXT)
  xcomm <- table(x)
  inext_out <- iNEXT(x = list(as.numeric(xcomm)), q = 0, datatype='abundance', nboot = 99) # run iNEXT on the community
  richness_est <- subset(inext_out$AsyEst, Diversity == 'Species richness') # Extract only richness info from output
  return(with(richness_est, data.frame(asymp_est = Estimator, 
									   asymp_est_stderr = s.e., 
									   asymp_est_CImin = LCL, 
									   asymp_est_CImax = UCL)))
}

# Function to get cumulative richness estimators by site and year
# Sequentially add years and see what happens to the cumulative observed richness and richness estimators.
# Each year's result represents all data up to that point in time.

# Modified 26 June 2018: duplicate $namedLocation to $plotID (applies to macroinverts, aquatic plants, zooplankton, fish) 
aq_plant_data$plotID<-aq_plant_data$namedLocation
fish_data$plotID<-fish_data$namedLocation
macroinvert_data$plotID<-macroinvert_data$namedLocation
phytop_data$plotID<-phytop_data$namedLocation
zoop_data$plotID<-zoop_data$namedLocation

# Modified 26 June: add option to do by site or by plot and to keep either the final observed values or all years
# (group argument and by_year = TRUE or FALSE)
richness_cumulative <- function(dat, column = 'taxonID', group = 'site', by_year = TRUE) {
  dat <- dat %>%
    rename(sp = !!column) %>%
    select(siteID, plotID, year, sp)
  if (group == 'site') dat <- dat %>% group_by(siteID)
  if (group == 'plot') dat <- dat %>% group_by(siteID, plotID)
  out <- dat %>%
    do(cbind(year = min(.$year):max(.$year),
             richness = map_int(min(.$year):max(.$year), function(yr) length(unique(.$sp[.$year <= yr]))),
             map_dfr(min(.$year):max(.$year), function(yr) estimator_chao1(.$sp[.$year <= yr])),
             map_dfr(min(.$year):max(.$year), function(yr) estimator_asymp(.$sp[.$year <= yr]))
    ))
  if (!by_year) {
    out %>% filter(year == max(year)) %>% select(-year)
  } else {
    out
  }
}


# ----------------------------------------------------------
# Run richness estimators for all taxa by site and year
# ----------------------------------------------------------

# Added by QDR, 19 June 2018
# Modified by QDR, 20 June 2018: Add other taxa

# Site-level
mammal_richness_cumulative <- richness_cumulative(mammal_data)
bird_richness_cumulative <- richness_cumulative(bird_data)
aq_plant_richness_cumulative <- richness_cumulative(aq_plant_data, column = 'scientificName')
macroinvert_richness_cumulative <- richness_cumulative(macroinvert_data, column = 'acceptedTaxonID')
fish_richness_cumulative <- richness_cumulative(fish_data)
mosq_richness_cumulative <- richness_cumulative(mosq_data, column = 'scientificName')
plant_richness_cumulative <- richness_cumulative(plant_data)
tree_richness_cumulative <- richness_cumulative(tree_data)
beetle_richness_cumulative <- richness_cumulative(beetle_data)
zoop_richness_cumulative <- richness_cumulative(zoop_data)
phytop_richness_cumulative <- richness_cumulative(phytop_data, column = 'scientificName')
littletree_richness_cumulative <- richness_cumulative(little_tree_data, column = 'binomial')

# Plot-level
mammal_richness_cumulative <- richness_cumulative(mammal_data, group='plot')
bird_richness_cumulative <- richness_cumulative(bird_data, group='plot')
aq_plant_richness_cumulative <- richness_cumulative(aq_plant_data, group='plot', column = 'scientificName')
macroinvert_richness_cumulative <- richness_cumulative(macroinvert_data, group='plot', column = 'acceptedTaxonID')
fish_richness_cumulative <- richness_cumulative(fish_data,group='plot')
mosq_richness_cumulative <- richness_cumulative(mosq_data, group='plot', column = 'scientificName')
plant_richness_cumulative <- richness_cumulative(plant_data,group='plot')
tree_richness_cumulative <- richness_cumulative(tree_data,group='plot')
beetle_richness_cumulative <- richness_cumulative(beetle_data,group='plot')
zoop_richness_cumulative <- richness_cumulative(zoop_data,group='plot')
phytop_richness_cumulative <- richness_cumulative(phytop_data, group='plot', column = 'scientificName')
littletree_richness_cumulative <- richness_cumulative(little_tree_data, group='plot', column = 'binomial')


# Export richness data by taxonomic group: site-level
# --------------------------------------
write.csv(mammal_richness_cumulative,file.path(export_path,"mammal_richness_cumulative.csv"),row.names=F)
write.csv(bird_richness_cumulative,file.path(export_path,"bird_richness_cumulative.csv"),row.names=F)
write.csv(beetle_richness_cumulative,file.path(export_path,"beetle_richness_cumulative.csv"),row.names=F)
write.csv(macroinvert_richness_cumulative,file.path(export_path,"macroinvert_richness_cumulative.csv"),row.names=F)
write.csv(mosq_richness_cumulative,file.path(export_path,"mosq_richness_cumulative.csv"),row.names=F)
write.csv(phytop_richness_cumulative,file.path(export_path,"phytop_richness_cumulative.csv"),row.names=F)
write.csv(plant_richness_cumulative,file.path(export_path,"plant_richness_cumulative.csv"),row.names=F)
write.csv(tree_richness_cumulative,file.path(export_path,"tree_richness_cumulative.csv"),row.names=F)
write.csv(zoop_richness_cumulative,file.path(export_path,"zoop_richness_cumulative.csv"),row.names=F)

# Also combine everything into one data frame and write to a single CSV for easier loading.
alltaxa_richness_cumulative <- rbind(
  data.frame(taxon = 'mammal', mammal_richness_cumulative),
  data.frame(taxon = 'bird', bird_richness_cumulative),
  data.frame(taxon = 'beetle', beetle_richness_cumulative),
  data.frame(taxon = 'mosquito', mosq_richness_cumulative),
  data.frame(taxon = 'all plants', plant_richness_cumulative),
  data.frame(taxon = 'all woody plants', tree_richness_cumulative),
  data.frame(taxon = 'trees', littletree_richness_cumulative),
  data.frame(taxon = 'macroinvertebrate', macroinvert_richness_cumulative),
  data.frame(taxon = 'fish', fish_richness_cumulative),
  data.frame(taxon = 'zooplankton', zoop_richness_cumulative),
  data.frame(taxon = 'phytoplankton', phytop_richness_cumulative),
  data.frame(taxon = 'aquatic plants', aq_plant_richness_cumulative)
)

write.csv(alltaxa_richness_cumulative, file.path(export_path, 'alltaxa_richness_cumulative.csv'), row.names = FALSE)

## Edit 26 June 2018 PLZ
# Export the plot-level richness (when including plotID above in richness calculation)
# Export richness data by taxonomic group: plot-level
# --------------------------------------
write.csv(mammal_richness_cumulative,file.path(export_path,"mammal_richness_cumulative_plot.csv"),row.names=F)
write.csv(bird_richness_cumulative,file.path(export_path,"bird_richness_cumulative_plot.csv"),row.names=F)
write.csv(beetle_richness_cumulative,file.path(export_path,"beetle_richness_cumulative_plot.csv"),row.names=F)
write.csv(macroinvert_richness_cumulative,file.path(export_path,"macroinvert_richness_cumulative_plot.csv"),row.names=F)
write.csv(mosq_richness_cumulative,file.path(export_path,"mosq_richness_cumulative_plot.csv"),row.names=F)
write.csv(phytop_richness_cumulative,file.path(export_path,"phytop_richness_cumulative_plot.csv"),row.names=F)
write.csv(plant_richness_cumulative,file.path(export_path,"plant_richness_cumulative_plot.csv"),row.names=F)
write.csv(tree_richness_cumulative,file.path(export_path,"tree_richness_cumulative_plot.csv"),row.names=F)
write.csv(zoop_richness_cumulative,file.path(export_path,"zoop_richness_cumulative_plot.csv"),row.names=F)


# -------------------------------------------------------------------------------------------------------------------------
## End of Code work 21 June 2018 
# Start with Small Mammals
## Work through diversity measure code next: use simple way or Chao
## Simple Richness: sum up by year, site

# Richness by Site and Year
mammal_richness_yr <- mammal_data %>%
  group_by(siteID, year) %>%
  summarize(richness = length(unique(taxonID)))

bird_richness_yr <- bird_data %>%
  group_by(siteID, year) %>%
  summarize(richness = length(unique(taxonID)))

# Overall Richness by Site
mammal_richness_site <- mammal_data %>%
  group_by(siteID) %>%
  summarize(richness = length(unique(taxonID)))

bird_richness_site <- bird_data %>%
  group_by(siteID) %>%
  summarize(richness = length(unique(taxonID))) 
 
## Repeat for Chao... evenness... others?

## Repeat, but demonstrate improving estimates (or at least different estimates)
## based on repeat sampling with some sites having multiple sampling bouts in a yr, 
## and multiple years. Demonstrate with rarefaction curves? 
## Should we do something with mark-recap data to get at abundance? there are
## several R packages 

## Beta diversity... ideas? 

## Compare with IUCN range maps

## 

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
# and exports those data. The API portion of the script is based on
# QDR's neon_api_grad_lab_rawcode.R available at: https://github.com/NEON-biodiversity/teaching/tree/master/grad_lab

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

#set working directory
google_drive <- 'C:/Users/Q/google_drive/NEON_EAGER' # if you are Q
google_drive <- '/Volumes/GoogleDrive/My Drive/Research/ScalingUp/NEON_EAGER' # if you are Phoebe
setwd(file.path(google_drive, "Manuscript4_NEON_Organisms")) # GD location
#setwd("/Volumes/neon/final_data/richness") # HPCC location
data_path <- '/mnt/research/neon/raw_data/organismal_data_june2018'
tax_path <- '/mnt/research/neon/raw_data/taxonomy_lists_june2018'
fig_path <- '/mnt/research/neon/MS4_NEONOrganisms/figs'
export_path <- '/mnt/research/neon/final_data/richness'

#Install/load packages
for (package in c("ggplot2", "lme4", "dplyr", "purrr", "reshape2", "lubridate", "iNEXT")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
             axis.text.y = element_text(size = 10),
             strip.background = element_blank())

## Code below from https://github.com/NEON-biodiversity/teaching/tree/master/grad_lab/neon_api_grad_lab_rawcode.R

# -------------------------------------------------------------------------------------------------------------------------

# ----------------------------------------
# Load all organismal data and do QC on it
# ----------------------------------------

# Added by QDR, 19 June 2018
# Modified by QDR, 20 June 2018: add other taxa besides mammals

# Read in data from each taxon
# Note: data are pulled with the R script on github/sydnerecord/NEON/data_extraction/datapull_neonapi.r
# The data for everything but mammals was pulled on 20 June 2018

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
richness_cumulative <- function(dat, column = 'taxonID') {
  dat %>%
    rename(sp = !!column) %>%
    select(siteID, year, sp) %>%
    group_by(siteID) %>%
    do(cbind(year = min(.$year):max(.$year),
             richness = map_int(min(.$year):max(.$year), function(yr) length(unique(.$sp[.$year <= yr]))),
             map_dfr(min(.$year):max(.$year), function(yr) estimator_chao1(.$sp[.$year <= yr])),
             map_dfr(min(.$year):max(.$year), function(yr) estimator_asymp(.$sp[.$year <= yr]))
    ))
}

# Function to reshape output of richness_cumulative for plotting with ggplot2

richness_shapeplotdat <- function(rich) {
  rich %>%
    melt(id.vars = c('siteID', 'year')) %>%
    mutate(type = case_when(grepl('chao1', variable) ~ 'chao1',
                            grepl('asymp', variable) ~ 'asymp',
                            TRUE ~ 'observed'),
           stat = case_when(grepl('var|stderr', variable) ~ 'var',
                            grepl('min', variable) ~ 'CImin',
                            grepl('max', variable) ~ 'CImax',
                            TRUE ~ 'estimate')) %>%
    dcast(siteID + year + type ~ stat) %>%
    group_by(siteID) %>%
    mutate(n_year = length(unique(year))) %>%
    ungroup
}

# Function to make a plot of cumulative richness by site and year

richness_plot <- function(dat, min_n_years, title, legend_pos, y_max) {
  pd <- position_dodge(width = 0.2)
  dat %>%
    filter(n_year >= min_n_years) %>%
  ggplot(aes(x = year, color = type)) +
    facet_wrap(~ siteID) +
    geom_errorbar(aes(ymin = CImin, ymax = CImax), width = 0.5, position = pd) +
    geom_line(aes(y = estimate), position = pd) +
    geom_point(aes(y = estimate), position = pd) +
    ggtitle(title) +
    scale_y_continuous(expand = c(0,0), name = 'richness') + 
    scale_x_continuous(breaks=min(dat$year):max(dat$year)) +
    theme(legend.position = legend_pos) +
    coord_cartesian(ylim = c(0, y_max))
}

# ----------------------------------------------------------
# Run richness estimators for all taxa by site and year
# ----------------------------------------------------------

# Added by QDR, 19 June 2018
# Modified by QDR, 20 June 2018: Add other taxa

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

# Make a plot of the cumulative richness estimates through the years

# Reshape data for better plotting
# Only plot sites that have at least 3 years of samples
# For some taxa, might need to do fewer years
p_mam <- mammal_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative small mammal richness', legend_pos = c(0.9, 0.05), y_max = 25)
p_bird <- bird_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative bird richness', legend_pos = 'bottom', y_max = 175)
p_aqplant <- aq_plant_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative aquatic plant richness', legend_pos = c(0.9, 0.05), y_max = 50)
p_macroinv <- macroinvert_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative benthic macroinvertebrate richness', legend_pos = c(0.9, 0.05), y_max = 300)
p_fish <- fish_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 2, title = 'Cumulative fish richness', legend_pos = c(0.9, 0.05), y_max = 40)
p_mosq <- mosq_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative mosquito richness', legend_pos = c(0.9, 0.05), y_max = 75)
p_plant <- plant_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative terrestrial plant richness', legend_pos = c(0.9, 0.05), y_max = 1000)
p_tree <- tree_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative tree & woody plant richness', legend_pos = c(0.9, 0.05), y_max = 100)
p_beetle <- beetle_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative ground beetle richness', legend_pos = c(0.9, 0.05), y_max = 200)
p_zoop <- zoop_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative zooplankton richness', legend_pos = c(0.9, 0.05), y_max = 100)
p_phytop <- phytop_richness_cumulative %>%
  richness_shapeplotdat() %>%
  richness_plot(min_n_years = 3, title = 'Cumulative phytoplankton richness', legend_pos = c(0.9, 0.05), y_max = 250)


pdf(file.path(fig_path, 'cumulative_richness_alltaxa.pdf'), height = 7, width = 9)
  p_mam; p_bird; p_aqplant; p_macroinv; p_fish; p_mosq; p_plant; p_tree; p_beetle; p_zoop; p_phytop;
dev.off()


# Export richness data by taxonomic group
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

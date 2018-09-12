# Set file paths
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#Close graphics devices
graphics.off()

#Install/load packages
for (package in c("lme4", "plyr", "dplyr", "purrr", "raster", "reshape2", "lubridate", "ggplot2", "lmerTest", "standardize", "car","multcomp")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

#You need to load all the workspace from the prep to be able to do any modeling.
load("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\neon_all_site_prep.RData")

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
library(psych)
all_plant_HFTOOL <- subset(all_plant, siteID==c('TOOL','HARV'))
pairs.panels(all_plant[c(3:10,14:15)]) # Generally you want to not include highly correlated (> 0.7 or <-0.7) predictors
#based on the correlation plots, you should include temp, nlcd, slpasp, roads_dist, Severe_dist 
# this is a much reduced model, so you could then just fit a random effects model and avoid the model selection
# random effects of plot nested within site accounts for the variance more appropriately
# furthermore model selection is a statistical nightmare with models including random effects - statisticians bascially have bar fights over this kind of stuff

# The predictor variables should be standardized, so that they are on similar scales before fitting the model.
sobj <- standardize(richness ~ nlcdCls + slpAspc + roads_dist + severe_dist + (1|siteID),
                    all_plant_HFTOOL)

# run mixed effects model on standardized variables
mod <- lme(sobj$formula, sobj$data) # model gives convergence error because it needs more data. run this if you add in Konza

# get anova table for fixed effects
anova(mod)

# get anova table for random effect of site
ranova(mod)

# Perform posthoc comparison to see what categories of NLCD are significantly different from one another

posthoc = glht(mod,
               
               linfct = mcp(nlcdCls="Tukey"))

mcs = summary(posthoc,
              test=adjusted("single-step"))

# look at outputs in tabluar form
mcs

# this output gives you letter indicating if categories are different from one another that can be used to add to a graph
# probably more useful thaan the straight outputs from the mcs object
cld(mcs,
    level=0.05,
    decreasing=TRUE)





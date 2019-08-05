### Load packages
library(tidyverse)
library(FD)
library(codyn)

##### Data file and cleaning
traits <- read.csv('Trait_data.csv', na.strings = "")
row.names(traits) <- traits$Code

sites <- read.csv('Raw_site_data.csv')

##### Split traits between effect traits and response traits
effectsT <- c("Invasive", "Growth.F", "Life.Hist", "Raunkiaer", 
              "Leaf.Size", "Height", "Growth.Season", "Growth.Length", 
              "Evergreen", "Nfix", "Palat", "Defence", "G.Rate")
responseT <- c("Palat", "Defence", "G.Rate", "Hermaphrodite", 
               "Clonal", "Wind", "Insect", "Animal", 
               "Self", "Wind.1", "Water", "Animal.1", "Explosive", 
               "None", "Precip.Env", "Drought", "Shade", "Root.D", "Seed.W")

## Separate response traits and create trait weight vector for pollination syndrome and dispersal
resp <- traits %>% subset(select = responseT)
trW <- rep(1, ncol(resp))
trW[colnames(resp) %in% c("Wind", "Insect", "Animal", "Self")] <- 0.25
trW[colnames(resp) %in% c("Wind.1", "Water", "Animal.1", "Explosive", "None")] <- 0.2

##### Create function to calculate response diversity
rdcalc <- function(sp, trait_mat, weighting) {
  trait_mat$Sp <- row.names(trait_mat)
  
  subtrait <- trait_mat %>%
    filter(Sp %in% sp) %>%
    select(-Sp)
  
  respgr <- dbFD(subtrait, corr = "cailliez", w = weighting,
                 calc.FRic = FALSE, calc.FGR = FALSE, calc.FDiv = FALSE,
                 clust.type = "ward", calc.CWM = FALSE)
  return(respgr$FDis)
}

#### Calculate response diversity for all species recorded in a site
siteL <- gather(sites, Species, value, -Site, -Year) %>% 
  subset(value > 0) %>% 
  subset(select = -value) %>% 
  arrange(Site, Year)

gomets <- siteL %>% 
  group_by(Site, Year) %>%
  summarize(RD = rdcalc(Species, resp, trW)) 

#### Calculate community turnover
siteL$Abun <- 1

st <- c(); turn <- c()
for(s in unique(siteL$Site)) {
  st <- c(st, s)
  turn <- c(turn, turnover(siteL[siteL$Site == s, ], "Year", "Species", "Abun")$total)
}

tov <- data.frame(Turn = turn,
                  Site = unique(siteL$Site))

#### Combine into site matrix
site_data <- gomets %>%
  left_join(tov)

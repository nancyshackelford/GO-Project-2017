### Load packages
library(tidyverse)
library(FD)

##### Data file and cleaning
traits <- read.csv('Trait_data.csv', na.strings = "")
row.names(traits) <- traits$Code

sites <- read.csv('Raw_site_data.csv')

##### Split traits between effect traits and response traits
effectsT <- c("Invasive", "Growth.F", "Life.Hist", "Raunkiaer", 
              "Leaf.Size", "Height", "Growth.Season", "Growth.Length", 
              "Evergreen", "Nfix", "Palat", "Defence", "G.Rate")
responseT <- c("Palat", "Defence", "G.Rate", "Hermaphrodite", 
               "Clonal", "Prop.Long", "Wind", "Insect", "Animal", 
               "Self", "Wind.1", "Water", "Animal.1", "Explosive", 
               "None", "Precip.Env", "Drought", "Shade", "Root.D", "Seed.W")

## Separate effect traits
eff <- traits %>% subset(select = effectsT)
rownames(eff) <- paste(traits$Code)

##### Create functional group designations based on effect traits
all <- dbFD(eff, corr = "cailliez", calc.FRic = FALSE, ord = 'metric',
            calc.FGR = TRUE, clust.type = "ward", calc.CWM = FALSE) #'g' for groups and 31 were chosen
grpsAll <- data.frame(Species = names(all$spfgr), Group = as.vector(all$spfgr))
grpsAll <- cbind(grpsAll, eff)[ , -1]
grpsAll$Species <- rownames(grpsAll)

#### Calculate functional redundancy for group in site
siteL <- gather(sites, Species, value, -Site, -Year) %>% 
  subset(value > 0) %>% 
  subset(select = -value) %>% 
  arrange(Site, Year) %>% left_join(grpsAll)

fr_grp <- siteL %>% 
  group_by(Site, Year, Group) %>%
  summarize(FR = n())

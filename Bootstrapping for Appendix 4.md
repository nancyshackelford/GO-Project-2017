# Bootstrapping to explore roll of species additions/losses in functional redundancy changes

## *Conservation Science and Practice,* in press

NAS 04-08-2019

### Script description --

Given that the change in functional redundancy (FR) is calculated as the change in species richness within functional groups, we investigated the difference between true changes in FR, as reflected in group-specific species richness changes, and the random addition and subtraction of species with functional groups ignored. To do so, this code is a series of simulations to examine the effect of gains or losses in species richness, using species randomly drawn from the available pool. (i.e., ‘null’ model) and compared it with our real data results. The following steps were taken:
>>1.  The number of species gained or lost per site was extracted from the dataset
>>2.  For species additions, this number was then used to randomly select from the regional species pool to “recruit” to the site in the second time point
>>3.  For species losses, the number was then used to randomly select from the site species pool to “lose” in the second time point
>>4.  Change in FR within groups based on this random process was calculated
>>5.  Change in FR was modeled against all predictors included in original model
>>6.  Coefficients and p-values for all predictors were extracted
This process was repeated 1,000 times, and the distribution of the coefficients and p-values in the randomly generated FR changes were compared to real data outputs 

### Load packages and prepare data
```
library(tidyverse)
library(nlme)
library(gridExtra)
library(extrafont)

##### Data file and cleaning
data <- read.csv("Site_data.csv")
colnames(data)[1] <- "Site" 
data <- data %>%
  mutate(AreaS = (Area_07 - mean(Area_07)) / sd(Area_07),
         NIS_P07 = NIS_07 / Rich_07) %>%
  mutate(NISS = (NIS_P07 - mean(NIS_P07)) / sd(NIS_P07))
rownames(data) <- data$Site

data_s <- read.csv('Group_data.csv')
data_s[is.na(data_s)] <- 0
data_s$Invasive <- factor(data_s$Invasive, 
                          levels = c("None", "Low", "Med", "High"))
data_s$Invasive <- as.numeric(data_s$Invasive)
data_s$Grp <- as.factor(data_s$Grp)
data_s <- data_s %>%
  filter(FR.07 > 0) %>%
  mutate(FRC = (FR.17 - FR.07) / FR.07) %>%
  mutate(AreaS = (Area.2007 - mean(Area.2007)) / sd(Area.2007),
         ConnS = (Conn - mean(Conn)) / sd(Conn),
         RdS = (RdDens.1KM - mean(RdDens.1KM)) / sd(RdDens.1KM))
```

### Create models of real data for comparison to random results
```
#### Create model of total species richness
sp_mod <- lm(RichC ~ Inv_C * RdS + ConnS + AreaS, 
             data = data)

#### Create model of functional redundancy within groups
fr1 <- lme(FRC ~ Invasive * RdS + ConnS + Area.Change + Grp, 
           random = ~1|Site, data = data_s)
```

#### Simulate changes in functional redundancy based on random species additions and losses
```
### Reorganize group-level data to slot into species model structure
simul_set <- data_s %>%
  select(Site, Grp, FR.07, Invasive, RdS, ConnS, Area.Change) %>%
  mutate(Site = fct_recode(Site, "SEYMOUR-S" = "SEYMOUR"))

### Calculate changes in species richness per site
n_change <- data %>%
  mutate(Addition = Rich_17 - Rich_07) %>%
  select(Code, Addition)

### Set up empty coefficient matrix and add coefficients from real data model
sim_coef <- data.frame(matrix(NA, nrow = 1, ncol = ncol(coef(fr1)) - 1))
sim_coef[1, ] <- as.numeric(coef(fr1)[1, 2:ncol(coef(fr1))])
colnames(sim_coef) <- colnames(coef(fr1))[2:ncol(coef(fr1))]

### Set up empty p-value matrix and add p-values from real data model
sim_p <- data.frame(matrix(NA, nrow = 1, ncol = ncol(coef(fr1)) - 1))
sim_p[1, ] <- summary(fr1)$tTable[2:nrow(summary(fr1)$tTable), 5]
colnames(sim_p) <- colnames(coef(fr1))[2:ncol(coef(fr1))]

### Run simulations, tracking coefficients and p-values for each model based on randomly assigned species shifts
for(i in 1:1000) {
  
  simul_set$FRC <- 0
  for(site in unique(simul_set$Site)) {
    n <- n_change$Addition[n_change$Code == as.character(site)]
    sub <- simul_set[simul_set$Site == site, ]
    frc <- rep(0, nrow(sub))
    
    if(n > 0) {
      r_add <- sample(unique(sub$Grp), n, replace = TRUE)
      
      for(add_one in r_add) {
        frc[sub$Grp == add_one] <-
          frc[sub$Grp == add_one] + 1
      }  
    }
    
    if(n < 0) {
      r_add <- sample(unique(sub$Grp), -n, replace = TRUE)
      
      for(add_one in r_add) {
        frc[sub$Grp == add_one] <-
          frc[sub$Grp == add_one] - 1
      }  
    }
    
    simul_set$FRC[simul_set$Site == site] <- frc
    
  }
  
  mod1 <- lme(FRC ~ Invasive * RdS + ConnS + Area.Change + Grp, 
              random = ~1|Site, data = simul_set)
  sim_coef[i + 1, ] <- as.numeric(coef(mod1)[1, 2:ncol(coef(mod1))])
  sim_p[i + 1, ] <- summary(mod1)$tTable[2:nrow(summary(mod1)$tTable), 5]
}
```

#### Create plots of coefficient and p-values for each predictor
```
### Invasion control
p1 <- 
  ggplot(sim_coef) +
  geom_histogram(aes(x = Invasive)) +
  geom_point(aes(x = Invasive[1], y = 1), 
             color = "red", size = 6) +
  xlab("Coefficient: Management") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

p2 <- 
  ggplot(sim_p) +
  geom_histogram(aes(x = Invasive)) +
  geom_point(aes(x = Invasive[1], y = 1), 
             color = "red", size = 6) +
  xlab("P-value: Management") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")


### Road density
p3 <- 
  ggplot(sim_coef) +
  geom_histogram(aes(x = RdS)) +
  geom_point(aes(x = RdS[1], y = 1), 
             color = "red", size = 6) +
  xlab("Coefficient: Road density (Std)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

p4 <- 
  ggplot(sim_p) +
  geom_histogram(aes(x = RdS)) +
  geom_point(aes(x = RdS[1], y = 1), 
             color = "red", size = 6) +
  xlab("P-value: Road density (Std)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

### Connectivity
p5 <- 
  ggplot(sim_coef) +
  geom_histogram(aes(x = ConnS)) +
  geom_point(aes(x = ConnS[1], y = 1), 
             color = "red", size = 6) +
  xlab("Coefficient: Connectivity (Std)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

p6 <- 
  ggplot(sim_p) +
  geom_histogram(aes(x = ConnS)) +
  geom_point(aes(x = ConnS[1], y = 1), 
             color = "red", size = 6) +
  xlab("P-value: Connectivity (Std)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

### Area lost between 2007 and 2017
p7 <- 
  ggplot(sim_coef) +
  geom_histogram(aes(x = Area.Change)) +
  geom_point(aes(x = Area.Change[1], y = 1), 
             color = "red", size = 6) +
  xlab("Coefficient: Area Lost (Prop)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

p8 <- 
  ggplot(sim_p) +
  geom_histogram(aes(x = Area.Change)) +
  geom_point(aes(x = Area.Change[1], y = 1), 
             color = "red", size = 6) +
  xlab("P-value: Area Lost (Prop)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16,
                                    family = "Garamond",
                                    face = "bold"),
        axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16,
                                   family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none")

grid.arrange(p1, p2,
             p3, p4,
             p5, p6,
             p7, p8,
             ncol = 2)
```

#### Create plots of results for each functional group
```
grp_names <- c("Early blooming, woody, phanerophytes",
               "Early blooming, defensive hemicryptophyte 
               herbs, mid to low palatability",
               "Hemicryptophyte rosettes, mid palatability",
               "Geophyte or hemicryptophyte, low to mid 
               palatability, mid growth rate",
               "Rapid growing, geophyte or hemicryptophyte",
               "Early blooming annual herbs, mid to rapid 
               growth rate",
               "Early blooming, defensive geophytes, rapid 
               growth rate",
               "Geophyte or phanerophyte, woody shrubs, mid 
               growth rate",
               "Small hemicryptophytes, rapid growth rate",
               "Early blooming, evergreen, polycarpic herbs, 
               low palatability",
               "Early blooming, hemicryptophytes, slow 
               growth rate",
               "Annuals, high palatability, rapid growth rate",
               "Annuals, mid palatability, rapid growth rate",
               "Defensive annuals, mid to low palatability",
               "Geophytes, slow growth rate",
               "Geophytes, high palatability",
               "Annuals, mid to low palatability, rapid 
               growth rate",
               "Early blooming, woody, mid to rapid growth rate",
               "Hemicryptophyte herbs, mid to rapid growth rate, 
               high palatability",
               "Geo/hemicryptophytes, low palatability",
               "Geo/hemicryptophytes, rapid growth rate",
               "Nitrogen fixing annuals",
               "ELR polycarpic herbs, mid to slow growth rate, 
               mid to low palatability",
               "Defensive woody evergreens")

for(i in 5:(ncol(sim_coef) - 1)) {
  for_plot <- data.frame(Plot = sim_coef[ , i])
  
  assign(paste("p", (i - 4), sep = ""),
         ggplot(for_plot) +
           geom_histogram(aes(x = Plot)) +
           geom_point(aes(x = Plot[1], y = 1),
                      color = "red", size = 6) +
           xlab(grp_names[i - 4]) +
           theme_bw() +
           theme(axis.title.x = element_text(size = 16,
                                             family = "Garamond",
                                             face = "bold"),
                 axis.text.x = element_text(size = 16,
                                            family = "Garamond"),
                 axis.title.y = element_blank(),
                 axis.text.y = element_text(size = 16,
                                            family = "Garamond")) +
           theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(),
                 legend.position = "none"))
}

grid.arrange(p1, p2, p3, p4,
             p5, p6, p7, p8,
             p9, p10, p11, p12,
             p13, p14, p15, p16,
             p17, p18, p19, p20,
             p21, p22, p23, p24,
             ncol = 4)

for(i in 5:(ncol(sim_p) - 1)) {
  for_plot <- data.frame(Plot = sim_p[ , i])
  
  assign(paste("p", (i - 4), sep = ""),
         ggplot(for_plot) +
           geom_histogram(aes(x = Plot)) +
           geom_point(aes(x = Plot[1], y = 1),
                      color = "red", size = 6) +
           xlab(grp_names[i - 4]) +
           theme_bw() +
           theme(axis.title.x = element_text(size = 16,
                                             family = "Garamond",
                                             face = "bold"),
                 axis.text.x = element_text(size = 16,
                                            family = "Garamond"),
                 axis.title.y = element_blank(),
                 axis.text.y = element_text(size = 16,
                                            family = "Garamond")) +
           theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(),
                 legend.position = "none"))
}

grid.arrange(p1, p2, p3, p4,
             p5, p6, p7, p8,
             p9, p10, p11, p12,
             p13, p14, p15, p16,
             p17, p18, p19, p20,
             p21, p22, p23, p24,
             ncol = 4)
```

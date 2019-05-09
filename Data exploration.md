# Garry oak meadow data exploration

## *Conservation Science and Practice, in revision*

NAS 09-05-2019

### Script description --

Create exploratory plots of response variables and predictors of interest in Garry oak meadow management research. Data was collected in the spring and summer of both 2007 and 2017, by Dr. Joseph Bennett (2007) and by Dr. Nancy Shackelford (2017) on Saanich Penninsula  in British Columbia. See repository README (currently in prep) and publication (currently in revision) for further details.

```
### Load packages
library(tidyverse)
library(gridExtra)
library(extrafont)

##### Data file and cleaning
### Data for site-level results
data <- read.csv("Simple_data.csv")
colnames(data)[1] <- "Site" 
data <- data %>%
  mutate(AreaS = (Area_07 - mean(Area_07)) / sd(Area_07)) # Standardize area
rownames(data) <- data$Site

### Data for group-level results (used in functional redundancy only)
data_s <- read.csv('For_analysis_S2.csv')
data_s[is.na(data_s)] <- 0
data_s$Invasive <- factor(data_s$Invasive, 
                          levels = c("None", "Low", "Med", "High")) # Correct ranking
data_s$Invasive <- as.numeric(data_s$Invasive) # To ordinal
data_s$Grp <- as.factor(data_s$Grp)
data_s <- data_s %>%
  filter(FR.09 > 0) %>%
  mutate(FRC = (FR.17 - FR.09) / FR.09) %>%
  mutate(AreaS = (Area.2007 - mean(Area.2007)) / sd(Area.2007),
         ConnS = (Conn - mean(Conn)) / sd(Conn),
         RdS = (RdDens.1KM - mean(RdDens.1KM)) / sd(RdDens.1KM)) # Drop empty functional groups and standardize predictors
```

```
#### Area lost plots
### Histogram
data$myfill <- data$AreaC
data$myfill[data$myfill > 0.25 & data$myfill <= 0.3] <- 7
data$myfill[data$myfill > 0.2 & data$myfill <= 0.25] <- 6
data$myfill[data$myfill > 0.15 & data$myfill <= 0.2] <- 5
data$myfill[data$myfill > 0.1 & data$myfill <= 0.15] <- 4
data$myfill[data$myfill > 0.05 & data$myfill <= 0.1] <- 3
data$myfill[data$myfill >= 0 & data$myfill <= 0.05] <- 2

ggplot(data) +
  geom_histogram(aes(x = AreaC, fill = factor(myfill)), 
                 breaks = seq(0,0.3,0.05)) +
  xlab("Proportional area lost") +
  scale_fill_brewer(palette = "Greens") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(size = 16,
                                   family = "Garamond"),
        axis.title.x = element_text(size = 20, face = "bold",
                                    family = "Garamond")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position = "none")
```

![Area lost](/Images/AreaLostHist.tiff?raw=true)

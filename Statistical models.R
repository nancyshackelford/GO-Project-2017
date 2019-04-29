### Libraries
library(MASS)
library(tidyverse)
library(gridExtra)
library(betareg)
library(MuMIn)
library(nlme)
library(lme4)

##### Data file and cleaning
### Data for site-level results
data <- read.csv("Simple_data.csv")
colnames(data)[1] <- "Site"
data <- data %>%
  mutate(AreaS = (Area_07 - mean(Area_07)) / sd(Area_07))
rownames(data) <- data$Site

### Data for group-level results (used in functional redundancy only)
data_s <- read.csv('For_analysis_S2.csv')
data_s[is.na(data_s)] <- 0
data_s$Invasive <- factor(data_s$Invasive, 
                          levels = c("None", "Low", "Med", "High"))
data_s$Invasive <- as.numeric(data_s$Invasive)
data_s$Grp <- as.factor(data_s$Grp)
data_s <- data_s %>%
  filter(FR.09 > 0) %>%
  mutate(FRC = (FR.17 - FR.09) / FR.09) %>%
  mutate(AreaS = (Area.2007 - mean(Area.2007)) / sd(Area.2007),
         ConnS = (Conn - mean(Conn)) / sd(Conn),
         RdS = (RdDens.1KM - mean(RdDens.1KM)) / sd(RdDens.1KM))

##### Statistical models
### Area lost
ac1 <- lm(AreaC ~ Inv_C * RdS + AreaS, 
          data = data[-which(data$Site == "Summit Park"), ]) # Exclude outlier
#plot(ac1)
summary(ac1)

### Native species
sp2 <- lm(NatC ~ Inv_C * RdS + ConnS + AreaS, 
          data = data)
#plot(sp2)
summary(sp2)

### Turnover
t1 <- glm(Turnover ~ Inv_C * RdS + ConnS + AreaS, data = data, family = Gamma) 
#plot(t1)
summary(t1)

### Response diversity
rd1 <- lm(RDC ~ Inv_C * RdS + ConnS + AreaS, data = data)
#plot(rd1)
summary(rd1)

### Functional redundancy
fr1 <- lme(FRC ~ Invasive * RdS + ConnS + AreaS + Grp, 
           random = ~1|Site, data = data_s)
#plot(fr1)
summary(fr1)


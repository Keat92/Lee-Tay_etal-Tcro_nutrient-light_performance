# Setting directory
# setwd("path/to/your/workingdirectory")

# Part 1: Survival
# Package used
library(tidyverse)
library(survival)#Kaplan meier analysis
library(readxl)
library(survminer)#survival plot
library(ggsci)#for publication ready color palette
library(sjPlot)#for stats result output in table format
library(car)#Anova test
library(ggsurvfit)#plotting survival curve

# Load dataset
# For Kaplan-Meier analysis, dataset has to reformat. *Surv* function of `survival` packages create survival object the two important variable, *time* and *event*, *event* read only status indicator, 0=alive, 1=dead, or T/F (TRUE=death) or 1/2(2=death)
data1 <- read_excel("C:/Users/yukin/Desktop/MyWork2025/M6. Tcro_CEA/2. DataAnalysis/Tcro_CEA/analysis_data1.xlsx", sheet = "survival")#for survival trajectories
data2 <- read_excel("C:/Users/yukin/Desktop/MyWork2025/M6. Tcro_CEA/2. DataAnalysis/Tcro_CEA/analysis_data1.xlsx", sheet = "end_mortality")#for logistic  regression
data3 <- read_excel("C:/Users/yukin/Desktop/MyWork2025/M6. Tcro_CEA/2. DataAnalysis/Tcro_CEA/analysis_data1.xlsx", sheet = "mortality")#for survival summarize

# Summarize survivorship
data3 <- data3 %>%
  mutate(survive = (end/20)*100)

data3 %>% group_by(light, nutrient) %>%
  summarise(mean = mean(survive))

data3 %>% group_by(nutrient) %>%
  summarise(mean = mean(survive))

# Logistic regression (binomial GLM)
# To test effects of light and nutrient (3 light level and 2 nutrient) on final proportion of survivors after 178 days of treatment
# binomial glm for interaction of PAR and nutrient on survivality
logit_reg <- glm(end ~ light*nutrient, data = data2, family = binomial(link = "logit"))

# Anova test
Anova(logit_reg, type =3)

# Construct regression result table 
tab_model(logit_reg)

# Kaplan-Meier plot and Log-Rank test
# Pivot longer for Kaplan-meier analysis
data1_long <- data1 %>% 
  pivot_longer(cols =starts_with("D"), names_to = "Day", names_prefix = "D",
               values_to = "status", values_drop_na =TRUE)

data1_long$Day <- as.numeric(data1_long$Day)
data1_long$light <- factor(data1_long$light)
data1_long$nutrient <- factor(data1_long$nutrient)

# On survival trajectories over time by treatment
# built survival object and fit survival curve
km_fit <- survfit(Surv(Day, status) ~ light+nutrient, data=data1_long)
summary(km_fit)

# FigS2: Kaplan-Meier survival curve
figs2 <- ggsurvfit(km_fit) + 
    scale_color_frontiers()+
    theme_pubr()

# Pairwise log-rank test on combination of treatment
(km_diff_combine <- pairwise_survdiff(Surv(Day, status) ~ light+nutrient, data=data1_long, p.adjust.method = "bonferroni"))

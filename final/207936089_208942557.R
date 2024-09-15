
library(readxl)
library(tidyverse)
library(psych)
library(easystats)
library(lavaan)

# Data --------------------------------------------------------------------

data <- read_excel("C:/Users/susana/Desktop/DSS and ASI dataset.xlsx") |> 
  slice(-1) |> 
  mutate(across(everything(), as.numeric)) |>
  filter(AGE >= 18)|>
  filter(GNDR != 3) #ther is only one...

glimpse(data)

# Anxiety Sensitivity is based on 3 indicators -each of them is calculated by the sum 
# of scores in the relevant items

data <- data %>%
  mutate(
    ASI_PC = rowSums(across(c("ASI_4", "ASI_12", "ASI_8", "ASI_7", "ASI_15", "ASI_3"))),
    ASI_CC = rowSums(across(c("ASI_14", "ASI_18", "ASI_10", "ASI_16", "ASI_2", "ASI_5"))),
    ASI_SC = rowSums(across(c("ASI_9", "ASI_6", "ASI_11", "ASI_13", "ASI_17", "ASI_1")))
  )


# Dissociation Sensitivity for the assignment is also based on 3 indicators -each one of them is calculated by the sum 
# of scores in the relevant items 

data <- data %>%
  mutate(
    DSS_Absor = rowSums(across(c("DSS_2", "DSS_14", "DSS_15", "DSS_18", "DSS_17", "DSS_19"))),
    DSS_Dep_Der = rowSums(across(c("DSS_7", "DSS_11", "DSS_12", "DSS_13", "DSS_25", "DSS_26"))),
    DSS_Amns = rowSums(across(c("DSS_3", "DSS_4", "DSS_5", "DSS_8", "DSS_23", "DSS_24")))
  )

# Part 1 - Measurement Model for Anxiety sensitivity and Dissociation sensitivity --------------

mod_cfa <- ' # Define the CFA model for Anxiety Sensitivity (ASI) and Dissociation Sensitivity (DSS)
    
  ## latent variable definitions (CFA)
  ASI =~ ASI_PC + ASI_CC + ASI_SC 
  DSS =~ DSS_Absor + DSS_Dep_Der + DSS_Amns 
  
  ## covariences - It will be done automatically, but we will indicate it for the assignment
  ASI ~~ DSS

'

# Fit the CFA model to the data
fit_cfa <- cfa(mod_cfa, data = data)

# Summarize the CFA model fit
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

## ASI and DSS factors have significant loadings, showing good measurement of their respective constructs.
##Significant positive and moderate relationship between ASI and DSS (standardized covariance = 0.297).

## Measure of fit - it looks like the models fit is not bad but also not good
fitMeasures(fit_cfa, output = "text",
            fit.measures = c("nfi","nnfi","tli","cfi","gfi","rmsea"))

#comparing to the just identified model
anova(fit_cfa)
#anova also not good!

# is there a cov that could make the fit better? the bigger mi the better fit we will get
modificationindices(fit_cfa, sort. = TRUE, maximum.number = 5)

# the reliability of the factors - we will want to check the omega - ASI(0.7762918) and DSS (0.8125657).
reliability_cfa <- semTools::reliability(fit_cfa)
reliability_cfa

# Visualize the CFA model 

library(tidySEM) ## using tidySEM to create a visual representation of the CFA model.
library(dplyr)

# Defining the layout for the path diagram

lay <- get_layout(
  "ASI_PC", "ASI_CC", "ASI_SC", NA,  "DSS_Absor", "DSS_Dep_Der", "DSS_Amns", 
  NA,         "ASI",    NA,      NA, NA,           "DSS", NA, NA,
  rows = 2,
  cols=3
)

# Prepare and plot the graph
g <- prepare_graph(fit_cfa, layout = lay, angle = 90)

# Adjust edge labels and positions
edges(g) <- edges(g) %>% 
  mutate(
    label = est_std,
    label_location = 0.4 # Adjusting label position to avoid overlap
  )

plot(g)


# Part 2.A  first hypothesis  ---------------------------
# testing the hypothesis that ASI predicts DSS
# we will define 2 models - one restricted where the slope is zero (DSS ~ 0*ASI)
# and one free where there is no assumption about the slope (DSS ~ ASI)

mod_2 <- '
  ## Latent variable definitions (CFA)
  ASI =~ ASI_PC + ASI_CC + ASI_SC
  DSS =~ DSS_Absor + DSS_Dep_Der + DSS_Amns

  ## Regressions
  DSS ~ ASI
'

mod_2_restrict <- '
  ## Latent variable definitions (CFA)
  ASI =~ ASI_PC + ASI_CC + ASI_SC
  DSS =~ DSS_Absor + DSS_Dep_Der + DSS_Amns

  ## Regressions
  DSS ~ 0*ASI
'

# Fit the models with sem function
fit_mod_2 <- sem(mod_2, data = data)
fit_mod_restrict <- sem(mod_2_restrict, data = data)

summary(fit_mod_2, fit.measures = TRUE, standardized = TRUE)
summary(fit_mod_restrict, fit.measures = TRUE, standardized = TRUE)

## Measure of models fit 
fitMeasures(fit_mod_2, output = "text",
            fit.measures = c("nfi","nnfi","tli","cfi","gfi","rmsea"))
fitMeasures(fit_mod_restrict, output = "text",
            fit.measures = c("nfi","nnfi","tli","cfi","gfi","rmsea"))

anova(fit_mod_2, fit_mod_restrict)
#the assumption that the path between DSS and ASI is zero damaged the fit
# the results indicates that there is a connection and that ASI predicts DSS.

# if we will want to show 2.A in graph but its the same as the CFA just with a reg-path instead of covarience ----------------------

## graph the Structural model
lay1 <- get_layout(
  "ASI_PC", "ASI_CC", "ASI_SC", NA,  "DSS_Absor", "DSS_Dep_Der", "DSS_Amns", 
  NA,         "ASI",    NA,      NA, NA,           "DSS", NA, NA,
  rows = 2,
  cols=3
)

graph_sem(fit_mod_2,
          edges = get_edges(fit_mod_2, label = "est_std"),
          layout = lay1, angle = 90)




# Part 2.B ---------------------------
#Multiple group analysis to check the assumption DSS ~ ASI differ by Gender

unique(data$GNDR)
#2 - woman
#1 - man

#building the model
mod_1 <- '
  ## latent variable definitions (CFA)
  ASI =~ ASI_PC + ASI_CC + ASI_SC
  DSS =~ DSS_Absor + DSS_Dep_Der + DSS_Amns

  ## Regressions
  DSS ~ ASI 
'

# Fit the model with the `group` argument.
fit_groups <- sem(mod_1, data = data, 
                  group = "GNDR") 

# lets check the goodnes of fit
fitMeasures(fit_groups, output = "text",
            fit.measures = c("nfi","nnfi","tli","cfi","gfi","rmsea"))
anova(fit_groups)


#visualizing the differences in paths

lay3 <- get_layout(
  NA,       NA,        NA,       "ASI_PC",
  "GNDR",   NA,        "ASI",    "ASI_CC",
  NA,       NA,        NA,       "ASI_SC",
  NA,       "DSS",     NA,       NA,
  "DSS_Absor", "DSS_Dep_Der", "DSS_Amns", NA,
  rows = 5
)


graph_sem(fit_groups, 
          edges = get_edges(fit_groups, label = "est_std"),
          nodes = get_nodes(fit_groups, label = "name"),
          layout = lay3)


# Comparing parameters  
# Do men and woman differ on the `DSS ~~ ASI` parameter statistically?

mod_comp <- '
  ## latent variable definitions (CFA)
  ASI =~ ASI_PC + ASI_CC + ASI_SC
  DSS =~ DSS_Absor + DSS_Dep_Der + DSS_Amns

  ## Regressions
  DSS ~ c(cvW, cvM) *ASI 
  
  ## Computed estimates
  cv_diff := cvM - cvW
'
fit_groups2 <- sem(mod_comp, data = data, 
                  group = "GNDR")

summary(fit_groups2, standardize = TRUE)

#yes they do

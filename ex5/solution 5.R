
  
library(lavaan)
library(bayestestR)

#Going back to the STRUCTURAL model...


group_data <- read.csv("group_data.csv")

mod <- '
  ## latent variable definitions (CFA)
  Psypath =~  BAI + BDI
    Sleep =~  sleep1 + sleep2 + sleep3

  ## Regressions
  Psypath ~ Sleep + trauma

  ## Covariances
  trauma ~~ BAI
'

fit_groups <- sem(mod, data = group_data, 
                  group = "gender")



# 1. Fit a model constraining a group equality on the intercepts
#Compare it (test) to the unconstrained model.
#What do you make of the results?
  
  
fit_groupsFixIntercepts <- sem(mod, data = group_data, 
                               group.equal = "intercepts",
                               group = "gender")

anova(fit_groups, fit_groupsFixIntercepts)
bayestestR::bayesfactor_models(fit_groupsFixIntercepts, denominator = fit_groups)

#It seems like this constrain does not significantly hurt the model-fit, and the BF supports the constrained model over the unconstrained model.

#Where does this come into play?
  
  
parameterEstimates(fit_groupsFixIntercepts, standardized = TRUE, output = "text")


#It seems that the groups do not differ in the means/intercepts of the *observed* variables.

# 2. For the effect of Sleep on Psypath

## Compute the difference in slopes between the groups


mod_diff <- '
  ## latent variable definitions (CFA)
  Psypath =~  BAI + BDI
    Sleep =~  sleep1 + sleep2 + sleep3

  ## Regressions
  Psypath ~ c(slpW, slpM) * Sleep + trauma

  ## Covariances
  trauma ~~ BAI
  
  slope_diff := slpW - slpM
'

fit_groups_diff <- sem(mod_diff, data = group_data, 
                       group = "gender")

parameterestimates(fit_groups_diff, standardized = TRUE, output = "text")[c(6, 7, 30, 31, 49), ]

#Seems like the difference between the slopes is not significant. This means that *genders does not moderate the effect of Sleep on Psychopathology*.

## Fit a model with an equality on this parameter

#Compare to the unrestricted model.
#What do you make of the results?
  
  
mod_fix_sleep <- '
  ## latent variable definitions (CFA)
  Psypath =~  BAI + BDI
    Sleep =~  sleep1 + sleep2 + sleep3

  ## Regressions
  Psypath ~ c(slp, slp) * Sleep + trauma

  ## Covariances
  trauma ~~ BAI
'

fit_groups_fix_sleep <- sem(mod_fix_sleep, data = group_data, 
                            group = "gender")

anova(fit_groups, fit_groups_fix_sleep)
bayestestR::bayesfactor_models(fit_groups_fix_sleep, denominator = fit_groups)

#Same conclusion - the BF supports a lack of moderation effect!
  
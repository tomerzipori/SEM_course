
library(lavaan)
library(bayestestR)
library(tidySEM)
library(magrittr)

# 1. Look at "partner_fatigue.png".


partner_fatigue <- read.csv("partner_fatigue.csv")

head(partner_fatigue)
```

- fatigue: Emotional fatigue  
- anxiety: State anxiety  
- As measured in heterosexual partners (Males and Females)  

## What does the model imply, causally?  

The model implies that an individual's Emotional Fatigue is affected by their Anxiety, *as well as by their partner's Anxiety*, and the same for their partner's Emotional Fatigue.

(Here we're looking only at heterosexual couples.)

## Is the model saturated (aka. just identified)?

The model is saturated - each variable is connected to all other variabels. We can expect this model to have `df == 0`.

# 2. Prof. Geller hypothesis 

Prof. Geller hypothesizes that women do more emotional work, and so are more fatigued by their partners anxiety compared to how men are affected.

## A. Test this hypothesis by comparing the relevant paths.

We want to compare paths, so we will use modifiers to mark them, and then we will calculate (with `:=`) the difference between them:
  
  
 

library(lavaan)

APIM_mod <- "
  ## Regressions
  fatigue_M ~ anxiety_M + anxF2M * anxiety_F
  fatigue_F ~ anxiety_F + anxM2F * anxiety_M

  ## Cov
  fatigue_F ~~ fatigue_M
  anxiety_F ~~ anxiety_M

  ## Calculated estimates
  cross_diff := anxM2F - anxF2M
"

APIM_fit <- sem(APIM_mod, data = partner_fatigue)

parameterEstimates(APIM_fit, standardized = TRUE, output = "text")
```
Looks like Men affect Women (b=0.423, beta=0.386) stronger than Women affect Men (b=0.281, beta=0.289) - but the difference is not significant (z=1.349$, p=0.177).

## B. Test this hypothesis by adding / removing a model constraint (what is the opposite of Prof. Geller's hypothesis?)

The opposite of Prof. Gellers hypothesis is that they are equal (by naming both `anx`).


APIM_modH <- "
  ## Regressions
  fatigue_M ~ anxiety_M + anx * anxiety_F
  fatigue_F ~ anxiety_F + anx * anxiety_M

  ## Cov
  fatigue_F ~~ fatigue_M
  anxiety_F ~~ anxiety_M
"

APIM_fitH <- sem(APIM_modH, data = partner_fatigue)


### Asses model fit


fitMeasures(APIM_fitH, output = "text",
            fit.measures = c("nfi", "nnfi", "tli", "cfi", "gfi", "rmsea"))

Looks good!


### Compare to an unconstrained model. Is the hypothesis supported?  


anova(APIM_fitH, APIM_fit)
```
No significant difference - the constrained model is not significantly worse than the constrained model.

But also...


bayestestR::bayesfactor_models(APIM_fitH, denominator = APIM_fit)

The constrained model is 5.67 times more supported by the data compared to the unconstrained model!

Prof. Geller is wrong!



# 3. Plot the model


library(tidySEM)

lay <- get_layout(
  "anxiety_M", NA, "fatigue_M",
  NA,          NA, NA, 
  "anxiety_F", NA, "fatigue_F", 
  rows = 3
)

prepare_graph(APIM_fit, layout = lay, angle = 90) %>% 
  edit_edges({label = paste0(est_sig_std, "\n", confint_std)}) %>% 
  edit_edges({label_location = .3}) %>%
  edit_nodes({label = c("Female\nAnxiety", "Male\nAnxiety", "Female\nFatigue", "Male\nFatigue")}) %>% 
  plot





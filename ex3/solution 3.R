library(lavaan)
library(tidySEM)

# 1. Refit the model with the missing auto-correlations

We need to add the *autoregressions* (the first 3 MIs).

mod_meas_fix <- '
  ## latent variable definitions (CFA)
  HOME_t1 =~ accept_t1 + variety_t1 + acStim_t1
  HOME_t2 =~ accept_t2 + variety_t2 + acStim_t2
  
  ## covariances
  HOME_t1 ~~ HOME_t2 + adhd_t1 + adhd_t2
  HOME_t2 ~~ adhd_t1 + adhd_t2
  adhd_t1 ~~ adhd_t2
  
  ## Autoregressions
   accept_t1 ~~ accept_t2
  variety_t1 ~~ variety_t2
   acStim_t1 ~~ acStim_t2
  
  ## self-regression
  # Do not forget these! Or you WILL HAVE A BAD TIME!
  adhd_t1 ~ 1 * adhd_t1
  adhd_t2 ~ 1 * adhd_t2
'

fit_meas_fix <- cfa(mod_meas_fix, data = adhd_home_env)

lay <- get_layout(
  "accept_t1", "variety_t1", "acStim_t1", NA, "accept_t2", "variety_t2", "acStim_t2",
  NA,          "HOME_t1",    NA,          NA, NA,          "HOME_t2",    NA,
  NA,          "adhd_t1",    NA,          NA, NA,          "adhd_t2",    NA,
  rows = 3
)


graph_sem(fit_meas_fix,
          edges = get_edges(fit_meas_fix, label = "est_std"),
          layout = lay, angle = 90)

#We can see that there are quite strong auto-correlations for the indicators!
  
#  ***An Important Note: Correlations/covariances between endogenous variables is*** **actually *the correlation/covariance between the unique variance of these endogenous variables (the part of their variance unaccounted for by the variable that affect or cause them)***. In our case, this means that the values of the correlations are not between the *observed* `accept`/`variety`/`acStim` in time1 and time2, but between the **errors** of `accept`/`variety`/`acStim` in time1 and time2! An example: the unique variation in `accept_t1` that is not due to the latent variable `HOME_t1` has a correlation of 0.65 with the unique variation in `accept_t2` that is not due to the latent variable `HOME_t2`.

# 2. How has the factor definition improved ?

## Are the loadings better?

standardizedSolution(fit_meas_fix, output = "text")[1:6, ]


  
  ## How about the reliability?
  
 # We need to re-fit the model with **single indicator latent variables** (instead of the self-regressions).

mod_meas_fix_latOnly <- '
  ## latent variable definitions (CFA)
  HOME_t1 =~ accept_t1 + variety_t1 + acStim_t1
  HOME_t2 =~ accept_t2 + variety_t2 + acStim_t2
  ADHD_t1 =~ 1*adhd_t1
  ADHD_t2 =~ 1*adhd_t2
  
  ## covariances
  HOME_t1 ~~ HOME_t2 + ADHD_t1 + ADHD_t2
  HOME_t2 ~~ ADHD_t1 + ADHD_t2
  ADHD_t1 ~~ ADHD_t2
  
  ## Autoregressions
   accept_t1 ~~ accept_t2
  variety_t1 ~~ variety_t2
   acStim_t1 ~~ acStim_t2
'

fit_meas_fix_latOnly <- cfa(mod_meas_fix_latOnly, data = adhd_home_env)

semTools::reliability(fit_meas_fix_latOnly)

fitMeasures(fit_meas_fix, output = "text",
            fit.measures = c("nfi", "nnfi", "tli", "cfi", 
                             "gfi", "rmsea"))

anova(fit_meas_fix)


#The measures of fit are amazing!
  
#  However, the chi^2 is significant! What to do?? (Also, note that chi^2/2 = 27.05 / 2 = 2.08 which is not *bad*, but still greater than the rule-of-thumb of 2.)

#With a large enough sample, and a complex enough model,
#the chi^2 test will almost definitely be significant.
#In such cases, we default to relying on the measures of fit,
#which are more indicative of how well our model fits to the data
#(or know well were we able to recreate the covariance matrix).
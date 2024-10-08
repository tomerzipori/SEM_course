

# ==== Defining a Measurement Model with Latent Variables ====


# Today we will explore the relationship between ADHD and home-environment in
# children, in a cross lagged panel design.

adhd_home_env <- read.csv("ex2/adhd_home_env.csv")
head(adhd_home_env)

# We can see that we have 4 measures taken at two time points:

# -  accept: How accepting are the parents of the child.
# - variety: How much variety of experiences is the child exposed to.
# -  acStim: (academic stimulation) How much is learning encouraged?
# (These 3 make up a scale of "home-environment".)
# -    adhd: number of adhd symptoms.







# Fitting a CFA / measurement model ----------------------------------------

library(lavaan)

mod_meas <- '
  ## latent variable definitions (CFA)
  # The "=~" can be read as "is identified by"
  HOME_t1 =~ accept_t1 + variety_t1 + acStim_t1
  HOME_t2 =~ accept_t2 + variety_t2 + acStim_t2
  
  ## covariances
  HOME_t1 ~~ HOME_t2 + adhd_t1 + adhd_t2 # many at once!
  HOME_t2 ~~ adhd_t1 + adhd_t2
  adhd_t1 ~~ adhd_t2
  
  ## self-regression
  # We need these for the observed vars (only in the measurment model).
  # (This is a silly bug; no extra parameters are actully "used".)
  adhd_t1 ~ 1 * adhd_t1
  adhd_t2 ~ 1 * adhd_t2
'

fit_meas <- cfa(mod_meas, data = adhd_home_env)




library(tidySEM)
library(dplyr)

lay <- get_layout(
  "accept_t1", "variety_t1", "acStim_t1", NA, "accept_t2", "variety_t2", "acStim_t2",
  NA,          "HOME_t1",    NA,          NA, NA,          "HOME_t2",    NA,
  NA,          "adhd_t1",    NA,          NA, NA,          "adhd_t2",    NA,
  rows = 3
)

g <- prepare_graph(fit_meas,  layout = lay, angle = 90)

edges(g) <- edges(g) %>% 
  mutate(
    label = est_std,
    label_location = 0.4 # need this to avoid overlap on path labels
  )

plot(g)

summary(fit_meas, standardize = TRUE)
# Why is there no z-test for HOME_t1 =~ accept_t1?
# Are the loadings good?

parameterEstimates(fit_meas, output = "text")
standardizedSolution(fit_meas, output = "text")
# By DEFAULT:
# 1. the COV between latent vars is estimated.
#   (but I recommend setting it manually as we did here.)
# 2. The factor loading of the 1st indicator of a latent variable is fixed to 1,
#   thereby fixing the scale of the latent variable... 
#   (And fixed parameters are not tested.)




# We can also set `std.lv = TRUE` to set the scale of latent vars to 1:
fit_meas <- cfa(mod_meas, data = adhd_home_env, 
                std.lv = TRUE)
summary(fit_meas, standardize = TRUE)
# But this can cause issues sometimes (often with structural models), so we will
# not be using this much.

# Reliability -------------------------------------------------------------

# To measure the reliability of our factors, we need to fit the same model in a
# slightly different way: Each observed variable (non indicator) needs to be
# used to define a single-indicator latent variable. (This has no effect on
# model fit / df, it is needed purely for technical reasons).


mod_lat_only <- '
  ## latent variable definitions (CFA)
  # The "=~" can be read as "is identified by"
  HOME_t1 =~ accept_t1 + variety_t1 + acStim_t1
  HOME_t2 =~ accept_t2 + variety_t2 + acStim_t2
  ADHD_t1 =~ 1*adhd_t1
  ADHD_t2 =~ 1*adhd_t2
  
  ## covariances
  # We not longet need any of these - by default COV between latent vars is
  # estimated. But we will do this anyway.
  HOME_t1 ~~ HOME_t2 + ADHD_t1 + ADHD_t2
  HOME_t2 ~~ ADHD_t1 + ADHD_t2
  ADHD_t1 ~~ ADHD_t2
  
  ## We also no longer need the self-regressors
'

fit_lat_only <- cfa(mod_lat_only, data = adhd_home_env)


semTools::reliability(fit_lat_only)
# We will look at `omega`, which is similar to alpha, but doesn't assume equal
# weights / loadings (which we just estimated!). It can be thought of as
# representing the variance explained across the indicators of each factor.
#
# read about the other indices here, and when you'd like to use them:
?semTools::reliability
# (see also semTools::reliabilityL2)
# https://doi.org/10.1037/met0000144
# https://doi.org/10.1177%2F2515245920951747


# === NOTE ===
# to get a correct measure of reliability, all indicators must be in the
# same "direction" (so all loadings should positive or all negative). If some
# indicator has a negative loading you need to reverse the variable for the
# reliability measures to make sense!








# we can also extract the scores of the latent variables, but note that these
# mostly only make sense WITHIN a SEM model. (set `append.data = TRUE` to also
# get the original data)
cfa_scores <- data.frame(lavPredict(fit_meas, append.data = FALSE))
head(cfa_scores)











# Equal Loadings ----------------------------------------------------------



# If we have some prior knowledge about the structure of latent variables, we
# might consider using equal factor loading. To do this, we simply give all the
# indicators the same *modifier label*.
#
# For example, we might assume that the loadings of HOME are equal in time 1 and
# in time 2. (This is called "measurement invariance" - we will learn more about
# this later in the semester.)

mod_meas_eq_load <- '
  ## latent variable definitions (CFA)
  HOME_t1 =~ b1 * accept_t1 + b2 * variety_t1 + b3 * acStim_t1
  HOME_t2 =~ b1 * accept_t2 + b2 * variety_t2 + b3 * acStim_t2
  
  ## covariances
  HOME_t1 ~~ HOME_t2 + adhd_t1 + adhd_t2
  HOME_t2 ~~ adhd_t1 + adhd_t2
  adhd_t1 ~~ adhd_t2
'

fit_meas_eq_load <- cfa(mod_meas_eq_load, data = adhd_home_env)
# How many free parameters have we gained?



summary(fit_meas_eq_load, standardize = TRUE)
# Note that the standardized loadings are NOT equal!
# (We will learn next time how to deal with that...)













# Exercise ----------------------------------------------------------------

big5 <- na.omit(psychTools::bfi)

head(big5)

# 1. Fit a CFA model with the big 5 as latent factors: A, C, E, N, O.
big5_cfa <- '
  ## latent variable definitions (CFA)
  # The "=~" can be read as "is identified by"
  A =~ A1 + A2 + A3 + A4 + A5
  C =~ C1 + C2 + C3 + C4 + C5
  E =~ E1 + E2 + E3 + E4 + E5
  N =~ N1 + N2 + N3 + N4 + N5
  O =~ O1 + O2 + O3 + O4 + O5
  
  ## covariances
  A ~~ O
  C ~~ O
  
  ## self-regression
  # We need these for the observed vars (only in the measurment model).
  # (This is a silly bug; no extra parameters are actully "used".)
'

fit_meas_bug5 <- cfa(big5_cfa, data = big5)

summary(fit_meas_bug5)

lay <- get_layout(
  NA,    NA,  "A",   NA,   NA,  NA,  NA,   NA,  "C",   NA,   NA,  NA,  NA,   NA,  "E",   NA,   NA,  NA,  NA,  NA,   "N",   NA,   NA,  NA,  NA,   NA,  "O",   NA,   NA,  NA,
  "A1", "A2", "A3", "A4", "A5", NA, "C1", "C2", "C3", "C4", "C5", NA, "E1", "E2", "E3", "E4", "E5", NA, "N1", "N2", "N3", "N4", "N5", NA, "O1", "O2", "O3", "O4", "O5", NA,
  rows = 2
)

g <- prepare_graph(fit_meas_bug5, layout = lay, angle = 90)

edges(g) <- edges(g) %>% 
  mutate(
    label = est_std,
    label_location = 0.4 # need this to avoid overlap on path labels
  )

plot(g)

# 2. Are there any items with low loadings? You can see the actual questions:
?psychTools::bfi
# 3. What are the reliabilities of the factors?

semTools::reliability(fit_meas_bug5)
# 4. Plot the CFA as nice as you can.








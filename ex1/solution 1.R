### Explain the implied causal relationship in this model (in words).

There are 3 effects the *Anxiety* has on *Income*:
  
1. An indirect effect mediated by Negative Mood: anxiety affects neg-mood which affects income.
2. An indirect effect mediated by Shyness: anxiety affects shyness which affects income.
3. A direct un-mediated affect.

### Fit this model with `lavaan` (one regression for each endogenous/predicted variable).


income_psych <- read.csv("income_psych.csv")

mediation_model <- '
  neg_mood ~ anxiety
   shyness ~ anxiety
    income ~ anxiety + neg_mood + shyness
'

fit <- sem(mediation_model, data = income_psych)
### Plot the model.
#the next line make the plot clearly arranged. chek the plot with and without the "layout" componet
lay <- get_layout(
  NA,        "neg_mood", NA,
  "anxiety", NA,         "income",
  NA,        "shyness",  NA,
  rows = 3
)

graph_sem(fit, edges = get_edges(fit, label = "est_std"), 
          layout = lay, angle = 90)

# 2. Estimate

mediation_model <- '
  neg_mood ~ a * anxiety
   shyness ~ c * anxiety
    income ~ e * anxiety + b * neg_mood + d * shyness
    
  ## Estimate:
  # Each of the paths in this model from *anxiety* to *income*.
  indirect_negmood := a * b
  indirect_shyness := c * d
            direct := e
  
  # The total of these paths.
  total := indirect_negmood + indirect_shyness + direct
  
  # The difference between the two indirect paths.
  indirect_diff := indirect_negmood - indirect_shyness
'

fit <- sem(mediation_model, data = income_psych)

summary(fit, standardize = TRUE)


### Explain your findings

1. There is a negative indirect path mediated by negative mood: anxiety increases negative-mood, which decreases income.
2. There is a negative indirect path mediated by shyness: anxiety increases shyness, which decreases income.
- These effects are not significantly different from one another.





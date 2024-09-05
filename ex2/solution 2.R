library(lavaan)
library(semTools)

# 1. Fit a CFA model with the big 5 as latent factors

big5_mod <- "
  A =~ A1 + A2 + A3 + A4 + A5
  C =~ C1 + C2 + C3 + C4 + C5
  E =~ E1 + E2 + E3 + E4 + E5
  N =~ N1 + N2 + N3 + N4 + N5
  O =~ O1 + O2 + O3 + O4 + O5
"

big5_fit <- cfa(big5_mod, data = big5)

# I just want some of the parameters
standardizedSolution(big5_fit, output = "text")[c(1:25, 56:65), ]


# 2. Are there any items with low loadings?
#You can see the actual items:
?psychTools::bfi

#Looking at a cutoff of >|0.5|:
  
#- A on A1
#- O on O2 and O5 also seem pretty low 

# 3. What are the reliabilities of the factors?

semTools::reliability(big5_fit)

#Oh... that's not good. But wait!

#**Remember**: For reliability, we need all items to be "in the same direction".

#Looking at `?psychTools::bfi` I've spotted that items A1, C4, C5, E1, E2, O2, and O5 are reversed. Let's flip them!

library(dplyr)

big5 <- big5 %>% 
  mutate(
    across(.cols = c(A1, C4, C5, E1, E2, O2, O5),
           .fns = function(x) -x,
           .names = "{col}_r")
  )

#Refit the model:
  
  big5_mod_with_r <- "
  A =~ A1_r + A2   + A3 + A4   + A5
  C =~ C1   + C2   + C3 + C4_r + C5_r
  E =~ E1_r + E2_r + E3 + E4   + E5
  N =~ N1   + N2   + N3 + N4   + N5
  O =~ O1   + O2_r + O3 + O4   + O5_r
"

big5_fit_with_r <- cfa(big5_mod_with_r, data = big5)

semTools::reliability(big5_fit_with_r)

# 4. Plot the CFA as nice as you can.

lay <- get_layout(
  NA,   NA,   "A",  NA,   NA,   NA, NA,   NA,   "C",  NA,   NA,   NA, NA,   NA,   "E",  NA,   NA,   NA, NA,   NA,   "N",  NA,   NA,   NA, NA,   NA,   "O",  NA,   NA,
  NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,
  NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,   NA, NA,   NA,   NA,   NA,   NA,
  "A1", "A2", "A3", "A4", "A5", NA, "C1", "C2", "C3", "C4", "C5", NA, "E1", "E2", "E3", "E4", "E5", NA, "N1", "N2", "N3", "N4", "N5", NA, "O1", "O2", "O3", "O4", "O5",
  rows = 4
)


g <- prepare_graph(big5_fit, layout = lay, angle = 90, ellipses_width  = 7)

edges(g) <- edges(g) %>% 
  mutate(label = est_std)

nodes(g) <- nodes(g) %>% 
  mutate(label = case_when(
    label == "A" ~ "Agreeableness", 
    label == "C" ~ "Conscientiousness",
    label == "E" ~ "Extraversion",
    label == "N" ~ "Neuroticism",
    label == "O" ~ "Openness",
    TRUE ~ label
  ))

plot(g)

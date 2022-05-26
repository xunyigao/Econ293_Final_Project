rm(list = ls())
options(stringAsFactors=FALSE)

#load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled)

set.seed(123)
#G7 MATH OUTCOMES
star_g7_math <- star_g7 |>
  filter(!is.na(g7tmathss))

# Define treatment and outcome vectors
W <- star_g7_math$W

outcome <- g7_outcomes[2]

Y <- star_g7_math |>
  pull(outcome) |>
  as.vector()

star_g7_x <- star_g7_math |>
  select(-W, -stdntid, -gkclasstype, -g1classtype, -g2classtype, -g3classtype, -cmpstype,-cmpsdura,
         -yearssmall, yearsstar, -gk_smallclass, -g1_smallclass, -g2_smallclass, -g3_smallclass) |>
  select(!starts_with(c("birth", "flagsgk"))) |>
  select(!ends_with("schid")) |>
  select(!contains(c(g7_outcomes)))


#HTE

n <-nrow(star_g7_math)

# Number of rankings predictions will be ranked on
num.rankings <- 5

# Prepare for data.splitting
# Assign a fold number to each observation.
# The argument 'clusters' in the next step will mimic K-fold cross-fitting.
num.folds <- 10
folds <- sort(seq(n) %% num.folds) + 1

# Estimate a causal forest
forest <- causal_forest(X=star_g7_x, Y=Y, W=W, W.hat=mean(W),
                        clusters = star_g7_math$g7schid,
                        min.node.size = 5)


#histogram of estimated HTE via causal forest
hist(predict(forest)$predictions)

#estimated ATE
g7_ate_math <- average_treatment_effect(forest, target.sample = "overlap", method = "AIPW")
g7_ate_math_se <- g7_ate_math[2]
g7_ate_math <- g7_ate_math[1]

#not statistically significant
print(paste0("95% CI: ", round(g7_ate_math,2),
             " +/- ", round(1.96 * g7_ate_math_se, 2)))

#calibrate forest
#p-values are both statistically insignificant meaning no evidence model is well calibrated or heterogeneity?
#but if there were, the below would be true
#mean.forest.prediction of 1.5 means the mean forest prediction is not exactly accurate. underfit i think?
#differential.forest.prediction of -1.13 means no heterogeneity
#
test_calibration(forest)

#important contributing variables in causal forest
var_importance <- variable_importance(forest) |> 
  as_tibble() |>
  mutate(n_col = row_number()) |>
  arrange(-V1)

important_vars_g7_math <- var_importance %>% 
  slice(1:10) %>% 
  pull(n_col)

important_vars_g7_math <- names(star_g7_x[important_vars_g7_math])



#G7 READING OUTCOMES

set.seed(123)

star_g7_reading <- star_g7 |>
  filter(!is.na(g7treadss))

# Define treatment and outcome vectors
W <- star_g7_reading$W

outcome <- g7_outcomes[1]

Y <- star_g7_reading |>
  pull(outcome) |>
  as.vector()

star_g7_x <- star_g7_reading |>
  select(-W, -stdntid, -gkclasstype, -g1classtype, -g2classtype, -g3classtype, -cmpstype,-cmpsdura,
         -yearssmall, yearsstar, -gk_smallclass, -g1_smallclass, -g2_smallclass, -g3_smallclass) |>
  select(!starts_with(c("birth", "flagsgk"))) |>
  select(!ends_with("schid")) |>
  select(!contains(c(g7_outcomes)))


#HTE

n <-nrow(star_g7_reading)

# Number of rankings predictions will be ranked on
num.rankings <- 5

# Prepare for data.splitting
# Assign a fold number to each observation.
# The argument 'clusters' in the next step will mimic K-fold cross-fitting.
num.folds <- 10
folds <- sort(seq(n) %% num.folds) + 1

# Estimate a causal forest
forest <- causal_forest(X = star_g7_x, Y=Y, W=W,W.hat=mean(W),
                        clusters = star_g7_reading$g7schid,
                        min.node.size = 5)

#histogram of estimated heterogeneous effects
hist(predict(forest)$predictions)

#estimated ATEs
g7_ate_reading <- average_treatment_effect(forest, target.sample = "overlap", method = "AIPW")
g7_ate_reading_se <- g7_ate_reading[2]
g7_ate_reading <- g7_ate_reading[1]

#not statistically significant
print(paste0("95% CI: ", round(g7_ate_reading,2),
             " +/- ", round(1.96 * g7_ate_reading_se, 2)))

#calibrate forest
#p-values are both statistically insignificant meaning no evidence model is well calibrated or heterogeneity?
#but if there were, the below would be true
#mean.forest.prediction of 1.4 means the mean forest prediction is not exactly accurate. underfit i think?
#differential.forest.prediction of -0.004 means no heterogeneity
#
test_calibration(forest)

#imortant contributing variables in causal forest
var_importance <- variable_importance(forest) |> 
  as_tibble() |>
  mutate(n_col = row_number()) |>
  arrange(-V1)

important_vars_g7_reading <- var_importance %>% 
  slice(1:10) %>% 
  pull(n_col)

important_vars_g7_reading<- names(star_g7_x[important_vars_g7_reading])

rm(list = ls())
options(stringAsFactors=FALSE)

#load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled)

set.seed(123)
#G7 MATH OUTCOMES
star_hs_1 <- star_hs |>
  filter(!is.na(hsgpaoverall))

# Define treatment and outcome vectors
W <- star_hs_1$W

outcome <- hs_outcomes[1]

Y <- star_hs_1 |>
  pull(outcome) |>
  as.vector()

star_hs_x <- star_hs_1 |>
  select(-W, -stdntid) |>
  select(!contains(c(hs_outcomes)))

n <- nrow(star_hs_1)

# Number of rankings predictions will be ranked on
num.rankings <- 5

# Prepare for data.splitting
# Assign a fold number to each observation.
# The argument 'clusters' in the next step will mimic K-fold cross-fitting.
num.folds <- 10
folds <- sort(seq(n) %% num.folds) + 1

# Estimate a causal forest
forest <- causal_forest(X = star_hs_x, Y=Y, W=W,
                        clusters = star_hs_x$hsid,
                        min.node.size = 5)

#histogram of estimated HTE via causal forest
hist(predict(forest)$predictions)

#ate predictions
hs_ate <- average_treatment_effect(forest, target.sample = "overlap", method = "AIPW")
hs_ate_se <- hs_ate[2]
hs_ate <- hs_ate[1]

#not statistically significant
print(paste0("95% CI: ", round(hs_ate,2),
             " +/- ", round(1.96 * hs_ate_se, 2)))

#calibrate forest
#mean.forest.prediction is statistically insignificant but if it wasn't i think the coef of 3.73 means way underfit
#differential.forest.prediction statistically significant and about 1, so there is heterogeneity

test_calibration(forest)

#important contributing variables in causal forest
var_importance <- variable_importance(forest) |> 
  as_tibble() |>
  mutate(n_col = row_number()) |>
  arrange(-V1)

important_vars_hs <- var_importance %>% 
  slice(1:10) %>% 
  pull(n_col)

important_vars_hs <- names(star_hs_x[important_vars_hs])


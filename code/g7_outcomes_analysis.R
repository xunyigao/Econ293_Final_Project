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
  select(-W, -stdntid) |>
  select(!contains(c(g7_outcomes)))


#ATE via linear regression
bivariate_ols <- lm(g7tmathss ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    + as.factor(race)
                    + as.factor(gksurban)
                    + as.factor(g1surban)
                    + as.factor(g2surban)
                    + as.factor(g3surban)
                    + as.factor(gkthighdegree)
                    + as.factor(g1thighdegree)
                    + as.factor(g2thighdegree)
                    + as.factor(g3thighdegree)
                    + as.factor(gkfreelunch)
                    + as.factor(g1freelunch)
                    + as.factor(g2freelunch)
                    + as.factor(g3freelunch)
                    + as.factor(gkrepeat)
                    + gktyears
                    + g1tyears
                    + g2tyears
                    + g3tyears
                    + as.factor(gkspeced)
                    + as.factor(g1speced), data=star_g7_math)
summary(bivariate_ols)

yearssmall_ols <- lm(g7tmathss ~ as.factor(yearssmall) 
                     + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid)
                     + as.factor(race)
                     + as.factor(gksurban)
                     + as.factor(g1surban)
                     + as.factor(g2surban)
                     + as.factor(g3surban)
                     + as.factor(gkthighdegree)
                     + as.factor(g1thighdegree)
                     + as.factor(g2thighdegree)
                     + as.factor(g3thighdegree)
                     + as.factor(gkfreelunch)
                     + as.factor(g1freelunch)
                     + as.factor(g2freelunch)
                     + as.factor(g3freelunch)
                     + as.factor(gkrepeat)
                     + gktyears
                     + g1tyears
                     + g2tyears
                     + g3tyears
                     + as.factor(gkspeced)
                     + as.factor(g1speced), data=star_g7_math)
summary(yearssmall_ols)

ksmall_ols <- lm(g7tmathss ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_g7_math)
summary(ksmall_ols)

g1small_ols <- lm(g7tmathss ~ g1_smallclass 
                  + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_math)
summary(g1small_ols)

g2small_ols <- lm(g7tmathss ~ g2_smallclass+as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(g2surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(g2thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(g2freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + g2tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_math)
summary(g2small_ols)

g3small_ols <- lm(g7tmathss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(g2surban)
                  + as.factor(g3surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(g2thighdegree)
                  + as.factor(g3thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(g2freelunch)
                  + as.factor(g3freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + g2tyears
                  + g3tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_math)
summary(g3small_ols)

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
forest <- causal_forest(X = star_g7_x, Y=Y, W=W,
                        clusters = as.factor(star_g7_x$g7schid),
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
  select(-W, -stdntid) |>
  select(!contains(c(g7_outcomes)))

#ATE via linear regression
bivariate_ols <- lm(g7treadss ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    + as.factor(race)
                    + as.factor(gksurban)
                    + as.factor(g1surban)
                    + as.factor(g2surban)
                    + as.factor(g3surban)
                    + as.factor(gkthighdegree)
                    + as.factor(g1thighdegree)
                    + as.factor(g2thighdegree)
                    + as.factor(g3thighdegree)
                    + as.factor(gkfreelunch)
                    + as.factor(g1freelunch)
                    + as.factor(g2freelunch)
                    + as.factor(g3freelunch)
                    + as.factor(gkrepeat)
                    + gktyears
                    + g1tyears
                    + g2tyears
                    + g3tyears
                    + as.factor(gkspeced)
                    + as.factor(g1speced), data=star_g7_reading)
summary(bivariate_ols)

yearssmall_ols <- lm(g7treadss ~ as.factor(yearssmall) +as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid)
                     + as.factor(race)
                     + as.factor(gksurban)
                     + as.factor(g1surban)
                     + as.factor(g2surban)
                     + as.factor(g3surban)
                     + as.factor(gkthighdegree)
                     + as.factor(g1thighdegree)
                     + as.factor(g2thighdegree)
                     + as.factor(g3thighdegree)
                     + as.factor(gkfreelunch)
                     + as.factor(g1freelunch)
                     + as.factor(g2freelunch)
                     + as.factor(g3freelunch)
                     + as.factor(gkrepeat)
                     + gktyears
                     + g1tyears
                     + g2tyears
                     + g3tyears
                     + as.factor(gkspeced)
                     + as.factor(g1speced), data=star_g7_reading)
summary(yearssmall_ols)

ksmall_ols <- lm(g7treadss ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_g7_reading)
summary(ksmall_ols)

g1small_ols <- lm(g7treadss ~ g1_smallclass +as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_reading)
summary(g1small_ols)

g2small_ols <- lm(g7treadss ~ g2_smallclass+as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(g2surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(g2thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(g2freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + g2tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_reading)
summary(g2small_ols)

g3small_ols <- lm(g7treadss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(race)
                  + as.factor(gksurban)
                  + as.factor(g1surban)
                  + as.factor(g2surban)
                  + as.factor(g3surban)
                  + as.factor(gkthighdegree)
                  + as.factor(g1thighdegree)
                  + as.factor(g2thighdegree)
                  + as.factor(g3thighdegree)
                  + as.factor(gkfreelunch)
                  + as.factor(g1freelunch)
                  + as.factor(g2freelunch)
                  + as.factor(g3freelunch)
                  + as.factor(gkrepeat)
                  + gktyears
                  + g1tyears
                  + g2tyears
                  + g3tyears
                  + as.factor(gkspeced)
                  + as.factor(g1speced), data=star_g7_reading)
summary(g3small_ols)

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
forest <- causal_forest(X = star_g7_x, Y=Y, W=W,
                        clusters = star_g7_x$g7schid,
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

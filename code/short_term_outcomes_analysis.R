
rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

# Load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled)

options(stringAsFactors=FALSE)

# Load cleaned student-level data
load("./data/star_short_term.rda")

star_short_term_1 <- star_short_term %>% 
  filter(!is.na(g3tmathss)) 

# Define outcome and treatment vectors
W <- star_short_term_1$W

outcome <- short_term_outcomes[2]

Y <- star_short_term_1 %>% pull(outcome) %>% as.vector()

star_short_term_x <- star_short_term_1 %>% 
  select(-W, -stdntid) %>% 
  select(!contains(c(short_term_outcomes)))

n <- nrow(star_short_term_1)

# Number of rankings that the predictions will be ranking on 
# (e.g., 2 for above/below median estimated CATE, 5 for estimated CATE quintiles, etc.)
num.rankings <- 5  

# Prepare for data.splitting
# Assign a fold number to each observation.
# The argument 'clusters' in the next step will mimic K-fold cross-fitting.
num.folds <- 10
folds <- sort(seq(n) %% num.folds) + 1

# Estimate a causal forest
forest <- causal_forest(X = star_short_term_x, Y=Y, W=W,
                        clusters = star_short_term_x$g3schid,
                        min.node.size = 5)


average_treatment_effect(forest, target.sample = "overlap", method = "AIPW")

test_calibration(forest)

# Explore variable importance suggested by the causal forest
var_importance <- variable_importance(forest) %>% 
  as_tibble() %>% 
  mutate(n_col = row_number()) %>% 
  arrange(-V1)

important_vars <- var_importance %>% 
  slice(1:10) %>% 
  pull(n_col)

test <- star_short_term_x[important_vars]

hist(predict(forest)$predictions)


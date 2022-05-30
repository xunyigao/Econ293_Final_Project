


rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

# Load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled, plm, sandwich, lmtest, glue)

options(stringAsFactors=FALSE)

# Load cleaned student-level data
load("./data/star_short_term.rda")

# Drop small class size flags


# Define short-term: grade 3 outcomes
short_term_outcomes <- c("g1treadss", "g1tmathss", "g1wordskillss")

star_short_term <- star_short_term %>% 
  filter(!is.na(g1_smallclass)) %>% 
  mutate(W = if_else(g1_smallclass==1, 1, 0))
  

star_short_term <- star_short_term %>% 
  dplyr::select(-gkclasstype, -g1classtype, -g2classtype, -g3classtype, -cmpstype,
                -yearssmall, -gk_smallclass, -g1_smallclass, -g2_smallclass, -g3_smallclass,
                -yearsstar, -g1readbsraw, -g1mathbsraw, -g1readbsobjraw, -g1mathbsobjraw,
                -g1readbsobjpct, -g1mathbsobjpct, -g1tchid, -gktchid, -g1promote, -cmpsdura)

# star_short_term_1 <- star_short_term %>%
#   filter(!is.na(g3tmathss)) %>%
#   dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2"))) 

# Exclude schoo level characteristics
star_short_term_1 <- star_short_term %>%
  dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2") | ends_with("_g3")))

star_short_term_1 <- star_short_term_1 %>%
  dplyr::select(-(starts_with("g2") | starts_with("g3")))

std_err <- function(x) sd(x)/sqrt(length(x))

for (outcome in short_term_outcomes) {
  
  # Drop observations with missing values for the outcome
  star_short_term_1 <- star_short_term %>%
    filter(!is.na(get(outcome)))
  
  # Define outcome and treatment vectors
  W <- star_short_term_1$W
  
  Y <- star_short_term_1 %>% pull(outcome) %>% as.vector()
  
  # Drop other covariates that are not useful
  star_short_term_x <- star_short_term_1 %>% 
    dplyr::select(-W, -stdntid) %>% 
    dplyr::select(-contains(c(short_term_outcomes))) %>% 
    dplyr::select(-contains("classsize")) %>% 
    dplyr::select(-contains("flag")) %>% 
    dplyr::select(-gkschid, -schid_yr1star)
  
  n <- nrow(star_short_term_1)
  
  
  # Linear Regression -------------------------------------------------------
  
  # Define a list of covariates manually inputed
  covariates <- c("race", "gender", "birthyear", "g1surban", "g2surban",
                  "g2readbsraw", "g2motivraw", "g3surban",
                  "g3freelunch", "g3schid", "g3tyears", "g2tyear")
  
  star_short_term_cov <- star_short_term_x %>% 
    dplyr::select(contains(c(covariates))) 
  
  
  linear_model_data <- star_short_term_x %>%
    mutate(W = W,
           y = Y) %>%
    dplyr::select(W, y, race, gender, birthyear, g1surban,
                  g1freelunch, g1schid)
  
  fe_model <- plm(y ~ factor(W) + factor(race) + gender + g1surban + g1freelunch,
                  data = linear_model_data,
                  index = c("g1schid"),
                  model = "within")
  
  # print summary using robust standard errors
  coeftest(fe_model, vcov. = vcovHC, type = "HC1")
  
  
  # Causal Forest -----------------------------------------------------------
  
  # Estimate a causal forest
  forest <- causal_forest(X = star_short_term_x, Y=Y, W=W, 
                          clusters = star_short_term_x$g1schid,
                          min.node.size = 5,
                          tune.parameters = "all",
                          seed = 20220524)
  
  # Plot propensity scores
  hist(forest$W.hat)
  
  # Average treatment effect
  average_treatment_effect(forest, method = "AIPW")
  
  # Test for heterogeneous treatment effects
  test_calibration(forest)
  
  #
  # Explore variable importance suggested by the causal forest
  # var_importance <- variable_importance(forest) %>% 
  #   as_tibble() %>% 
  #   mutate(n_col = row_number()) %>% 
  #   arrange(-V1)
  # 
  # important_vars <- var_importance %>% 
  #   slice(1:10) %>% 
  #   pull(n_col)          
  # 
  # test <- star_short_term_x[important_vars]
  
  star_short_term_x <- star_short_term_x %>% 
    mutate(cate_estimate = predict(forest)$predictions)
  
  hist(predict(forest)$predictions)
  
  # ggplot(star_short_term_x, aes(race, g1freelunch, fill = cate_estimate)) +
  #   geom_raster()
  # 
  # star_short_term_x %>% 
  #   # group_by(race) %>% 
  #   # summarize(mean_cate = mean(cate_estimate)) %>% 
  #   # ungroup() %>% 
  #   ggplot(aes(x = factor(g1surban), y = cate_estimate)) +
  #   geom_boxplot(aes(group = factor(g1surban)))
  
  
  save(star_short_term_x, file = glue("./data/cate_estimates_short_term_{outcome}.rda"))

}


# QINI Curve --------------------------------------------------------------

# rate.wordskillss <- rank_average_treatment_effect(forest, star_short_term_x$cate_estimate, target = "QINI")
# 
# plot(rate.wordskillss)

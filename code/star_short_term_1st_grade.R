


rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

# Load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled, plm, sandwich, lmtest, glue)

options(stringAsFactors=FALSE)

# Load cleaned student-level data
load("./data/star_short_term.rda")


# Define short-term: grade 3 outcomes
short_term_outcomes <- c("g1treadss", "g1tmathss", "g1wordskillss")

star_short_term <- star_short_term %>% 
  filter(!is.na(g1_smallclass)) %>% 
  mutate(W = if_else(g1_smallclass==1, 1, 0))
  
# Drop small class size flags
star_short_term <- star_short_term %>% 
  dplyr::select(-gkclasstype, -g1classtype, -g2classtype, -g3classtype, -cmpstype,
                -yearssmall, -gk_smallclass, -g1_smallclass, -g2_smallclass, -g3_smallclass,
                -yearsstar, -g1readbsraw, -g1mathbsraw, -g1readbsobjraw, -g1mathbsobjraw,
                -g1readbsobjpct, -g1mathbsobjpct, -g1tchid, -gktchid, -g1promote, -cmpsdura)

# star_short_term_1 <- star_short_term %>%
#   filter(!is.na(g3tmathss)) %>%
#   dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2"))) 

# Exclude school level characteristics
star_short_term_1 <- star_short_term %>%
  dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2") | ends_with("_g3")))

star_short_term_1 <- star_short_term_1 %>%
  dplyr::select(-(starts_with("g2") | starts_with("g3")))

std_err <- function(x) sd(x)/sqrt(length(x))

for (outcome in short_term_outcomes) {
  
  # Drop observations with missing values for the outcome
  star_short_term_1 <- star_short_term_1 %>%
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
  print(coeftest(fe_model, vcov. = vcovHC, type = "HC1"))
  
  
  # Causal Forest -----------------------------------------------------------
  
  # Estimate a causal forest
  forest <- causal_forest(X = star_short_term_x, Y=Y, W=W, 
                          clusters = star_short_term_x$g1schid,
                          min.node.size = 5,
                          tune.parameters = "all",
                          seed = 20220524)
  
  # Plot propensity scores
  # hist(forest$W.hat)
  
  # Average treatment effect
  print(average_treatment_effect(forest, method = "AIPW"))
  
  
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
  
  star_short_term_x %>% 
    ggplot(aes(x = cate_estimate)) +
    geom_histogram(bins = 50, color = "black") + 
    theme_light() +
    xlab("CATE Estimate") +
    ylab("Number of students")
  
  ggsave(glue("./graphs/cate_estimates_hist_short_term_{outcome}.png"),
         width = 14,
         height = 10)
  
  
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


# Analyzing CATE estimates ------------------------------------------------


# Test for heterogeneous treatment effects
  test_calibration(forest)
  
  quartile_est_df <- tibble(quartile = rep(c(1, 2, 3, 4), 2), 
                            type = c(rep("Mean CATE", 4), rep("AIPW ATE", 4)),
                            estimate = rep(NA_real_, 8),
                            std_err = rep(NA_real_, 8))
  
  
  star_short_term_x <- star_short_term_x %>% 
    mutate(
      y = Y,
      cate_quartile = ntile(cate_estimate, 4),
      w = W
    )
  
  # Compute ATE using AIPW for each quartile
  for (q in seq(1, 4, 1)) {
    
    X_q = star_short_term_x %>%
      filter(cate_quartile==q) %>% 
      dplyr::select(-cate_quartile, -cate_estimate, -y, -w) 
    
    Y_q <- star_short_term_x %>%
      filter(cate_quartile==q) %>% 
      pull(y)
    
    W_q <- star_short_term_x %>%
      filter(cate_quartile==q) %>% 
      pull(w)
    
    g1_school <- star_short_term_x %>%
      filter(cate_quartile==q) %>%
      pull(g1schid)
    
    forest_q <- causal_forest(X_q, Y_q, W_q,
                              clusters = g1_school,
                              tune.parameters = "all",
                              seed = 20220524)
    
    quartile_ate <- average_treatment_effect(forest_q, target.sample = "overlap")
    
    aipw_se = quartile_ate[2]
    
    # print(quartile_ate)
    
    mean_cate = star_short_term_x %>%
      filter(cate_quartile==q) %>% 
      pull(cate_estimate) %>% 
      mean()
    
    cate_se = star_short_term_x %>%
      filter(cate_quartile==q) %>% 
      pull(cate_estimate) %>% 
      std_err()
    
    quartile_est_df[q, "estimate"] <- mean_cate
    quartile_est_df[q, "std_err"] <- cate_se
    quartile_est_df[q+4, "estimate"] <- quartile_ate[1]
    quartile_est_df[q+4, "std_err"] <- aipw_se
    
    # print(mean_cate)
    
  }
  
  ggplot(quartile_est_df) +
    aes(x = quartile, y = estimate, group=type, color=type) + 
    geom_point(position=position_dodge(0.2), size = 3) +
    geom_errorbar(aes(ymin=estimate-2*std_err, ymax=estimate+2*std_err),
                  width=.2, position=position_dodge(0.2),
                  size = 1.2) +
    ylab("") + xlab("CATE Quartile") +
    theme_light() +
    theme(legend.position="bottom", legend.title = element_blank())  
    
  ggsave(glue("./graphs/cate_quartiles_short_term_{outcome}.png"),
         width = 14,
         height = 10)


}

# QINI Curve --------------------------------------------------------------

# rate.wordskillss <- rank_average_treatment_effect(forest, star_short_term_x$cate_estimate, target = "QINI")
# 
# plot(rate.wordskillss)

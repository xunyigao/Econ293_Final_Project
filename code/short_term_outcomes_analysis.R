
rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

# Load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled, plm, sandwich, lmtest)

options(stringAsFactors=FALSE)

# Load cleaned student-level data
load("./data/star_short_term.rda")

# Drop small class size flags


# Define short-term: grade 3 outcomes
short_term_outcomes <- c("g3treadss", "g3tmathss", "g3readbsraw", "g3mathbsraw")

star_short_term <- star_short_term %>% 
  dplyr::select(-gkclasstype, -g1classtype, -g2classtype, -g3classtype, -cmpstype,
         -yearssmall, -gk_smallclass, -g1_smallclass, -g2_smallclass, -g3_smallclass,
         -yearsstar)

star_short_term_1 <- star_short_term %>%
  filter(!is.na(g3mathbsraw))

# star_short_term_1 <- star_short_term %>%
#   filter(!is.na(g3tmathss)) %>%
#   dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2"))) 

# Exclude schoo level characteristics
star_short_term_1 <- star_short_term_1 %>%
  dplyr::select(-(ends_with("_k") | ends_with("_g1") | ends_with("_g2") | ends_with("_g3")))

std_err <- function(x) sd(x)/sqrt(length(x))


# Define outcome and treatment vectors
W <- star_short_term_1$W

outcome <- short_term_outcomes[4]

Y <- star_short_term_1 %>% pull(outcome) %>% as.vector()

star_short_term_x <- star_short_term_1 %>% 
  dplyr::select(-W, -stdntid) %>% 
  dplyr::select(-contains(c(short_term_outcomes))) %>% 
  dplyr::select(-contains("classsize")) %>% 
  dplyr::select(-contains("flag")) %>% 
  dplyr::select(-gkschid, -g1schid, -g2schid)

n <- nrow(star_short_term_1)

# Define a list of covariates manually inputed
covariates <- c("race", "gender", "birthyear", "g1surban", "g2surban",
                "g2readbsraw", "g2motivraw", "g3surban",
                "g3freelunch", "g3schid", "g3tyears", "g2tyear")

star_short_term_cov <- star_short_term_x %>% 
  dplyr::select(contains(c(covariates))) 

# Linear Regression -------------------------------------------------------

linear_model_data <- star_short_term_x %>%
  mutate(W = W,
         y = Y) %>%
  dplyr::select(W, y, race, gender, birthyear, g1surban, g2surban,
                g2readbsraw, g2motivraw, g3surban,
                g3freelunch, g3schid)

fe_model <- plm(y ~ factor(W) + factor(race) + gender + g1surban + g2surban + g2readbsraw + g3freelunch,
                    data = linear_model_data,
                    index = c("g3schid"),
                    model = "within")

# print summary using robust standard errors
coeftest(fe_model, vcov. = vcovHC, type = "HC1")

# Number of rankings that the predictions will be ranking on
# (e.g., 2 for above/below median estimated CATE, 5 for estimated CATE quintiles, etc.)
num.rankings <- 5

# Prepare for data.splitting
# Assign a fold number to each observation.
# The argument 'clusters' in the next step will mimick K-fold cross-fitting.
num.folds <- 10
folds <- sort(seq(n) %% num.folds) + 1

# cor(star_short_term_x, W) 

# Causal Forest -----------------------------------------------------------

# Estimate a causal forest
forest <- causal_forest(X = star_short_term_cov, Y=Y, W=W,
                        clusters = star_short_term_x$g3schid,
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
var_importance <- variable_importance(forest) %>% 
  as_tibble() %>% 
  mutate(n_col = row_number()) %>% 
  arrange(-V1)

important_vars <- var_importance %>% 
  slice(1:10) %>% 
  pull(n_col)          

test <- star_short_term_x[important_vars]

star_short_term_x <- star_short_term_x %>% 
  mutate(cate_estimate = predict(forest)$predictions)

hist(predict(forest)$predictions)

ggplot(star_short_term_x, aes(race, g3motivraw, fill = cate_estimate)) +
  geom_raster()

star_short_term_x %>% 
  group_by(race) %>% 
  summarize(mean_cate = mean(cate_estimate)) %>% 
  ungroup() %>% 
  ggplot(aes(x = race, y = mean_cate)) +
  geom_point()
  





# Plot Quartiles of CATE --------------------------------------------------


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
  
  g3_school <- star_short_term_x %>%
    filter(cate_quartile==q) %>%
    pull(g3schid)
  
  forest_q <- causal_forest(X_q, Y_q, W_q,
                            clusters = g3_school)
  
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
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=estimate-2*std_err, ymax=estimate+2*std_err), width=.2, position=position_dodge(0.2)) +
  ylab("") + xlab("CATE Quartile") +
  ggtitle("Average CATE vs. AIPW estimates by quartile") +
  theme_minimal() +
  theme(legend.position="bottom", legend.title = element_blank())


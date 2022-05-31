



rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

# Load packages
library(pacman)
p_load(tidyverse, haven, readr, janitor, grf, labelled, plm, sandwich, lmtest, glue)

options(stringAsFactors=FALSE)

# Load cleaned student-level data
load("./data/cate_estimates_short_term_g1treadss.rda")

# Drop small class size flags


# Define short-term: grade 3 outcomes
short_term_outcomes <- c("g1treadss", "g1tmathss", "g1wordskillss")



# Compare school-level averages to School-level AIPW  ---------------------

# CATE estimates using original causal forest, averaging by schools
school_cf_cate <- star_short_term_x %>% 
  group_by(g1schid) %>% 
  add_count() %>%
  summarize(cate_estimate = mean(cate_estimate),
            cate_se = sd(cate_estimate),
            n_students = max(n)) %>%
  ungroup()

hist(school_cf_cate$cate_estimate)


# Heterogeneity by School-Characteristics ---------------------------------

# Merge in g1 school level data
k3_schools <- read_sav("./data/STAR_K-3_Schools.sav")

k3_schools <- k3_schools %>% clean_names()

# Create quartiles of school size, percent bused, and free-lunch students
k3_schools <- k3_schools %>% 
  mutate(enrollment_quartile = ntile(g1enrmnt, 4),
         bused_quartile = ntile(g1bused, 4),
         lunch_quartile = ntile(g1frlnch, 4),
         minority_school = if_else(g1white<50, 1, 0))

star_short_term_x <- star_short_term_x %>% 
  left_join(k3_schools, by = c("g1schid" = "schid"))


ggplot(star_short_term_x, aes(enrollment_quartile, lunch_quartile, fill = cate_estimate)) +
  geom_raster() + 
  theme_light()

ggplot(star_short_term_x, aes(enrollment_quartile, bused_quartile, fill = cate_estimate)) +
  geom_raster() + 
  theme_light()

ggplot(star_short_term_x, aes(enrollment_quartile, minority_school, fill = cate_estimate)) +
  geom_raster() + 
  theme_light()

star_short_term_x %>% 
  ggplot(aes(x = factor(lunch_quartile), y = cate_estimate)) +
  geom_boxplot() + 
  geom_smooth(se=TRUE, aes(group=1), method="loess") +
  theme_light() + 
  xlab("Free Lunch Quartile") +
  ylab("CATE estimate")

star_short_term_x %>% 
  ggplot(aes(x = factor(enrollment_quartile), y = cate_estimate)) +
  geom_boxplot() + 
  geom_smooth(se=TRUE, aes(group=1), method="loess") +
  theme_light() + 
  xlab("School Size Quartile") +
  ylab("CATE estimate")







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

#ATE via linear regression (complex models)
bivariate_ols <- lm(g7tmathss ~ W 
                    + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    + as.factor(g4schid)
                    + as.factor(g5schid)
                    + as.factor(g6schid)
                    + as.factor(g7schid), data=star_g7_math)
summary(bivariate_ols)

yearssmall_ols <- lm(g7tmathss ~ as.factor(yearssmall) 
                     + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid)
                     + as.factor(g4schid)
                     + as.factor(g5schid)
                     + as.factor(g6schid)
                     + as.factor(g7schid), data=star_g7_math)
summary(yearssmall_ols)

ksmall_ols <- lm(g7tmathss ~ gk_smallclass
                 + as.factor(gkschid)
                 + as.factor(g1schid)
                 + as.factor(g2schid)
                 + as.factor(g3schid)
                 + as.factor(g4schid)
                 + as.factor(g5schid)
                 + as.factor(g6schid)
                 + as.factor(g7schid), data=star_g7_math)
summary(ksmall_ols)

g1small_ols <- lm(g7tmathss ~ g1_smallclass
                  + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid), data=star_g7_math)
summary(g1small_ols)

g2small_ols <- lm(g7tmathss ~ g2_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid)
                  , data=star_g7_math)
summary(g2small_ols)

g3small_ols <- lm(g7tmathss ~ g3_smallclass 
                  + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid), data=star_g7_math)
summary(g3small_ols)


#ATE via linear regression (complex models)
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

#ATE via linear regression (simple models)
bivariate_ols <- lm(g7treadss ~ W 
                    + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    + as.factor(g4schid)
                    + as.factor(g5schid)
                    + as.factor(g6schid)
                    + as.factor(g7schid), data=star_g7_reading)
summary(bivariate_ols)

yearssmall_ols <- lm(g7treadss ~ as.factor(yearssmall) + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid)
                     + as.factor(g4schid)
                     + as.factor(g5schid)
                     + as.factor(g6schid)
                     + as.factor(g7schid), data=star_g7_reading)
summary(yearssmall_ols)

ksmall_ols <- lm(g7treadss ~ gk_smallclass + as.factor(gkschid)
                 + as.factor(g1schid)
                 + as.factor(g2schid)
                 + as.factor(g3schid)
                 + as.factor(g4schid)
                 + as.factor(g5schid)
                 + as.factor(g6schid)
                 + as.factor(g7schid), data=star_g7_reading)
summary(ksmall_ols)

g1small_ols <- lm(g7treadss ~ g1_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid), data=star_g7_reading)
summary(g1small_ols)

g2small_ols <- lm(g7treadss ~ g2_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid), data=star_g7_reading)
summary(g2small_ols)

g3small_ols <- lm(g7treadss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  + as.factor(g4schid)
                  + as.factor(g5schid)
                  + as.factor(g6schid)
                  + as.factor(g7schid), data=star_g7_reading)
summary(g3small_ols)


#ATE via linear regression (complex models)
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

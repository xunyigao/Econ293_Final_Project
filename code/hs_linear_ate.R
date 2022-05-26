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

#ATE via linear regression (simple models)
bivariate_ols <- lm(hsgpaoverall ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    + as.factor(g4schid)
                    + as.factor(g5schid)
                    + as.factor(g6schid)
                    + as.factor(g7schid)
                    + as.factor(g8schid)
                    , data=star_hs_1)
summary(bivariate_ols)

yearssmall_ols <- lm(hsgpaoverall ~ as.factor(yearssmall) 
                     + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid)
                     , data=star_hs_1)
summary(yearssmall_ols)

ksmall_ols <- lm(hsgpaoverall ~ gk_smallclass +as.factor(gkschid)
                 , data=star_hs_1)
summary(ksmall_ols)

g1small_ols <- lm(hsgpaoverall ~ g1_smallclass 
                  + as.factor(gkschid)
                  + as.factor(g1schid)
                  , data=star_hs_1)
summary(g1small_ols)

g2small_ols <- lm(hsgpaoverall ~ g2_smallclass+as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  , data=star_hs_1)
summary(g2small_ols)

g3small_ols <- lm(hsgpaoverall ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  , data=star_hs_1)
summary(g3small_ols)



#ATE via linear regression (complex models)
bivariate_ols <- lm(hsgpaoverall ~ W + as.factor(gkschid)
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
                    + as.factor(g1speced), data=star_hs_1)
summary(bivariate_ols)

yearssmall_ols <- lm(hsgpaoverall ~ as.factor(yearssmall) 
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
                     + as.factor(g1speced), data=star_hs_1)
summary(yearssmall_ols)

ksmall_ols <- lm(hsgpaoverall ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_hs_1)
summary(ksmall_ols)

g1small_ols <- lm(hsgpaoverall ~ g1_smallclass 
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
                  + as.factor(g1speced), data=star_hs_1)
summary(g1small_ols)

g2small_ols <- lm(hsgpaoverall ~ g2_smallclass+as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_hs_1)
summary(g2small_ols)

g3small_ols <- lm(hsgpaoverall ~ g3_smallclass + as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_hs_1)
summary(g3small_ols)

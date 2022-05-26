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

#3RD GRADE MATH
#ATE via linear regression (simple models)
bivariate_ols <- lm(g3tmathss ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid), data=star_short_term_1)
summary(bivariate_ols)

yearssmall_ols <- lm(g3tmathss ~ as.factor(yearssmall) 
                     + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid), data=star_short_term_1)
summary(yearssmall_ols)

ksmall_ols <- lm(g3tmathss ~ gk_smallclass + as.factor(gkschid)
                 + as.factor(g1schid)
                 + as.factor(g2schid)
                 + as.factor(g3schid), data=star_short_term_1)
summary(ksmall_ols)

g1small_ols <- lm(g3tmathss ~ g1_smallclass 
                  + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_1)
summary(g1small_ols)

g2small_ols <- lm(g3tmathss ~ g2_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid)
                  , data=star_short_term_1)
summary(g2small_ols)

g3small_ols <- lm(g3tmathss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_1)
summary(g3small_ols)

#ATE via linear regression (complex models)
bivariate_ols <- lm(g3tmathss ~ W + as.factor(gkschid)
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
                    + as.factor(g1speced), data=star_short_term_1)
summary(bivariate_ols)

yearssmall_ols <- lm(g3tmathss ~ as.factor(yearssmall) 
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
                     + as.factor(g1speced), data=star_short_term_1)
summary(yearssmall_ols)

ksmall_ols <- lm(g3tmathss ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_short_term_1)
summary(ksmall_ols)

g1small_ols <- lm(g3tmathss ~ g1_smallclass 
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g1small_ols)

g2small_ols <- lm(g3tmathss ~ g2_smallclass+as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g2small_ols)

g3small_ols <- lm(g3tmathss ~ g3_smallclass + as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g3small_ols)

#3RD GRADE READING
star_short_term_2 <- star_short_term %>% 
  filter(!is.na(g3treadss)) 

# Define outcome and treatment vectors
W <- star_short_term_1$W

outcome <- short_term_outcomes[1]

Y <- star_short_term_2 %>% pull(outcome) %>% as.vector()

star_short_term_x <- star_short_term_2 %>% 
  select(-W, -stdntid) %>% 
  select(!contains(c(short_term_outcomes)))

#ATE via linear regression
bivariate_ols <- lm(g3treadss ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid)
                    , data=star_short_term_2)
summary(bivariate_ols)

yearssmall_ols <- lm(g3treadss ~ as.factor(yearssmall) + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid), data=star_short_term_2)
summary(yearssmall_ols)

ksmall_ols <- lm(g3treadss ~ gk_smallclass + as.factor(gkschid)
                 + as.factor(g1schid)
                 + as.factor(g2schid)
                 + as.factor(g3schid), data=star_short_term_2)
summary(ksmall_ols)

g1small_ols <- lm(g3treadss ~ g1_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g1small_ols)

g2small_ols <- lm(g3treadss ~ g2_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g2small_ols)

g3small_ols <- lm(g3treadss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g3small_ols)

#ATE via linear regression (complex models)
bivariate_ols <- lm(g3tmathss ~ W + as.factor(gkschid)
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
                    + as.factor(g1speced), data=star_short_term_1)
summary(bivariate_ols)

yearssmall_ols <- lm(g3tmathss ~ as.factor(yearssmall) 
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
                     + as.factor(g1speced), data=star_short_term_1)
summary(yearssmall_ols)

ksmall_ols <- lm(g3tmathss ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_short_term_1)
summary(ksmall_ols)

g1small_ols <- lm(g3tmathss ~ g1_smallclass 
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g1small_ols)

g2small_ols <- lm(g3tmathss ~ g2_smallclass+as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g2small_ols)

g3small_ols <- lm(g3tmathss ~ g3_smallclass + as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_1)
summary(g3small_ols)

#3RD GRADE READING
star_short_term_2 <- star_short_term %>% 
  filter(!is.na(g3treadss)) 

# Define outcome and treatment vectors
W <- star_short_term_1$W

outcome <- short_term_outcomes[1]

Y <- star_short_term_2 %>% pull(outcome) %>% as.vector()

#ATE via linear regression (simple models)
bivariate_ols <- lm(g3treadss ~ W + as.factor(gkschid)
                    + as.factor(g1schid)
                    + as.factor(g2schid)
                    + as.factor(g3schid), data=star_short_term_2)
summary(bivariate_ols)

yearssmall_ols <- lm(g3treadss ~ as.factor(yearssmall) + as.factor(gkschid)
                     + as.factor(g1schid)
                     + as.factor(g2schid)
                     + as.factor(g3schid), data=star_short_term_2)
summary(yearssmall_ols)

ksmall_ols <- lm(g3treadss ~ gk_smallclass + as.factor(gkschid)
                 + as.factor(g1schid)
                 + as.factor(g2schid)
                 + as.factor(g3schid), data=star_short_term_2)
summary(ksmall_ols)

g1small_ols <- lm(g3treadss ~ g1_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g1small_ols)

g2small_ols <- lm(g3treadss ~ g2_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g2small_ols)

g3small_ols <- lm(g3treadss ~ g3_smallclass + as.factor(gkschid)
                  + as.factor(g1schid)
                  + as.factor(g2schid)
                  + as.factor(g3schid), data=star_short_term_2)
summary(g3small_ols)


#ATE via linear regression (complex models)
bivariate_ols <- lm(g3treadss ~ W + as.factor(gkschid)
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
                    + as.factor(g1speced), data=star_short_term_2)
summary(bivariate_ols)

yearssmall_ols <- lm(g3treadss ~ as.factor(yearssmall) 
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
                     + as.factor(g1speced), data=star_short_term_2)
summary(yearssmall_ols)

ksmall_ols <- lm(g3treadss ~ gk_smallclass +as.factor(gkschid)
                 + as.factor(race)
                 + as.factor(gksurban)
                 + as.factor(gkthighdegree)
                 + as.factor(gkfreelunch)
                 + as.factor(gkrepeat)
                 + gktyears
                 + as.factor(gkspeced), data=star_short_term_2)
summary(ksmall_ols)

g1small_ols <- lm(g3treadss ~ g1_smallclass 
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
                  + as.factor(g1speced), data=star_short_term_2)
summary(g1small_ols)

g2small_ols <- lm(g3treadss ~ g2_smallclass+as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_2)
summary(g2small_ols)

g3small_ols <- lm(g3treadss ~ g3_smallclass + as.factor(gkschid)
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
                  + as.factor(g1speced), data=star_short_term_2)
summary(g3small_ols)
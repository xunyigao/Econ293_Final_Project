
rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

library(pacman)
library(grf)
library(fastDummies)
library(labelled)
p_load(tidyverse, haven, readr, janitor)

options(stringAsFactors=FALSE)

star <- read_sav("./data/STAR_Students.sav")
names(star) <- tolower(names(star))

#1. creating indicator variable for treatment group; 1 = in small class for at least a year in grades kinder - 3rd
star <- star %>% 
  mutate(W = if_else(gkclasstype==1 | g1classtype==1 | g2classtype==1 | g3classtype==1, 1, 0)) %>% 
  mutate(W = if_else(is.na(W), 0, W))

#2. creating indicators for what year students were in small classes. 1 = in small class, 0 = not in small class
star$gk_smallclass <- case_when(star$gkclasstype == 1 ~ 1,
                                ((star$gkclasstype == 2) | (star$gkclasstype == 3)) ~ 0)

star$g1_smallclass <- case_when(star$g1classtype == 1 ~ 1,
                                ((star$g1classtype == 2) | (star$g1classtype == 3)) ~ 0)

star$g2_smallclass <- case_when(star$g2classtype == 1 ~ 1,
                                ((star$g2classtype == 2) | (star$g2classtype == 3)) ~ 0)

star$g3_smallclass <- case_when(star$g3classtype == 1 ~ 1,
                                ((star$g3classtype == 2) | (star$g3classtype == 3)) ~ 0)


# Load K-3 school level data
k3_schools <- read_sav("./data/STAR_K-3_Schools.sav")

k3_schools <- k3_schools %>% clean_names()

# create a duplicate of school data to merge into student data for each year of k-3
k3_k_schools <- k3_schools


colnames(k3_k_schools) <- paste(colnames(k3_k_schools), "k", sep = "_")

k3_g1_schools <- k3_schools

colnames(k3_g1_schools) <- paste(colnames(k3_g1_schools), "g1", sep = "_")

k3_g2_schools <- k3_schools

colnames(k3_g2_schools) <- paste(colnames(k3_g2_schools), "g2", sep = "_")

k3_g3_schools <- k3_schools

colnames(k3_g3_schools) <- paste(colnames(k3_g3_schools), "g3", sep = "_")

# Merge-in school data separately for each grade the student was in K-3 

star <- star %>% 
  left_join(k3_k_schools, by = c("gkschid" = "schid_k")) %>% 
  left_join(k3_g1_schools, by = c("g1schid" = "schid_g1")) %>% 
  left_join(k3_g2_schools, by = c("g2schid" = "schid_g2")) %>% 
  left_join(k3_g3_schools, by = c("g3schid" = "schid_g3"))

# Drop variables with too many missing observations

  
# Drop observations which have key variables missing
na_counts = map(star, ~sum(is.na(.)))

# Make categorical variables dummy - update with more covariates that are categorical when covars are decided

#star <- dummy_cols(star, select_columns = c("race", "gender", "g3surban", "g3tgen", "g3trace", "g3thighdegree", "g3tcareer", "g3ttrain", "g3freelunch"))

#SHORT TERM OUTCOMES
# Define short-term: grade 3 outcomes
short_term_outcomes <- c("g3treadss", "g3tmathss", "g3readbsraw",
                         "g3mathbsraw")

# List of variables to drop whe predicting short-term outcomes
drop_for_short_term <- c("g3tlistss", "g3sciencess", "g3socialsciss", "g3spellss",
                         "g3vocabss", "g3mathcomputss", "g3mathnumconcss", 
                         "g3mathapplss", "g3wordskillss", "g3mathbsobjraw", 
                         "g3mathbsobjpct", "g3readbsobjpct")

# Drop the other grade 3 outcomes from the list above
star_short_term <- star[,!(names(star) %in% drop_for_short_term)]

# Drop future (beyond grade 3) outcomes and characteristics
star_short_term <- star_short_term %>% 
  select(!starts_with(c("g4", "g5", "g6", "g7", "g8" , "hs")))

# LONG TERM OUTCOMES
#7TH GRADE
# Define one long term outcome: 7th grade outcomes
g7_outcomes <- c("g7treadss", "g7tmathss")

# List of variables to drop when predicting 7th grade outcomes
drop_for_g7 <- c("g7tlangss","g7tbattss", "g7sciencess", "g7socialsciss",
                 "g7readcomprehss", "g7spellss", "g7vocabss", "g7mathcomputss",
                 "g7mathconcapplss", "g7langexpss", "g7langmechss", 
                 "g7studyskillss", "g7readbsobjraw", "g7mathbsobjraw")

# Drop the other grade 7 outcomes from the list above
star_g7 <- star[,!(names(star) %in% drop_for_g7)]

# Drop future (beyond grade 7) outcomes and characteristics)
star_g7 <- star_g7 |>
  select(!starts_with(c("g8", "hs")))

# high school GPA
hs_outcomes <- c("hsgpaoverall")

#drop other HS GPA outcomes
drop_for_hs <- c("hsgpaflang", "hsgpamath", "hsgpascience")

# Drop the other HS GPA outcomes from the list above
star_hs <- star[,!(names(star) %in% drop_for_hs)]


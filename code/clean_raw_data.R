
rm(list = ls())

# Set working directory
setwd("~/Documents/Github/Econ293_Final_Project/")

library(pacman)
p_load(tidyverse, haven, readr, janitor)

options(stringAsFactors=FALSE)

star <- read_sav("./data/STAR_Students.sav")
names(star) <- tolower(names(star))

#1. creating indicator variable for treatment group; 1 = in small class for at least a year in grades kinder - 3rd
star$W <- ifelse(((star$gkclasstype == 1) | (star$g1classtype == 1) | (star$g2classtype) == 1 | (star$g3classtype == 1)), 1, 0)

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
  





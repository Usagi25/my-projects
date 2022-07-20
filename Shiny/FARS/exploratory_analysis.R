# Analysis
# July 20th, 2022

library(tidyverse)

# crash <- read.csv("accident.csv")
person <- read.csv("person.csv")

# Find subset of crashes with pedestrians
peds <- person %>%
  filter(ST_CASE %in% unique(person[which(person$PER_TYPNAME=="Pedestrian"), "ST_CASE"]))
# Aoc 2024 Day 1

library(magrittr)
library(dplyr)

dat <- read.csv2("../data/D1.txt",sep = " ",header = FALSE)

# A
dat %<>% mutate(
  V1=sort(V1),
  V4=sort(V4),
  delta=abs(V1-V4)
  )
dat %>% summarise(sum(delta))

# B
d1 <- dat %>% group_by(V4) %>% count %>% ungroup
comp <- dat %>% pull(V1)
d1 %>% filter(V4 %in% comp) %>% 
  summarise(sum(V4*n))

# Aoc 2024 Day 13

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat2 <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D13.txt",sep = ",",header = FALSE)

# data
dat2$V1 <- cutBefore(dat2$V1,":")
dat2$V1 <- cutBefore(dat2$V1,"=")
dat2$V2 <- cutBefore(dat2$V2,"=")

machines <- dat2 %>% filter(row_number()%%3==0) %>% 
  rename(X=V1,Y=V2)
mA <- dat2 %>% filter(row_number()%%3==1) %>%
  mutate(V1=cutBefore(V1,"\\+"),
         V2=cutBefore(V2,"\\+")) %>% 
  rename(XA=V1,YA=V2)
mB <- dat2 %>% filter(row_number()%%3==2) %>%
  mutate(V1=cutBefore(V1,"\\+"),
         V2=cutBefore(V2,"\\+")) %>% 
  rename(XB=V1,YB=V2)

machines %<>% bind_cols(mA,mB)
machines %<>% mutate_all(as.numeric)

# A
machines %<>% mutate(det = XA*YB-XB*YA)
machines %>% summarise(sum(det==0))
# All dets are !=0 
machines %<>% mutate(
  aa = (YB*X-XB*Y) %% det,
  bb = (-YA*X+XA*Y) %% det,
  a = (YB*X-XB*Y) %/% det,
  b = (-YA*X+XA*Y) %/% det,
  cost = (3*a+b)*(aa==0)*(bb==0)
)
machines %>% summarise(sum(cost))
# 27157 

# B
machines %<>% mutate(X=X+10000000000000,Y=Y+10000000000000)

machines %<>% mutate(det = XA*YB-XB*YA,
                     aa=(YB*X-XB*Y) %% det,
                     bb=(-YA*X+XA*Y) %% det,
                     a = (YB*X-XB*Y) %/% det,
                     b =(-YA*X+XA*Y) %/% det,
                     cost=(3*a+b)*(aa==0)*(bb==0)
)
options(digits = 20)
machines %>% summarise(sum(cost))
# 104015411578548

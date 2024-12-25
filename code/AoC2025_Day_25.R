# Aoc 2024 Day 25

  ################
 # Merry Xmas ! #
################

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

##########################################################
# Data
  
dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D25.txt")
which(dat=="") 

locks <- tibble(id_l=integer(),x1=integer(),x2=integer(),x3=integer(),x4=integer(),x5=integer())
keys <- tibble(id_k=integer(),x1=integer(),x2=integer(),x3=integer(),x4=integer(),x5=integer())
idl <- 0
idk <- 0
for(i in (0:499)*8+1){
  sd <- dat[i:(i+6)]
  type <- ifelse(sd[1]==".....","key","lock")
  vals <- df_from_vector_of_strings(sd) %>% summarise_all(function(x)sum(x=="#"))
  if(type=="lock"){
    idl <- idl+1
    locks %<>% bind_rows(vals %>% mutate(id_l=idl))
  } 
  if(type=="key"){
    idk <- idk+1
    keys %<>% bind_rows(vals %>% mutate(id_k=idk))
  }
}

##########################################################
# A

locks %>% 
  cross_join(keys) %>% 
  mutate(valid=
           x1.x + x1.y < 8 &
           x2.x + x2.y < 8 &
           x3.x + x3.y < 8 &
           x4.x + x4.y < 8 &
           x5.x + x5.y < 8
  ) %>% 
  summarise(sum(valid))
#

##########################################################
# B

print("Merry Christmas !")
# Finally the historians are useful for something

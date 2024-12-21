# Aoc 2024 Day 5

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

manuals <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D5_manuals.txt")
manuals <- manuals[-1]
rules <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D5_rules.txt",sep = "|",header = FALSE)

# A

read_manual <- function(manual) str_split(manual,",") %>% unlist %>% as.integer

check_rule <- function(V1,V2,m){
  if(V1 %in% m & V2 %in% m) if(which(m==V1)>which(m==V2)) return(FALSE)
  TRUE
}

value_manual <- function(i){
  m <- read_manual(manuals[i])
  if(all(pmap_lgl(.l = rules,.f = function(V1,V2){check_rule(V1,V2,m)}))){
    return(m[(length(m)+1)/2])
  }
  return(0)
}
map(.x = 1:length(manuals),.f = function(x) value_manual(x)) %>% reduce(sum)

# 5091


# B

value_manual_B <- function(i){
  m <- read_manual(manuals[i])
  if(all(pmap_lgl(.l = rules,.f = function(V1,V2){check_rule(V1,V2,m)}))){
    return(0)
  }
  while(!all(pmap_lgl(.l = rules,.f = function(V1,V2){check_rule(V1,V2,m)}))){
    for(j in 1:nrow(rules)){
      V1 <- rules[j,1]
      V2 <- rules[j,2]
      if(!check_rule(V1,V2,m)){
        m[m==V1] <- 0
        m[m==V2] <- V1
        m[m==0] <- V2
      #  j <- 1 # not necessary but saves a bit of execution time
      }
    }
  }
  return(m[(length(m)+1)/2])
}

map(.x = 1:length(manuals),.f = function(x) value_manual_B(x)) %>% reduce(sum)

# 4681

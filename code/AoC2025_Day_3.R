# Aoc 2024 Day 3

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D3.txt")
dat <- paste0(dat,collapse = "")

# A

gg <- gregexpr("mul\\([0-9]{1,3}\\,[0-9]{1,3}\\)",dat)
places <- gg[[1]]
ll <- attributes(places)$match.length

calc_prod <- function(i){
  ss <- substr(dat,places[i]+4,places[i]+ll[i]-2)
  a <- cutAfter(ss,",") %>% as.integer
  b <- cutBefore(ss,",") %>% as.integer
  a*b
}

map(.x =1:length(places),.f = calc_prod) %>% 
  reduce(sum)

# B

dos <- gregexpr("do\\(\\)",dat)[[1]]
donts <- gregexpr("don\\'t\\(\\)",dat)[[1]]
calc_redo <- function(x){
  dos[dos>x][1]
}
tab <- tibble(stop=donts) %>%
  rowwise %>% 
  mutate(redo=calc_redo(stop)) %>% 
  ungroup %>% 
  filter(!duplicated(redo))

valid_place <- function(x){
  sum(tab$stop<x)==sum(tab$redo<x)
}
valid_place(5)
valid_place(668)

calc_prod_2 <- function(x){
  if (valid_place(places[x])) return(calc_prod(x))
  0
}
calc_prod(668)
calc_prod_2(668)

map(.x =1:length(places),.f = calc_prod_2) %>% 
  reduce(sum)

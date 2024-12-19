# Aoc 2024 Day 19

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

towels <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D19t.txt",sep = ",",header = FALSE) %>% as.matrix %>% t
towels <- towels[,1] %>% unname()
towels <- sapply(towels,rmFirstChar)

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D19.txt",sep = " ",header = FALSE)
pats <- dat$V1

##########################################################
# A

pat <- pats[1]
string_after <- function(tow,pat){
  substr(pat,nchar(tow)+1,nchar(pat))
}
string_after("brw",pat)

is_poss <- function(pat){
  #print(pat)
  if(pat=="") return(TRUE)
  possib <- sapply(towels, function(t)grepl(pattern = paste0("^",t),x=pat))
  if(!any(possib)) return(FALSE)
  my_tow <- towels[possib]
  new_pats <- sapply(my_tow,function(x)string_after(tow=x,pat=pat))
  while(length(new_pats>0)){
    if(is_poss(new_pats[1])) return(TRUE)
    new_pats <- new_pats[-1]
  }
  # return(map(new_pats,is_poss) %>% reduce(any)) # too slow to check every combination
  return(FALSE)
}
is_poss("gruguburu")

map(pats,is_poss) %>% reduce(sum)
# 209

##########################################################
# B

# save intermediate results
prev <- tibble(x="",nb=1)
nb_poss <- function(pat){
  if(pat %in% prev$x) return(prev$nb[prev$x==pat])
  possib <- sapply(towels, function(t)grepl(pattern = paste0("^",t),x=pat))
  if(!any(possib)) return(0)
  my_tow <- towels[possib]
  new_pats <- sapply(my_tow,function(x)string_after(tow=x,pat=pat))
  val <- map(new_pats,nb_poss) %>% reduce(sum)
  prev <<- prev %>% add_row(x=pat,nb=val)
  return(val)
}
nb_poss("brwbrwbrwbbrw")

map(pats,nb_poss) %>% reduce(sum)
# 777669668613191

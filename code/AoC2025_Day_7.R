# Aoc 2024 Day 7

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D7.txt",sep = " ",header = FALSE)

dat %<>% mutate(V1=(cutAfter(V1,":")),
                V1=as.numeric(V1)) %>% 
  as.matrix

# A
eval_row <- function(v){
  if(v[3]==0) return(v[1]==v[2])
  v1 <- v
  v1[2] <- v1[2] + v1[3]
  v1 <- v1[-3]
  v2 <- v
  v2[2] <- v2[2] * v2[3]
  v2 <- v2[-3]
  eval_row(v1) | eval_row(v2)
}
eval_row(c(292,11,6,16,20,0))  

restab <- tibble(i=numeric(),val=numeric(),eval=logical())
res <- 0
for(i in 1:nrow(dat)){
  row <- c(coalesce(dat[i,],0),0)
  restab <- restab %>% add_row(i=i,val=row[1],eval=eval_row(row))
  res <- res + eval_row(row) * row[1]
}
res

# 12940396350192 

# B
eval_row_B <- function(v){
  #  print(v)
  if(v[3]==0) return(v[1]==v[2])
  v1 <- v
  v1[2] <- v1[2] + v1[3]
  v1 <- v1[-3]
  v2 <- v
  v2[2] <- v2[2] * v2[3]
  v2 <- v2[-3]
  v3 <- v
  val <- as.numeric(paste0(v3[2],v3[3]))
  if(is.na(val)) { print(val); print(v); return(FALSE) }
  v3[2] <- val
  v3 <- v3[-3]
  eval_row_B(v1) || eval_row_B(v2) || eval_row_B(v3) 
}
eval_row_B(c(292,11,6,16,20,0))  
eval_row_B(c(7290,6,8,6,15,0))  
row <- c(coalesce(dat[3,],0),0)
eval_row_B(row)

res <- 0
for(i in 1:nrow(dat)){
  # for(i in 1:20){
  print(i)
  if(restab[i,3] %>% unlist) next()
  row <- c(coalesce(dat[i,],0),0)
  res <- res + eval_row_B(row) * row[1]
  if(is.na(res)) break()
}
res + 12940396350192

# 106016735664498 

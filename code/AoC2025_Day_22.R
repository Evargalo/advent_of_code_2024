# Aoc 2024 Day 22

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data
dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D22.txt")
dat %<>% as.numeric

##########################################################
# A

library("bit64")

# adapt xor to big integers
bxl <- function(a,b){
  seuil <- 2^15
  if(a > seuil | b > seuil) return(
    seuil * (bxl(a %/% seuil, b %/% seuil)) +
      (bxl(a %% seuil, b %% seuil))
  )
  bin_a <- intToBits(a)
  bin_b <- intToBits(b)
  xor(bin_a,bin_b) %>% packBits(type = "integer")
}
bxl(42,15)

prune <- function(x) x %% 16777216
prune(100000000)+.1

change_secret <- function(x){
  y <- bxl(64*x,x) %>% prune
  z <- bxl (y %/% 32, y) %>% prune
  bxl(2048*z,z) %>% prune
}
change_secret(123)

change_2000 <- function(x){
  for(i in 1:2000){
    if(i%%100==0) print(i)
    x <- change_secret(x)
  }
  x
}
change_2000(1)

s <- sapply(X = dat,FUN = change_2000)
sum(s)
# 17965282217

##########################################################
# B

res <- tibble(monkey=integer(),step=integer(),secret_number=numeric())

change_2000 <- function(i){
  print(i)
  x <- dat[i]
  v <- x
  for(j in 1:2000){
    y <- suppressMessages(change_secret(x))
    v <- c(v,y)
    x <- y
  }
  res <<- res %>% bind_rows(tibble(monkey=i,step=0:2000,secret_number=v))
}

for(i in 1:(length(dat))){
  change_2000(i)  
}

res %<>% group_by(monkey) %>% 
  mutate(price=secret_number %% 10, 
         change=price-lag(price),
         l1=lag(change,1,default = 10),
         l2=lag(change,2,default = 20),
         l3=lag(change,3,default = 30),
         seq=paste(l3,l2,l1,change)
  ) %>% ungroup

res2 <- res %>% filter(step>=4) 
res2 %<>% group_by(seq,monkey) %>% 
  filter(row_number()==1) %>% 
  ungroup 
res2 %>% 
  group_by(seq) %>% summarise(p=sum(price)) %>% ungroup %>% 
  summarise(max(p))
# 2152


#######################################################################
# B alternative with memoizing - not really faster
res <- tibble(monkey=integer(),step=integer(),secret_number=numeric())
mem <- tibble(prev=numeric(),sn=numeric())
change_secret_2 <- function(x){
  sub_mem <- mem %>% filter(prev==x)
  if(nrow(sub_mem) == 1) return(sub_mem %>% pull(sn) %>% unlist())
  y <- change_secret(x)
  mem <<- add_row(mem,prev=x,sn=y)
  return(y)
}

change_2000_2 <- function(i){
  print(i)
  x <- dat[i]
  v <- x
  for(j in 1:2000){
    y <- suppressMessages(change_secret_2(x))
    v <- c(v,y)
    x <- y
  }
  res <<- res %>% bind_rows(tibble(monkey=i,step=0:2000,secret_number=v))
}
for(i in 1:(length(dat))){
  change_2000_2(i)  
}
# Aoc 2024 Day 2

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/D2.txt",sep = " ",header = FALSE)
m <- as.matrix(dat) %>% unname

assess_report <- function(r){
  step <- r-lag(r,)
  step <- step[!is.na(step)]
  all(step %in% 1:3) || all(step %in% -(1:3))
}

# A
res <- 0
for(i in 1:nrow(m)){
  r <- m[i,]
  if(assess_report(r)) res <- res+1
}
res

# B
res <- 0
for(i in 1:nrow(m)){
  r <- m[i,]
  win <- assess_report(r)
  j <- 1
  while(!win & j<9){
    win <- assess_report(r[-j])
    j <- j+1
  }
  if(win) res <- res+1
}
res

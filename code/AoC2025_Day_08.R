# Aoc 2024 Day 8

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D8.txt",header = FALSE)

maze <- buildMaze(dat$V1)

m <- maze$Maze
drawMaze(m)

maze$foundChar

ncol <- max(m$x)
nrow <- max(m$y)


# A

antinodes <- tibble(x=integer(),y=integer())

for(i in 1:length(maze$foundChar)){
  spot <- maze$foundChar[i]
  if(spot==".") next()
  tab <- maze[[i+2]]
  tab %<>% 
    rename(xx=x,yy=y) %>% 
    cross_join(tab) %>% 
    filter(x!=xx | y!=yy) %>% 
    mutate(xa=2*xx-x,ya=2*yy-y,xb=2*x-xx,yb=2*y-yy)
  antinodes %<>% bind_rows(tab %>% select(x=xa,y=ya),tab %>% select(x=xb,y=yb))
}

antinodes %<>% filter(x>0,y>0,x<=nrow,y<=ncol) %>% unique

nrow(antinodes)

# 278

# B

antinodes <- tibble(x=integer(),y=integer())

for(i in 1:length(maze$foundChar)){
  spot <- maze$foundChar[i]
  if(spot==".") next()
  tab <- maze[[i+2]]
  tab %<>% 
    rename(xx=x,yy=y) %>% 
    cross_join(tab) %>% 
    filter(x!=xx | y!=yy) %>% 
    mutate(dx=xx-x,dy=yy-y)
  for(j in 1:nrow(tab)){
  an <- tibble(jump=(-50):50,
               x=tab$x[j]+jump*tab$dx[j],
               y=tab$y[j]+jump*tab$dy[j])
  antinodes %<>% bind_rows(an %>% select(x,y))
  }
}

antinodes %<>% filter(x>0,y>0,x<=nrow,y<=ncol) %>% unique

nrow(antinodes)
# 1067
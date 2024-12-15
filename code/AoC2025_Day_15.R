# Aoc 2024 Day 15

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

##########################################################
# Data
moves <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D15B.txt")
moves <- paste0(moves,collapse = "") %>% str_split_1(pattern = "")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D15.txt",sep = " ",header = FALSE)

maze <- buildMaze(dat$V1)
m <- maze$Maze
max(m$x)
max(m$y)
drawMaze(m)

##########################################################
# A

move <- function(m,dir){
  robot <- m %>% filter(t=="@")
  if(dir=="^") front <- m %>% filter(x<=robot$x,y==robot$y) %>% arrange(-x)
  if(dir=="v") front <- m %>% filter(x>=robot$x,y==robot$y) %>% arrange(x)
  if( dir=="<") front <- m %>% filter(x==robot$x,y<=robot$y) %>% arrange(-y)
  if( dir==">") front <- m %>% filter(x==robot$x,y>=robot$y) %>% arrange(y)
  front %<>% mutate(rk=row_number())
  if(all(front$t!=".")) return(m)
  firstwall <- front %>% filter(t=="#") %>% pull(rk) %>% head(1) %>% unlist
  firstdot <- front %>% filter(t==".") %>% pull(rk) %>% head(1) %>% unlist
  if(firstwall < firstdot) return(m)
  front %<>% filter(rk<=firstdot) %>% mutate(t=lag(t,default = ".")) %>% 
    select(-rk) 
  return(front %>% bind_rows(m %>% anti_join(front %>% select(x,y))))
}

i <- 0
for(dir in moves){
  suppressMessages(m <- move(m,dir))
  if(i %% 100 == 0) print(i)
  i<-i+1
}
m %>% filter(t=="O") %>% mutate(gps=100*(x-1)+y-1) %>% summarise(sum(gps))
# 1515788

##########################################################
# B

# bigger maze
m <- maze$Maze
m <- bind_rows(
  m %>% mutate(y=2*y-1,t=case_when(
    t=="O"~"[",
    TRUE~t)) ,
  m %>% mutate(y=2*y,t=case_when(
    t=="O"~"]",
    t=="@"~".",
    TRUE~t)) )
drawMaze(m,switch=TRUE)

no_cell <- m %>% filter(FALSE)
# tiles that will be moved when robot tries to go up
move_up <- function(mm){
  dest <- mm %>% mutate(x=x-1) %>% select(-t) %>% left_join(m)
  if(all(dest$t==".")) return(mm)
  if(any(dest$t=="#")) return(no_cell)
  dest2 <- dest %>% filter(t=="[") %>% mutate(y=y+1) %>% select(-t) %>% left_join(m)
  dest3 <- dest %>% filter(t=="]") %>% mutate(y=y-1) %>% select(-t) %>% left_join(m)
  dest %<>% bind_rows(dest2,dest3) %>% unique()
  ns <- move_up(dest %>% filter(t=="]" | t=="["))
  if(nrow(ns)>0) return(mm %>% bind_rows(ns))
  return(no_cell)
}
# tiles that will be moved when robot tries to go down
move_down <- function(mm){
  dest <- mm %>% mutate(x=x+1) %>% select(-t) %>% left_join(m)
  if(all(dest$t==".")) return(mm)
  if(any(dest$t=="#")) return(no_cell)
  dest2 <- dest %>% filter(t=="[") %>% mutate(y=y+1) %>% select(-t) %>% left_join(m)
  dest3 <- dest %>% filter(t=="]") %>% mutate(y=y-1) %>% select(-t) %>% left_join(m)
  dest %<>% bind_rows(dest2,dest3) %>% unique()
  ns <- move_down(dest %>% filter(t=="]" | t=="["))
  if(nrow(ns)>0) return(mm %>% bind_rows(ns))
  return(no_cell)
}

moveB <- function(m,dir){
  if( dir=="<" | dir==">") return(move(m,dir)) # easy case
  robot <- m %>% filter(t=="@")
  if(dir=="^") dest <- move_up(robot)
  if(dir=="v") dest <- move_down(robot)
  if(nrow(dest)==0) return(m)
  rempl <- dest %>% mutate(t=".")   # moving tiles are replaced by "." ...
  if(dir=="^") dest %<>% mutate(x=x-1)
  if(dir=="v") dest %<>% mutate(x=x+1)
  # ... but if they are already replaced by another moving tile
  dest %<>% bind_rows(rempl %>% anti_join(dest %>% select(x,y)))
  # everything else is unchanged in m
  m <- dest %>% bind_rows(m %>% anti_join(dest %>% select(x,y))) 
  return(m)
}

i <- 0
for(dir in moves){
  suppressMessages(m <- moveB(m,dir))
  if(i %% 100 == 0) print(i)
  i <- i+1
}
m %>% filter(t=="[") %>% 
  mutate(gps=100*(x-1)+y-1) %>% 
  summarise(sum(gps))
# 1516544

##########################################################

# Aoc 2024 Day 20

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D20.txt",sep = " ",header = FALSE)
mm <- buildMaze(dat$V1) 
m <- mm$Maze
drawMaze(m,TRUE)

##########################################################
# A

m %<>% filter(t != "#")
# Find the path from S to E by removing all deadends 
# actually it is useless with my data, all "." cells are in the path
tidy_m <- fill_de(m)
drawMaze(tidy_m,TRUE)

# get the order of passage on each cell of the path
road <- tidy_m %>% filter(t=="S") %>% mutate(d=0)
cur <- road 
while(cur$t!="E"){
  new_cur <- tidy_m %>% filter(abs(x-cur$x)+abs(y-cur$y)==1) %>% 
    mutate(d=cur$d+1)
  cur <- new_cur %>% anti_join(road,by=c("x","y"))
  road %<>% bind_rows(cur)
}  

# find the shortcuts
road %<>% select(-t)
cheats <- road %>%
  cross_join(road %>% rename(xx=x,yy=y,dd=d)) %>% 
  filter(dd>=d+102) %>% 
  filter(abs(xx-x)+abs(yy-y) == 2)

cheats %>% count
# 1360

##########################################################
# B

# nb of new cheats for each cheat length k
res <- tibble(k=2,nb_cheats=nrow(cheats))
for(k in 3:20){
  print(k)
  new_cheats <- road %>%
    cross_join(road %>% rename(xx=x,yy=y,dd=d)) %>% 
    filter(dd>=d+100+k) %>% 
    filter(abs(xx-x)+abs(yy-y) == k) %>% 
    anti_join(cheats,by=c("x","y","xx","yy"))
  res %<>% add_row(k=k,nb_cheats=nrow(new_cheats))
  cheats %<>% bind_rows(new_cheats)
}
sum(res$nb_cheats)
# 1005476


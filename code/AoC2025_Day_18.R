# Aoc 2024 Day 18

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D18.txt",sep = ",",header = FALSE)
names(dat) <- c("x","y")

##########################################################
# A
da <- dat %>% filter(row_number()<=1024)
m <- expand_grid(x=0:70,y=0:70,t=".")
nrow(m)
da %<>% mutate(t="#")
m <- bind_rows(da,m) %>% group_by(x,y) %>% filter(row_number()==1) %>% ungroup

drawMaze(m)
SW <- m %>% filter(x+y<70)
NE <- m %>% filter(x+y>=70) 

nm <- m
fill_de <- function(m){
  m -> newMaze
  newMaze %>% filter(t == "#") %>% select(x, y) -> blocks
  newMaze %>% filter(t == ".") -> canPass
  canPass$bw = pmap_int(canPass %>% select(x, y), countNeighbours, spots=blocks)
  canPass %>% filter((bw>2)) %>% select(x,y) %>% mutate(t="#") -> deadEnds
  while (nrow(deadEnds)>0) {
    print(nrow(deadEnds))
    newMaze %>% anti_join(deadEnds, by=c("x", "y")) %>% bind_rows(deadEnds) -> newMaze
    newMaze %>% filter(t == "#") %>% select(x, y) -> blocks
    newMaze %>% filter(t == ".") -> canPass
    canPass$bw = pmap_int(canPass %>% select(x, y), countNeighbours, spots=blocks)
    canPass %>% filter((bw>2)) %>% select(x,y) %>% mutate(t="#") -> deadEnds
    nm <<- newMaze
  }
  newMaze
}

nm <- fill_de(m)

drawMaze(nm)

find_cr <- function(m){
  m %>% filter(t == ".") %>% select(x, y) -> canPass
  canPass %>% select(xx=x, yy=y) -> crossRoads
  crossRoads$pw=pmap_int(crossRoads, countNeighbours, spots=canPass)
  crossRoads %<>% mutate(pw=pw-(xx==0) - (yy==0) -(xx==70) - (yy==70) ) 
  crossRoads %>% filter(pw>2)
}

cr <- find_cr(m)
cr2 <- find_cr(nm)

origin <- (tibble(x=0,y=0,t="."))
dest <- (tibble(x=70,y=70,t="."))

endPointChar <- "£"

walls <- "#"
ways <- "."

sm <- m %>% filter(x<30,y<30)

distanceInMaze <- function(m, origin, walls="#", ways="."){  
  endPointChar <- "£"
  # m <- mutate(t=if_else(x==70 & y==70,endPointChar,t))
  ways <- c(ways, endPointChar)
  newSmallMaze <- m %>% mutate(t=ifelse(x==origin$x & y==origin$y, 
                                        endPointChar, t))
  newSmallMaze %>% filter(t %in% ways) -> canPass
  current_step <- 0
  canPass %>% mutate(dist=ifelse(test = (t==endPointChar), current_step, -1)) -> canPass
  canPass %>% group_by(dist) %>% count
  newCalc <- canPass %>% filter(dist==current_step) %>% mutate(xx=x, yy=y)
  while(nrow(newCalc)>0 & current_step <145){
    print(current_step)
    current_step <- current_step+1
    pmap_dfr(.l=newCalc %>% select(xx, yy), .f=find4Neighbours, spots=(canPass %>% filter(dist==-1))) -> newCalc
    newCalc %>% mutate(dist=current_step) -> newCalc
    canPass %>% anti_join(newCalc, by=c("x", "y")) %>% bind_rows(newCalc) %>% unique ->canPass
    newCalc %<>% mutate(xx=x, yy=y)
  }
  return(canPass)
}

drawMaze(SW)

drawMaze(nm)
test <- distanceInMaze(nm,origin)
test2 <- distanceInMaze(nm,dest)

test %>% group_by(dist) %>% count

gf_tile(test,gformula = y~x,fill = ~dist)
gf_tile(test2,gformula = y~x,fill = ~dist)

tt <- bind_rows(test %>% filter(dist>-1),test2 %>% filter(dist>-1),nm %>% filter(t=="#") %>% mutate(dist=-20)) 

gf_tile(tt,gformula = y~x,fill = ~dist)

test %>% filter(x==70,y==70)

tab <- test %>% filter(dist==145) %>% rename(xx=x,yy=y) %>% 
  full_join(test2 %>% filter(dist==145))

tab %>% rowwise %>% mutate(dist=290+max(xx-x,yy-y)) %>% ungroup %>% group_by(dist) %>% count

tab %>% mutate(dist=290+abs(xx-x)+abs(yy-y)) %>% summarise(min(dist))

# 266 too low



##########################################################
# B

k <- 2024
fall <- function(k){
  da <- dat %>% filter(row_number()<=k)
  m <- expand_grid(x=0:70,y=0:70,t=".")
  da %<>% mutate(t="#")
  m <- bind_rows(da,m) %>% group_by(x,y) %>% filter(row_number()==1) %>% ungroup
  m
}
drawMaze(m)


#

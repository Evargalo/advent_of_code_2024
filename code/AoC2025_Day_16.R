# Aoc 2024 Day 16

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D16.txt",sep = " ",header = FALSE)

# dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D16test.txt",sep = " ",header = FALSE)

# dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D16testB.txt",sep = " ",header = FALSE)

ma <- buildMaze(dat$V1)
m <- ma$Maze
drawMaze(m)

goal <- ma$CharESpots
dep <- ma$CharSSpots
cr <- crossRoads(ma,"#",c(".","E","S")) %>% 
  bind_rows(goal %>% rename(xx=x,yy=y),
            dep %>% rename(xx=x,yy=y)) %>% 
  mutate(id_cr=row_number())
cr %<>% select(x=xx,y=yy,id_cr)

##########################################################
# A

# the interesting spots from xa,ya, vertically (ends of corridor and crossroads)
move_v <- function(xa,ya,l){
  corridor <- m %>% 
    filter(x == xa, y != ya) %>% 
    mutate(dist=abs(x-xa)+abs(y-ya)) %>% 
    arrange(dist) 
  left_wall <- corridor %>% filter(y<ya & t=="#") %>% filter(y==max(y)) %>% pull(y) %>% unlist
  right_wall <- corridor %>% filter(y>ya & t=="#") %>% filter(y==min(y)) %>% pull(y) %>% unlist
  corridor %<>% filter(y > left_wall & y < right_wall)
  res <- corridor %>% 
    filter(y == left_wall+1 | y == right_wall-1) %>% 
    bind_rows(
      corridor %>% left_join(cr,by=c("x","y")) %>% filter(!is.na(id_cr))
    ) %>% 
    mutate(l=dist+l) %>% 
    select(x,y,l)
  return(res)
}
move_v(dep$x,dep$y,0)

# the interesting spots from xa,ya, horizontally (ends of corridor and crossroads)
move_h <- function(xa,ya,l){
  corridor <- m %>% 
    filter(x != xa, y == ya) %>% 
    mutate(dist=abs(x-xa)+abs(y-ya)) %>% 
    arrange(dist) 
  left_wall <- corridor %>% filter(x<xa & t=="#") %>% filter(x==max(x)) %>% pull(x) %>% unlist
  right_wall <- corridor %>% filter(x>xa & t=="#") %>% filter(x==min(x)) %>% pull(x) %>% unlist
  corridor %<>% filter(x > left_wall & x < right_wall)
  res <- corridor %>% 
    filter(x == left_wall+1 | x == right_wall-1) %>% 
    bind_rows(
      corridor %>% left_join(cr,by=c("x","y")) %>% filter(!is.na(id_cr))
    ) %>% 
    mutate(l=dist+l) %>% 
    select(x,y,l)
  return(res)
}
move_h(dep$x,dep$y,0)

# x,y: position / l: score until x,y / dir= 0,1 -> horizontal,vertical
move <- function(x,y,l,dir){
  if(dir==1) res <- move_h(xa = x,ya = y,l)
  if(dir==0) res <- move_v(xa = x,ya = y,l)
  return(res %>% mutate(dir=1-dir))
}

# starting point, two possible directions
pos0 <- dep %>% mutate(l=0,dir=0) %>% 
  bind_rows(dep %>% mutate(l=1000,dir=1))
# where we have already gone
visited <- pos0 %>% select(x,y) %>% unique
# first move
pos <- pmap_dfr(pos0, move)
i <- 1
while(pos %>% inner_join(goal,by=c("x","y")) %>% nrow == 0){
  pos %<>% anti_join(visited,by=c("x","y")) %>% unique
  visited %<>% bind_rows(pos %>% select(x,y)) %>% unique
  pos %<>% mutate(l=1000+l)
  pos <- pmap_dfr(pos, move)
  i <- i+1
  if(i %% 5==0) {print(i);print(nrow(pos));print(nrow(visited))} 
}
pos %>% inner_join(goal,by=c("x","y")) %>% summarise(min(l))
# 94436

##########################################################
# B

# keep track of intermediate steps
move_v <- function(xa,ya,l){
  corridor <- m %>% 
    filter(x == xa, y != ya) %>% 
    mutate(dist=abs(x-xa)+abs(y-ya)) %>% 
    arrange(dist) 
  left_wall <- corridor %>% filter(y<ya & t=="#") %>% filter(y==max(y)) %>% pull(y) %>% unlist
  right_wall <- corridor %>% filter(y>ya & t=="#") %>% filter(y==min(y)) %>% pull(y) %>% unlist
  corridor %<>% filter(y > left_wall & y < right_wall)
  res <- corridor %>% 
    filter(y == left_wall+1 | y == right_wall-1) %>% 
    bind_rows(
      corridor %>% left_join(cr,by=c("x","y")) %>% filter(!is.na(id_cr))
    ) %>% 
    mutate(l=dist+l,xa=xa,ya=ya) %>% 
    select(x,y,l,xa,ya)
  return(res)
}
move_v(dep$x,dep$y,0)

move_h <- function(xa,ya,l){
  corridor <- m %>% 
    filter(x != xa, y == ya) %>% 
    mutate(dist=abs(x-xa)+abs(y-ya)) %>% 
    arrange(dist) 
  left_wall <- corridor %>% filter(x<xa & t=="#") %>% filter(x==max(x)) %>% pull(x) %>% unlist
  right_wall <- corridor %>% filter(x>xa & t=="#") %>% filter(x==min(x)) %>% pull(x) %>% unlist
  corridor %<>% filter(x > left_wall & x < right_wall)
  res <- corridor %>% 
    filter(x == left_wall+1 | x == right_wall-1) %>% 
    bind_rows(
      corridor %>% left_join(cr,by=c("x","y")) %>% filter(!is.na(id_cr))
    ) %>% 
    mutate(l=dist+l,xa=xa,ya=ya) %>% 
    select(x,y,l,xa,ya)
  return(res)
}
move_h(dep$x,dep$y,0)

move <- function(x,y,l,dir){
  if(dir==1) res <- move_h(xa = x,ya = y,l)
  if(dir==0) res <- move_v(xa = x,ya = y,l)
  if(nrow(res)==0) return(tibble(x,y,l,xa=x,ya=y,rn=0))
  res %<>% mutate(rn=row_number())
  res %<>% mutate(dir=1-dir)
  return(res)
}

pos0 %<>% mutate(id_a=row_number())
# keeping ids of each move
hist2 <- tibble(i=numeric(),xa=numeric(),ya=numeric(),xb=numeric(),yb=numeric(),id_a=numeric(),id_b=numeric(),l=numeric())
hist2 %<>% bind_rows(tibble(i=0,xa=pos0$x,ya=pos0$y,xb=pos0$x,yb=pos0$y,id_b=0,id_a=0,l=pos0$l))

i <- 1
pos <- pos0
visited <- pos %>% select(x,y) %>% unique

while(pos %>% inner_join(goal,by=c("x","y")) %>% nrow == 0){
  old_pos <- pos %>% select(x=x,y=y,id_a) %>% mutate(i=i)
  pos %<>% group_by(x,y,dir,l) %>% count %>% ungroup %>% select(-n)
  pos %<>% mutate(prev=row_number())
  old_pos %<>%
    right_join(pos %>% select(x,y,prev),by = join_by(x, y),relationship = "many-to-many") 
  new_pos <- pmap_dfr(pos %>% select(-prev), move, .id="prev") %>% 
    filter(rn!=0) %>% 
    mutate(prev=as.numeric(prev),id_b=row_number())
  new_pos %<>% anti_join(visited,by=c("x","y")) %>% unique
  old_pos %<>% mutate(xa=x,ya=y) %>% 
    select(-x,-y) %>% 
    left_join(new_pos,by = join_by(prev, xa, ya),relationship = "many-to-many") %>% 
    rename(xb=x,yb=y) %>% 
    select(-rn,-dir,-prev)
  pos <- new_pos %>% select(x,y,l,dir,id_a=id_b) %>% mutate(l=1000+l)
  hist2 %<>% bind_rows(old_pos)
  visited %<>% bind_rows(pos %>% select(x,y)) %>% unique
  i <- i+1
  if(i %% 5==0) {print(i);print(nrow(pos));print(nrow(visited))} 
}
final <- pos %>% inner_join(goal,by=c("x","y")) %>% filter(l==min(l)) %>% unique
iMax <- max(hist2$i)
sols <- final$id_a

# tibble of all the tiles crossed in winning paths
seats <- goal
add_seats <- function(xa,ya,xb,yb){
  new_seats <- cross_join(tibble(x = xa:xb),tibble(y=ya:yb))  
  seats <<- seats %>% bind_rows(new_seats) %>% unique  
  return(TRUE)
}
for(j in iMax:1){
  h <- hist2 %>% filter(id_b %in% sols & i==j)
  h %<>%  group_by(xb,yb) %>% filter(l==min(l)) %>% ungroup
  sols <- h$id_a
  pmap(h %>% select(xa,ya,xb,yb),add_seats)
}
nrow(seats)
# 481

##########################################################
# Graphics
n <- seats %>% mutate(t="O") %>% bind_rows(m %>% anti_join(seats)) 
n %>% group_by(t) %>% count
n %>% drawMaze(switch = TRUE)


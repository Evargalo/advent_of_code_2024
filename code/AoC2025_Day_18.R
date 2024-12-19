# Aoc 2024 Day 18

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D18.txt",sep = ",",header = FALSE)
names(dat) <- c("x","y")
da <- dat %>% filter(row_number()<=1024)
m <- expand_grid(x=0:70,y=0:70,t=".")
da %<>% mutate(t="#")
m <- bind_rows(da,m) %>% group_by(x,y) %>% filter(row_number()==1) %>% ungroup

##########################################################
# A

drawMaze(m)
nm <- fill_de(m)
drawMaze(nm)
origin <- (tibble(x=0,y=0,t="."))
dest <- (tibble(x=70,y=70,t="."))

distanceInMaze <- function(m, origin, walls="#", ways=".",dmax=145){  
  endPointChar <- "£"
  ways <- c(ways, endPointChar)
  newSmallMaze <- m %>% mutate(t=ifelse(x==origin$x & y==origin$y, 
                                        endPointChar, t))
  newSmallMaze %>% filter(t %in% ways) -> canPass
  current_step <- 0
  canPass %>% mutate(dist=ifelse(test = (t==endPointChar), current_step, -1)) -> canPass
  canPass %>% group_by(dist) %>% count
  newCalc <- canPass %>% filter(dist==current_step) %>% mutate(xx=x, yy=y)
  while(nrow(newCalc)>0 & current_step < dmax){
    print(current_step)
    current_step <- current_step+1
    pmap_dfr(.l=newCalc %>% select(xx, yy), .f=find4Neighbours, spots=(canPass %>% filter(dist==-1))) -> newCalc
    newCalc %>% mutate(dist=current_step) -> newCalc
    canPass %>% anti_join(newCalc, by=c("x", "y")) %>% bind_rows(newCalc) %>% unique ->canPass
    newCalc %<>% mutate(xx=x, yy=y)
  }
  return(canPass)
}

drawMaze(nm)
# from start
test <- distanceInMaze(nm,origin,dmax=145)
# from end
test2 <- distanceInMaze(nm,dest,dmax=145)
gf_tile(test,gformula = y~x,fill = ~dist)
gf_tile(test2,gformula = y~x,fill = ~dist)
tt <- bind_rows(test %>% filter(dist>-1),test2 %>% filter(dist>-1),nm %>% filter(t=="#") %>% mutate(dist=-20)) 
gf_tile(tt,gformula = y~x,fill = ~dist)

tab <- test %>% filter(dist==145) %>% rename(xx=x,yy=y) %>% 
  full_join(test2 %>% filter(dist==145))
tab %>% mutate(dist=290+abs(xx-x)+abs(yy-y)) %>% summarise(min(dist))

# 318

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
m <- fall(2024)
drawMaze(m) # we can pass
m <- fall(2500)
nm <- fill_de(m)
drawMaze(nm) # we can pass, only one way
test <- distanceInMaze(nm,origin,dmax=600)
gf_tile(test,gformula = y~x,fill = ~dist)

test %>% filter(x==70,y==70)
path <- test %>% filter(dist>-1 & dist <453)
gf_tile(path,gformula = y~x,fill = ~dist)

dat %<>% mutate(id=row_number())
bombs <- dat %>% inner_join(path %>% select(x,y))
b <- bombs %>% head(19) 
path %>% anti_join(b %>% select(x,y)) %>% mutate(safe=0) %>% bind_rows(b %>% mutate(safe=id)) %>% 
  gf_tile(gformula = y~x,fill = ~safe)
b %>% tail(1)
# 56,29

# Graphics
path %>% anti_join(b %>% select(x,y)) %>% mutate(bombe="0") %>% bind_rows(b %>% mutate(bombe="1")) %>% 
  gf_tile(gformula = (-x)~y,fill = ~bombe)


##############################################################
# B differently, doesn't need to draw maps to access visually

# walls on either side. If they intersect we cannot pass
SW <- tibble(x=-1,y=0:70) %>% bind_rows(tibble(x=0:70,y=71))
NE <- tibble(x=71,y=0:70) %>% bind_rows(tibble(x=0:70,y=-1))
# byte fallen taht are not connected to a wall
unconnected <- tibble(x=integer(),y=integer())

# Try to connect xx,yy to a wall
include <- function(xx,yy,tab_name){
  tab <- get(tab_name)
  if(any(abs(tab$x-xx)<=1 & abs(tab$y-yy)<=1)){
    assign(tab_name,tab %>% add_row(x=xx,y=yy),envir = .GlobalEnv)
    assign("unconnected",unconnected %>% filter(x!=xx | y!=yy),envir = .GlobalEnv)
    to_add <- unconnected %>% filter(abs(x-xx)<=1 & abs(y-yy)<=1)
    map2(.x = to_add$x,.y=to_add$y,function(x,y)include(x,y,tab_name))
    return(TRUE)
  }
  return(FALSE)
}

# act for each falling byte
assign_drop <- function(x,y){
  lNE <- include(x,y,"NE")
  lSW <- include(x,y,"SW")
  if(lNE | lSW) if(inner_join(NE,SW,by = join_by(x, y)) %>% nrow > 0) return(TRUE)
  if(!lNE & !lSW) assign("unconnected",unconnected %>% add_row(x=x,y=y),envir = .GlobalEnv)
  return(FALSE)
}

for(i in 1:3000){
  if(i %% 100==0) print(i)
  if(assign_drop(dat$x[i],dat$y[i])) {print(i);break}
}
dat[i,]
# 56,29

gf_tile(NE,(-y)~x)
gf_tile(SW,(-y)~x)
gf_tile(unconnected,(-y)~x)

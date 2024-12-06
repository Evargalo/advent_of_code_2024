# Aoc 2024 Day 6

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D6.txt",sep = ",",header = FALSE)

# dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D6_test.txt",sep = ",",header = FALSE)

# A

maze <- buildMaze(dat$V1)

m <- maze$Maze 
m %>% drawMaze

yDir <- c(0,1,0,-1)
xDir <- c(-1,0,1,0)


dirs <- 1:4
dir <- dirs[1]
out <- FALSE

xMax <- max(m$x)
yMax <- max(m$y)

currentpos <- m %>% filter(t=="^") %>% mutate(dir=dir)
xx <- currentpos$x
yy <- currentpos$y

while(!out){
  #  print(xx);print(yy)
  m %<>% mutate(t=if_else(x==xx & y==yy,"X",t))
  xx <- xx + xDir[dir]
  yy <- yy + yDir[dir]
  if(xx==0 | xx>xMax | yy==0 | yy>yMax){
    out <- TRUE
    next()
  }
  tt <- m %>% filter(x==xx,y==yy) %>% pull(t) %>% unlist
  if(tt=="#"){
    xx <- xx - xDir[dir]
    yy <- yy - yDir[dir]
    dir <- (dir %% 4) + 1
  }
}

sum(m$t=="X")
m %>% drawMaze

# 4973


# B

m <- maze$Maze 
m %>% drawMaze

out <- FALSE

history <- currentpos
history %<>% mutate(dir=1) 

xx <- currentpos$x
yy <- currentpos$y
dir <- dirs[1]

while(!out){
  #  print(xx);print(yy)
  m %<>% mutate(t=if_else(x==xx & y==yy,"X",t))
  history %<>% add_row(x=xx,y=yy,t="X",dir=dir)
  xx <- xx + xDir[dir]
  yy <- yy + yDir[dir]
  if(xx==0 | xx>xMax | yy==0 | yy>yMax){
    out <- TRUE
    next()
  }
  tt <- m %>% filter(x==xx,y==yy) %>% pull(t) %>% unlist
  if(tt=="#"){
    xx <- xx - xDir[dir]
    yy <- yy - yDir[dir]
    dir <- (dir %% 4) + 1
  }
}

history %<>% 
  mutate(rank=row_number())

mm <- maze$Maze 

eval_change <- function(xx,yy){
  
  m <- mm
  if(m %>% filter(x==xx,y==yy) %>% pull(t) != ".") return(FALSE)
  m %<>% mutate(t=if_else(x==xx & y==yy,"#",t))
  currentpos <- m %>% filter(t=="^")
  dir <- dirs[1]
  out <- FALSE
  xx <- currentpos$x
  yy <- currentpos$y
  loop <- FALSE
  short_history <- tibble(x=integer(),y=integer(),t=character(),dir=integer())
  
  while(!out & !loop){
    m %<>% mutate(t=if_else(x==xx & y==yy,"X",t))
    ddir <- dir
    if(nrow(short_history %>% filter(x==xx,y==yy,dir==ddir)) >0 ){
      loop <- TRUE
      next()
    }
    short_history %<>% add_row(x=xx,y=yy,t="X",dir=ddir)
    xx <- xx + xDir[dir]
    yy <- yy + yDir[dir]
    if(xx==0 | xx>xMax | yy==0 | yy>yMax){
      out <- TRUE
      next()
    }
    tt <- m %>% filter(x==xx,y==yy) %>% pull(t) %>% unlist
    if(tt=="#"){
      xx <- xx - xDir[dir]
      yy <- yy - yDir[dir]
      dir <- (dir %% 4) + 1
    }
  }
  return(loop)
}

eval_change(6,5)

res <- tibble(x=integer(),y=integer())
res %<>% mutate(change=TRUE) 
for(i in 1:(nrow(history)-1)){
  # for(i in 186:(nrow(history)-1)){
  if(i%%10==0) print(i)
  ll <- history %>% filter(rank==i)
  xx <- ll$x + xDir[ll$dir]
  yy <- ll$y + yDir[ll$dir]
  if(nrow(res %>% filter(x==xx,y==yy))>0) next()
  if(eval_change(xx,yy)) {
    res %<>% add_row(x=xx,y=yy,change=TRUE)
  } else{
    res %<>% add_row(x=xx,y=yy,change=FALSE)
  }
}
res %>% group_by(change) %>% count 

# 4971 too high

m<-mm %>% filter(t=="#")

next_step <- function(.x,.y,.dir,m=mm){
  m <- m %>% filter(x==.x | y==.y)
  if(.dir==1) res <- m %>% filter(x<.x & t=="#") %>% filter(x==max(x)) %>% mutate(x=x+1, l=.x-x)
  if(.dir==3) res <- m %>% filter(x>.x & t=="#") %>% filter(x==min(x)) %>% mutate(x=x-1, l=x-.x)
  if(.dir==2) res <- m %>% filter(y>.y & t=="#") %>% filter(y==min(y)) %>% mutate(y=y-1, l=y-.y)
  if(.dir==4) res <- m %>% filter(y<.y & t=="#") %>% filter(y==max(y)) %>% mutate(y=y+1, l=.y-y)
  return(res %>% mutate(dir=(.dir%%4)+1))
}

# A
cur <- currentpos %>% select(-t) %>% mutate(l=0)
road <- cur
while(nrow(cur)>0){
  cur <- next_step(cur$x,cur$y,cur$dir) %>% select(-t)
  road %<>% bind_rows(cur) 
}
road %>% summarise(sum(l))

# B
eval_change <- function(xx,yy){
  m <- mm
  if(m %>% filter(x==xx,y==yy) %>% pull(t) != ".") return(FALSE)
  m %<>% mutate(t=if_else(x==xx & y==yy,"#",t))
  currentpos <- m %>% filter(t=="^")
  dir <- dirs[1]
  out <- FALSE
  cur <- currentpos %>% select(-t) %>% mutate(l=0,dir=dir)
  loop <- FALSE
  short_history <- tibble(x=integer(),y=integer(),dir=integer(),l=integer())
  
  while(!out & !loop){
    xx <- cur$x ; yy <- cur$y; ddir <- cur$dir
    if(nrow(short_history %>% filter(x==xx,y==yy,dir==ddir)) >0 ){
      loop <- TRUE
      next()
    }
    short_history %<>% bind_rows(cur)
    cur <- next_step(cur$x,cur$y,cur$dir,m %>% filter(t=="#")
) %>% select(-t)
    if(nrow(cur)==0){
      out <- TRUE
      next()
    }
  }
  return(loop)
}

res <- tibble(x=integer(),y=integer())
res %<>% mutate(change=TRUE) 
for(i in 1:(nrow(history)-1)){
 # for(i in 1590:(nrow(history)-1)){
  if(i%%10==0) print(i)
  ll <- history %>% filter(rank==i)
  xx <- ll$x + xDir[ll$dir]
  yy <- ll$y + yDir[ll$dir]
  if(nrow(res %>% filter(x==xx,y==yy))>0) next()
  if(eval_change(xx,yy)) {
    res %<>% add_row(x=xx,y=yy,change=TRUE)
  } else{
    res %<>% add_row(x=xx,y=yy,change=FALSE)
  }
}
res %>% group_by(change) %>% count 



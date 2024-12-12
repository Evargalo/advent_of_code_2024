# Aoc 2024 Day 12

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D12.txt")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D12.txt",sep = " ",header = FALSE)

maze <- buildMaze(dat$V1)
m <- maze$Maze
drawMaze(m)
max(m$x)
max(m$y)

m$reg <- 0

# A

m$reg[1] <- 1


# Identify the regions

# nb_done <- 0
attrib_reg <- function(x,y,t,reg){
  # nb_done <<- nb_done+1
  # if(nb_done %% 100 ==0) print(nb_done)
  if(reg != 0) return(FALSE)
  xx <- x ; yy <- y ;  tt <- t
  last_reg <- max(m$reg)
  regs_neighbours <- find4Neighbours(xx,yy,m) %>% filter(t==tt & reg!=0) %>% pull(reg) %>% unique
  if(length(regs_neighbours)==0){
    m <<- m %>% mutate(reg=if_else(x==xx & y==yy,last_reg+1,reg))
    return(TRUE)
  }
  if(length(regs_neighbours)==1){
    m <<- m %>% mutate(reg=if_else(x==xx & y==yy,regs_neighbours,reg))
    return(TRUE)
  }
  if(length(regs_neighbours)>1){
    m <<- m %>% 
      mutate(reg=if_else(x==xx & y==yy,regs_neighbours[1],reg)) %>% 
      mutate(reg=if_else(reg %in% regs_neighbours,regs_neighbours[1],reg))
    return(TRUE)
  }
}
pmap(.l = m,.f = attrib_reg)

# tibble of regions
areas <- table(m$reg) 
res <- tibble(reg=names(areas),area=areas %>% as.integer(),perim=0)

# perimeter of regions
calc_perim <- function(reg){
  print(reg)
  rreg <- reg
  my_reg <- m %>% filter(reg==rreg)
  s <- pmap_int(my_reg,function(x,y,t,reg){
    find4Neighbours(x,y,m) %>% filter(reg==rreg) %>% nrow
  }) %>% sum %>% unlist
  res <<- res %>% mutate(perim=if_else(reg==rreg,4*area-s,perim))
  return(TRUE)
}
map(res$reg,calc_perim)
res %>% summarise(sum(area*perim))

# 1363682

# B

res$sides <- 0

# add a fictitious region around the map
m %<>% bind_rows(
  tibble(x=0,y=0:141,t=".",reg=-1),
  tibble(x=141,y=0:141,t=".",reg=-1),
  tibble(y=0,x=1:140,t=".",reg=-1),
  tibble(y=141,x=1:140,t=".",reg=-1),
)

rreg <- 1
my_reg <- m %>% filter(reg==rreg)

# borders between a given region (inner bloc xi,yi) and an outer one (xo,yo)
crossings <- pmap_dfr(my_reg,function(x,y,t,reg){
  bind_rows(find4Neighbours(x,y,m) %>% filter(reg!=rreg) %>% select(xo=x,yo=y) %>% mutate(xi=x,yi=y,num=0))
})
crossings$num[1]<-1
# are two borders adjacent and part of a same side ?
same_crossing <- function(xo,yo,xi,yi,mxo,myo,mxi,myi){
  (abs(xo-mxo)+abs(yo-myo)==1) & (abs(xi-mxi)+abs(yi-myi)==1)
}
# identify the sides of each border
num_cross <- function(i){
  last_num <- max(crossings$num)
  mxo <- crossings[i,1] 
  myo <- crossings[i,2] 
  mxi <- crossings[i,3] 
  myi <- crossings[i,4] 
  prev <- crossings %>% filter(num!=0 & same_crossing(xo,yo,xi,yi,mxo,myo,mxi,myi))
  if(nrow(prev)==0) crossings$num[i] <<- last_num+1
  if(nrow(prev)==1) crossings$num[i] <<- prev$num
  if(nrow(prev)>1) {
    crossings$num[i] <<- prev$num[1]
    crossings <<- crossings %>% mutate(num=if_else(num %in% prev$num,prev$num[1],num))
  }
  return(TRUE)
}
map(1:nrow(crossings),.f = num_cross)
crossings

# number of sides of a region
calc_sides <- function(reg){
  print("reg="); print(reg)
  rreg <- reg
  my_reg <- m %>% filter(reg==rreg)
  crossings <<- pmap_dfr(my_reg,function(x,y,t,reg){
    bind_rows(find4Neighbours(x,y,m) %>% filter(reg!=rreg) %>% select(xo=x,yo=y) %>% mutate(xi=x,yi=y,num=0))
  })
  crossings$num[1]<<-1
  map(1:nrow(crossings),.f = num_cross)
  return(n_distinct(crossings$num))
}

calc_sides(835)

res %<>% rowwise %>% mutate(sides=calc_sides(reg)) %>% ungroup
res %>% summarise(sum(area*sides))
# 787680

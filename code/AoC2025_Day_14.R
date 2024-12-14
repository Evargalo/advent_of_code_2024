# Aoc 2024 Day 14

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D14.txt",sep = " ",header = FALSE)
dat %<>% mutate(
  V1=cutBefore(V1,"="),
  V2=cutBefore(V2,"="),
  x=cutAfter(V1,",") %>% as.integer,
  y=cutBefore(V1,",") %>% as.integer,
  vx=cutAfter(V2,",") %>% as.integer,
  vy=cutBefore(V2,",") %>% as.integer,
) %>% 
  select(-V1,-V2)
ddat <- dat
xMax <- 101
yMax <- 103
mx<-(xMax-1)/2
my<-(yMax-1)/2
# A
find_q <- function(x,y){
  case_when(
    x==mx | y==my ~ 0,
    x<mx & y<my ~ 1,
    x<mx & y>my ~ 2,
    x>mx & y<my ~ 3,
    x>mx & y>=my ~ 4,
    TRUE ~ 0
  )
}
dat %<>% mutate(
  x100=(x+100*vx) %% xMax,
  y100=(y+100*vy) %% yMax
) %>% 
  mutate(q100=find_q(x100,y100))

dat %>% group_by(q100) %>% count %>% ungroup %>% filter(q100!=0) %>% summarise(prod(n))
# 230172768

# B
dat <- ddat
res <- tibble(i=numeric(),vx=numeric(),vy=numeric())
for(i in 1:1000){
  dat %<>% mutate(
  x=(x+vx)%%xMax,
  y=(y+vy)%%yMax
  )
  res <<- res %>% add_row(i=i,vx=var(dat$x),vy=var(dat$y))
}
res %>% filter(vx==min(vx) | vy==min(vy))
# x : 7+101p
# y : 53+103q
# too lazy for chinese rests, let's bruteforce it :
all_x <- 7+101*(1:103)
all_y <- 53+103*(1:101)
all_x[all_x %in% all_y]
# 8087

# To show the Xmas tree:
dat <- ddat
dat %<>% mutate(
    x8087=(x+8087*vx) %% xMax,
    y8087=(y+8087*vy) %% yMax
  )
dat %>% gf_tile((-y8087)~x8087,col="darkgreen")

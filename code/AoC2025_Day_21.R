# Aoc 2024 Day 21

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data
dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D21.txt")
# numeric keyboard
doorKB <- tibble(x=3,y=0,t='A')
doorKB %<>% bind_rows(tibble(x=2,y=0,t='0'))
doorKB %<>% bind_rows(tibble(x=1:3,y=1,t=as.character(1:3)))
doorKB %<>% bind_rows(tibble(x=1:3,y=2,t=as.character(4:6)))
doorKB %<>% bind_rows(tibble(x=1:3,y=3,t=as.character(7:9)))
# directional keyboard
dirKB <- tibble(x=3,y=1,t='A')
dirKB %<>% bind_rows(tibble(x=2,y=1,t='^'))
dirKB %<>% bind_rows(tibble(x=1:3,y=0,t=c('<','v','>')))

##########################################################
# A

# what directional command can make the robot move from any key to any other one on keyboard KB
build_tab_trans <- function(KB){
  paths <- KB %>% cross_join(KB %>% rename(xx=x,yy=y,tt=t)) %>% 
    mutate(dist=abs(xx-x)+abs(yy-y)) %>% 
    arrange(dist)
  # if you wanna press the same key stay on place
  p0 <- paths %>% filter(dist==0) %>% 
    mutate(p="") %>% 
    select(ta=t,tb=tt,p)
  # if the key is adjacent, make just one move
  p1 <- paths %>% filter(dist==1) %>% 
    mutate(p=case_when(
    y==yy & x==xx+1 ~"<",
    y==yy & x==xx-1 ~">",
    y==yy+1 & x==xx ~"v",
    y==yy-1 & x==xx ~"^",
    TRUE ~ ""
  )) %>% 
    select(t1a=t,t1b=tt,p)
  pp <- p1 %>% rename(ta=t1a,tb=t1b)
  res <- bind_rows(p0,pp)
  # if further away, make any move and then you're back in the previous case
  for(i in 2:max(paths$dist)){
    p2 <- paths %>% filter(dist==i) %>% 
      select(t2a=t,t2b=tt) %>% 
      left_join(p1,by=c("t2a"="t1a"),relationship = "many-to-many") %>% 
      select(t2a,t2b,t_int=t1b,p1=p) %>% 
      left_join(pp,by=c("t_int"="ta"),relationship = "many-to-many") %>% 
      filter(tb==t2b) %>% 
      mutate(p=paste0(p1,p)) %>% 
      select(t2a,t2b,p)
    pp <- p2 %>% rename(ta=t2a,tb=t2b)
    res %<>% bind_rows(pp)
  }
  res
}
# our two kinds of keyboards
dir_trans <- build_tab_trans(dirKB)
door_trans <- build_tab_trans(doorKB)

# moves necessary to dial a string s
s<-"<>>A"
build_input <- function(s){
  v1 <- s %>% str_split_1("")
  tibble(key=v1,prev=lag(v1,default = "A"))
}
build_input(s)

# We call cost the number of human presses needed for the initial command
# cost of moving on the first robot keyboard
cost3 <- function(key,prev){
  tib <- tibble(key,prev) %>% 
    left_join(dir_trans %>% rename(prev=ta,key=tb),by = join_by(key, prev)) %>%
    mutate(cost=nchar(p)+1) 
  min(tib$cost)
}
cost3(key,prev)
cost3_s <- function(s){
  input <- build_input(s)
  pmap_int(input,cost3) %>% sum
}
cost3_s(s)
# cost of moving on the second robot keyboard
cost2 <- function(key,prev){
  tib <- tibble(key,prev) %>% 
    left_join(dir_trans %>% rename(prev=ta,key=tb),by = join_by(key, prev)) %>%
    rowwise %>% mutate(cost=cost3_s(p %>% paste0("A"))) %>% ungroup
  min(tib$cost)
}
cost2(key,prev)
cost2_s <- function(s){
  input <- build_input(s)
  pmap_int(input,cost2) %>% sum
}
cost2_s(s)
# cost of moving on the numeric keyboard
key <- "2" ; prev <- "0"
cost0 <- function(key,prev){
  tib <- tibble(key,prev) %>% 
    left_join(door_trans %>% rename(prev=ta,key=tb),by = join_by(key, prev)) %>%
    rowwise %>% mutate(cost=cost2_s(p %>% paste0("A"))) %>% ungroup
  min(tib$cost)
}
cost0("9","0")
cost0_s <- function(s){
  input <- build_input(s)
  pmap_int(input,cost0) %>% sum
}
cost0_s("029A") 

res <- tibble(s=dat,nb=as.numeric(substr(dat,1,3))) %>% 
  rowwise %>% mutate(val=cost0_s(s)) %>% 
  ungroup 
res %>% summarise(sum(nb*val))
# 184180

##########################################################
# B

# 1 human directional keypad
cost <- expand.grid(key=dirKB$t,prev=dirKB$t)
cost %<>% mutate(robot=0,c=1)

# 25 robot directional keypads
p <- "<><" ; rob <- 1
cost_s <- function(p,rob){
  p %<>% paste0("A")
  input <- build_input(p)
  input %<>% left_join(cost %<>% filter(robot==rob-1),by = join_by(key, prev)) 
  sum(input$c)  
}
for(k in 1:25){
  print(k)
  new_cost <- expand.grid(key=dirKB$t,prev=dirKB$t) %>% 
    left_join(dir_trans %>% rename(prev=ta,key=tb),by = join_by(key, prev)) %>%
    rowwise %>% mutate(cost=cost_s(p,k)) %>% ungroup %>% 
    group_by(prev,key) %>% 
    summarise(c=min(cost),.groups="drop") %>% 
    mutate(robot=k)
  cost %<>% bind_rows(new_cost)
}

# 1 numeric keypad
new_cost <- expand.grid(key=doorKB$t,prev=doorKB$t) %>% 
  left_join(door_trans %>% rename(prev=ta,key=tb),by = join_by(key, prev)) %>%
  rowwise %>% mutate(cost=cost_s(p,26)) %>% ungroup %>% 
  group_by(prev,key) %>% 
  summarise(c=min(cost),.groups="drop") %>% 
  mutate(robot=26)
cost %<>% bind_rows(new_cost)

# for a string input, read the costs of each move in tibble "cost"
cost0 <- function(kk,pp){
  cost %>% filter(key==kk,prev==pp,robot==26) %>% pull(c) %>% unlist
}
cost0_s <- function(s){
  input <- build_input(s) %>% rename(kk=key,pp=prev)
  pmap(input,cost0) %>% reduce(sum)
}

res <- tibble(s=dat,nb=as.numeric(substr(dat,1,3))) %>% 
  rowwise %>% mutate(val=cost0_s(s)) %>% 
  ungroup 

res %>% summarise(tot=sum(nb*val)) %>% pull(tot)
# 231309103124520
